### VACANCIES BY INDUSTRY BLOCK  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## VACANCIES BY INDUSTRY UI ## -----
vacancies_industry_stats_card_ui <- function(id) {
  ns <- NS(id)
  tags$script("$(function() {$('[data-toggle=\"tooltip\"]').tooltip();});")

  tagList(

    div(class = "govuk-grid-row",
      uiOutput(ns("card_all_vacancies")),
      uiOutput(ns("card_total_services"))
    ),

    # --- Industry breakdown card ---
    mod_govuk_data_vis_card_ui(
      id = ns("vacancies_industry_card"),
      title = "Vacancies by Industry",
      help_text = "Vacancies by SIC section",
      help_text_source = "Source: ONS - Labour market statistics, vacancies by industry",
      help_link = "https://data.trade.gov.uk/datasets/4609dc12-0dfa-4734-8ecb-6c50b59d163d",
      help_link_text = "ONS Labour Market Overview",
      table_content  = reactableOutput(ns("vac_table"),  height = "400px"),
      visual_content = plotlyOutput(ns("vac_plot"),       height = "480px"),
      query = ns("sql_query"),

      controls = list(
        shinyWidgets::radioGroupButtons(
          inputId = ns("chart_type"),
          label   = "Choose a graph :",
          choiceNames = list(
            tags$span(`data-toggle`="tooltip", title = "Treemap (snapshot)", tags$i(class = "fa fa-th-large")),
            tags$span(`data-toggle`="tooltip", title = "Stacked Bar Chart", tags$i(class = "fa fa-bar-chart")),
            tags$span(`data-toggle`="tooltip", title = "Line Chart",        tags$i(class = "fa fa-line-chart")),
            tags$span(`data-toggle`="tooltip", title = "Area Chart",        tags$i(class = "fa fa-area-chart"))
          ),
          choiceValues = c("treemap","stacked_bar","line","stacked_area"),
          justified = TRUE,
          size = "sm",
          status = "danger"
        ),
        mod_quick_date_range_ui(
          id            = ns("vac_dates"),
          label_quick   = "Date range",
          label_picker  = "Time period",
          custom_picker = "slider",
          presets       = c("custom", "ytd", "past_year", "3y", "5y", "none")
        ),
        mod_filter_picker_ui(
          id = ns("sic_filter"),
          label = "Industry section filter",
          multiple = TRUE,
          actions_box = TRUE,
          live_search = TRUE,
          virtual_scroll = 10,
          selected_text_format = "count > 2"
        )
      ),

      accordion_controls = list(
        conditionalPanel(
          condition = sprintf("input['%s'] == 'treemap'", ns("chart_type")),
          shinyWidgets::radioGroupButtons(
            inputId     = ns("colour_by"),
            label       = "Colour scheme",
            choices     = c("Industry section" = "sic_section", "Value (sequential)" = "value"),
            justified   = TRUE,
            size        = "sm",
            status      = "danger"
          ),
          shinyWidgets::radioGroupButtons(
            inputId      = ns("label_mode"),
            label        = "Block labels",
            choices      = c("Industry + value" = "both", "Industry only" = "label", "Value only" = "value"),
            justified    = TRUE,
            size         = "sm",
            status       = "danger"
          )
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'stacked_bar'", ns("chart_type")),
          shinyWidgets::sliderTextInput(
            inputId = ns("stack_mode"),
            label = "Time Interval between bars",
            choices = c("quarterly","annually","5year","decade"),
            selected = "annually",
            grid = TRUE
          )
        ),
        mod_annotation_line_ui(ns("date_lines"),
          type = "date", title = "Add key dates",
          add_label = "Add key date", show_delete = TRUE, auto_mask_date = TRUE
        ),
        mod_annotation_line_ui(ns("value_lines"),
          type = "value", title = "Add key values",
          add_label = "Add key values", show_delete = TRUE, auto_mask_date = TRUE
        )
      )
    )
  )
}

## VACANCIES BY INDUSTRY SERVER ## -----
vacancies_industry_stats_card_server <- function(id, conn = APP_DB$pool) {
  moduleServer(id, function(input, output, session) {

    AGG_CODES <- c("B-S", "G-S")

    shinyWidgets::updateRadioGroupButtons(session, "chart_type", selected = "treemap")
    shinyWidgets::updateRadioGroupButtons(session, "colour_by",  selected = "sic_section")
    shinyWidgets::updateRadioGroupButtons(session, "label_mode", selected = "both")
    date_lines  <- mod_annotation_line_server("date_lines",  type = "date")
    value_lines <- mod_annotation_line_server("value_lines", type = "value")

    # Base cleaned lazy tbl — cached (no filter inputs needed for the base)
    cleaned_full_tbl <- reactive({
      get_vacancies_industry_tbl()
    }) %>% bindCache("vacancies_industry")   # static key: schema/table never changes

    # Picker is fed only non-aggregate sections
    sic_picker_tbl <- reactive({
      cleaned_full_tbl() %>%
        dplyr::filter(!sic_section %in% !!AGG_CODES)
    })
    sic_pick <- mod_filter_picker_server(
      id       = "sic_filter",
      data_tbl = sic_picker_tbl,
      column   = "sic_section"
    )

    # Date range module
    dates <- mod_quick_date_range_server(
      id                 = "vac_dates",
      data_tbl           = cleaned_full_tbl,
      presets            = c("custom", "ytd", "past_year", "3y", "5y", "none"),
      default            = "5y",
      frequency          = "quarterly",
      custom_picker      = "slider",
      preserve_selection = TRUE
    )

    # Date-filtered collected data (still includes aggregates so stat cards work)
    dat <- reactive({
      dr        <- dates$date_range()
      date_from <- if (!is.null(dr) && length(dr) == 2) as.Date(dr[[1]]) else NULL
      date_to   <- if (!is.null(dr) && length(dr) == 2) as.Date(dr[[2]]) else NULL

      t <- cleaned_full_tbl() %>%
        apply_filters_general(
          date_col  = "time_period",
          date_from = date_from,
          date_to   = date_to
        )

      list(
        data = t %>% dplyr::collect(),
        sql  = sql_render_pool_safe(t)
      )
    })

    # Snapshot: latest time_period within filtered range (drives stat cards)
    snapshot_df <- reactive({
      df <- dat()$data
      req(nrow(df) > 0)
      latest_period <- max(df$time_period, na.rm = TRUE)
      df %>%
        dplyr::filter(time_period == latest_period) %>%
        dplyr::filter(!is.na(value), value > 0)
    })

    aggregates_df <- reactive({
      snapshot_df() %>%
        dplyr::filter(sic_section %in% !!AGG_CODES)
    })

    # Time series of non-aggregate sections, with optional SIC picker filter
    filtered_long_df <- reactive({
      df <- dat()$data
      req(nrow(df) > 0)
      sel <- sic_pick$selected()
      df <- df %>%
        dplyr::filter(!sic_section %in% !!AGG_CODES) %>%
        dplyr::filter(!is.na(value), value > 0)
      if (length(sel) > 0) df <- df %>% dplyr::filter(sic_section %in% !!sel)
      df
    })

    # Treemap snapshot (latest period of filtered_long_df)
    industry_df <- reactive({
      df <- filtered_long_df(); req(nrow(df) > 0)
      latest_period <- max(df$time_period, na.rm = TRUE)
      df %>%
        dplyr::filter(time_period == latest_period) %>%
        dplyr::arrange(dplyr::desc(value))
    })

    # ---- Outputs ----
    output$sql_query <- renderText({ dat()$sql })

    # Aggregate stat cards — All vacancies (B-S) and Total services (G-S)
    output$card_all_vacancies <- renderUI({
      agg <- aggregates_df()
      row <- agg %>% dplyr::filter(sic_section == "B-S") %>% dplyr::slice(1)
      req(nrow(row) == 1)
      snap_lbl <- format(row$time_period[[1]], "%b %Y")
      govuk_stats_card(
        id       = session$ns("all_vacancies"),
        title    = "All Vacancies (000s)",
        headline = row$value[[1]],
        delta    = NULL,
        period   = snap_lbl,
        good_if_increase = TRUE,
        format_headline  = govuk_format_number,
        show_info  = TRUE,
        info_text  = "Total vacancies across all industries (SIC B-S) (000s).",
        info_icon  = "i"
      )
    })

    output$card_total_services <- renderUI({
      agg <- aggregates_df()
      row <- agg %>% dplyr::filter(sic_section == "G-S") %>% dplyr::slice(1)
      req(nrow(row) == 1)
      snap_lbl <- format(row$time_period[[1]], "%b %Y")
      govuk_stats_card(
        id       = session$ns("total_services"),
        title    = paste0("Total Services (SIC ", row$sic_section[[1]], ") (000s)"),
        headline = row$value[[1]],
        delta    = NULL,
        period   = snap_lbl,
        good_if_increase = TRUE,
        format_headline  = govuk_format_number,
        show_info  = TRUE,
        info_text  = "Aggregate of all service industry sections (SIC G-S) (000s).",
        info_icon  = "i"
      )
    })

    output$vac_table <- reactable::renderReactable({
      df <- industry_df(); req(nrow(df) > 0)
      reactable::reactable(
        df %>% dplyr::select(business_metric, sic_section, time_period, value) %>%
               dplyr::mutate(value = round(value, 1)),
        sortable   = TRUE,
        filterable = TRUE,
        resizable  = TRUE,
        pagination = TRUE,
        highlight  = TRUE,
        striped    = TRUE,
        columns    = list(
          value = reactable::colDef(name = "Vacancies (000s)")
        )
      )
    })

    output$vac_plot <- plotly::renderPlotly({
      ct <- input$chart_type %||% "treemap"

      if (ct == "treemap") {
        df       <- industry_df(); req(nrow(df) > 0)
        snap_lbl <- format(max(df$time_period, na.rm = TRUE), "%b %Y")

        textinfo_map <- c(
          both  = "label+value+percent parent",
          label = "label",
          value = "value+percent parent"
        )
        textinfo_val <- textinfo_map[[ input$label_mode %||% "both" ]]

        colour_by <- input$colour_by %||% "sic_section"

        if (colour_by == "sic_section") {
          sic_levels <- unique(df$sic_section)
          pal        <- rep(dbt_palettes$dbt$extended,
                            length.out = length(sic_levels))
          colour_vec <- pal[ match(df$sic_section, sic_levels) ]

          marker_cfg <- list(
            colors = colour_vec,
            line   = list(width = 1.5, color = "#ffffff")
          )
        } else {
          v_norm <- (df$value - min(df$value)) / (max(df$value) - min(df$value) + 1e-9)
          seq_pal <- grDevices::colorRampPalette(
            c("#b0c9e1", "#4174a3", "#00285f")
          )(100)
          colour_vec <- seq_pal[ pmax(1L, ceiling(v_norm * 100)) ]

          marker_cfg <- list(
            colors = colour_vec,
            line   = list(width = 1.5, color = "#ffffff")
          )
        }

        plotly::plot_ly(
          data       = df,
          type       = "treemap",
          labels     = ~business_metric,
          parents    = ~"",
          values     = ~value,
          textinfo   = textinfo_val,
          hovertemplate = paste0(
            "<b>%{label}</b><br>",
            "SIC section: ", df$sic_section, "<br>",
            "Vacancies (000s): %{value:,.1f}<br>",
            "Share: %{percentParent:.1%}<extra></extra>"
          ),
          marker     = marker_cfg,
          textfont   = list(family = "GDS Transport, Arial, sans-serif", size = 11)
        ) %>%
          plotly::layout(
            title = list(
              text = paste0("<b>Vacancies by Industry</b><br>",
                            "<sup>Snapshot: ", snap_lbl, " \u2014 values in 000s</sup>"),
              font = list(family = "GDS Transport, Arial, sans-serif",
                          size = 14, color = "#0b0c0c"),
              x = 0, xanchor = "left"
            ),
            margin    = list(t = 60, l = 0, r = 0, b = 0),
            paper_bgcolor = "#ffffff",
            font      = list(family = "GDS Transport, Arial, sans-serif")
          ) %>%
          plotly::config(
            displayModeBar = TRUE,
            modeBarButtonsToRemove = c("lasso2d", "select2d"),
            toImageButtonOptions  = list(
              format   = "svg",
              filename = paste0("vacancies_by_industry_", snap_lbl)
            )
          )
      } else {
        df <- filtered_long_df(); req(nrow(df) > 0)
        dbt_ts_plot(
          df          = df,
          chart_type  = ct,
          bar_interval = input$stack_mode %||% "annually",
          bar_agg     = "last",
          x_title     = "Time period",
          y_title     = "Vacancies (000s)",
          palette     = dbt_palettes$gaf,
          initial_legend_mode = "hidden",
          group_col   = "sic_section",
          time_col    = "time_period",
          value_col   = "value",
          vlines = date_lines$values_out(), vline_labels = date_lines$labels_out(),
          hlines = value_lines$values_out(), hline_labels = value_lines$labels_out()
        )
      }
    })
  })
}

## VACANCIES INDUSTRY DATA ## ------

# Vacancies by industry table: ons.labour_market__vacancies_industry
get_vacancies_industry_tbl <- function() {
  base <- dplyr::tbl(APP_DB$pool,
                     dbplyr::in_schema("ons", "labour_market__vacancies_industry"))

  period_sql <- .date_sql_for("MMM-MMM YYYY", "time_period")

  base %>%
    dplyr::mutate(time_period = !!period_sql) %>%
    dplyr::select(
      time_period,
      business_metric,
      sic_section,
      dataset_identifier_code,
      value
    )
}
