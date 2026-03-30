### VACANCIES BY INDUSTRY BLOCK  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## VACANCIES BY INDUSTRY UI ## -----
vacancies_industry_ui <- function(id) {
  ns <- NS(id)
  tags$script("$(function() {$('[data-toggle=\"tooltip\"]').tooltip();});")

  tagList(

    div(class = "govuk-grid-row",
      uiOutput(ns("card_all_vacancies")),
      uiOutput(ns("card_total_services"))
    ),

    # --- Treemap: individual industry breakdown  ---
    mod_govuk_data_vis_card_ui(
      id = ns("vacancies_industry_card"),
      title = "Vacancies by Industry",
      help_text = "Vacancies by individual industry section (SIC 2007). Aggregate totals (All vacancies B-S, Total services G-S) are shown above. Each block is sized by number of vacancies at the latest available snapshot.",
      help_text_source = "Source: ONS - Labour market statistics, vacancies by industry",
      help_link = "https://data.trade.gov.uk/datasets/4609dc12-0dfa-4734-8ecb-6c50b59d163d",
      help_link_text = "ONS Labour Market Overview",
      table_content  = reactableOutput(ns("vac_table"),  height = "400px"),
      visual_content = plotlyOutput(ns("vac_plot"),       height = "480px"),
      query = ns("sql_query"),

      controls = list(
        mod_quick_date_range_ui(
          id            = ns("vac_dates"),
          label_quick   = "Snapshot period",
          label_picker  = "Time period",
          custom_picker = "slider",
          presets       = c("custom", "latest", "past_year", "3y", "5y", "none")
        )
      ),

      accordion_controls = list(
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
      )
    )
  )
}

## VACANCIES BY INDUSTRY SERVER ## -----
vacancies_industry_server <- function(id, conn = APP_DB$pool) {
  moduleServer(id, function(input, output, session) {

    shinyWidgets::updateRadioGroupButtons(session, "colour_by",  selected = "sic_section")
    shinyWidgets::updateRadioGroupButtons(session, "label_mode", selected = "both")

    # Base cleaned lazy tbl — cached (no filter inputs needed for the base)
    cleaned_full_tbl <- reactive({
      get_vacancies_industry_tbl()
    }) %>% bindCache("vacancies_industry")   # static key: schema/table never changes

    # Date range module (drives the snapshot picker)
    dates <- mod_quick_date_range_server(
      id                 = "vac_dates",
      data_tbl           = cleaned_full_tbl,
      presets            = c("custom", "latest", "past_year", "3y", "5y", "none"),
      default            = "latest",
      frequency          = "monthly",
      custom_picker      = "slider",
      preserve_selection = TRUE
    )

    # Filtered data — collect once, then we take the latest period within range for the chart
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

    # Snapshot: always the LATEST time_period within the filtered range
    snapshot_df <- reactive({
      df <- dat()$data
      req(nrow(df) > 0)
      latest_period <- max(df$time_period, na.rm = TRUE)
      df %>%
        dplyr::filter(time_period == latest_period) %>%
        dplyr::filter(!is.na(value), value > 0)
    })

    # B-S = All vacancies, G-S = Total services
    aggregates_df <- reactive({
      snapshot_df() %>%
        dplyr::filter(sic_section %in% c("B-S", "G-S"))
    })

    industry_df <- reactive({
      snapshot_df() %>%
        dplyr::filter(!sic_section %in% c("B-S", "G-S")) %>%
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
        info_text  = "Aggregate of all service industry sections (SIC G-S) (000s). Displayed separately as it overlaps with individual section breakdowns below.",
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
      df       <- industry_df(); req(nrow(df) > 0)
      snap_lbl <- format(max(df$time_period, na.rm = TRUE), "%b %Y")

      # Build text info for each block label mode
      textinfo_map <- c(
        both  = "label+value+percent parent",
        label = "label",
        value = "value+percent parent"
      )
      textinfo_val <- textinfo_map[[ input$label_mode %||% "both" ]]

      # --- Colour by SIC section (categorical) or by value (sequential) ---
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
