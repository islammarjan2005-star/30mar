### WORKFORCE JOBS BY INDUSTRY BLOCK  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## WORKFORCE JOBS UI ## -----
employment_workforce_industry_stats_card_ui <- function(id) {
  ns <- NS(id)
  tags$script("$(function() {$('[data-toggle=\"tooltip\"]').tooltip();});")

  tagList(

    div(class = "govuk-grid-row",
      uiOutput(ns("card_all_jobs")),
      uiOutput(ns("card_total_services"))
    ),

    # --- Industry breakdown card  ---
    mod_govuk_data_vis_card_ui(
      id = ns("workforce_industry_card"),
      title = "Workforce Jobs by Industry",
      help_text = "Workforce jobs by SIC section",
      help_text_source = "Source: ONS - Labour market statistics, workforce jobs by industry",
      help_link = "https://data.trade.gov.uk/datasets/4609dc12-0dfa-4734-8ecb-6c50b59d163d",
      help_link_text = "ONS Labour Market Overview",
      table_content  = reactableOutput(ns("wf_table"),  height = "400px"),
      visual_content = plotlyOutput(ns("wf_plot"),       height = "480px"),
      query = ns("sql_query"),

      controls = list(
        mod_quick_date_range_ui(
          id            = ns("wf_dates"),
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

## WORKFORCE JOBS SERVER ## -----
employment_workforce_industry_stats_card_server <- function(id, conn = APP_DB$pool) {
  moduleServer(id, function(input, output, session) {

    AGG_CODES <- c("A-T", "G-T")

    shinyWidgets::updateRadioGroupButtons(session, "colour_by",  selected = "sic_section")
    shinyWidgets::updateRadioGroupButtons(session, "label_mode", selected = "both")

    # Base cleaned lazy tbl — cached (no filter inputs needed for the base)
    cleaned_full_tbl <- reactive({
      get_workforce_jobs_tbl()
    }) %>% bindCache("workforce_industry")   # static key: schema/table never changes

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
      id                 = "wf_dates",
      data_tbl           = cleaned_full_tbl,
      presets            = c("custom", "ytd", "past_year", "3y", "5y", "none"),
      default            = "5y",
      frequency          = "monthly",
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

    # Non-aggregate sections at the latest period, with SIC picker applied
    industry_df <- reactive({
      df <- snapshot_df()
      sel <- sic_pick$selected()
      df <- df %>% dplyr::filter(!sic_section %in% !!AGG_CODES)
      if (length(sel) > 0) df <- df %>% dplyr::filter(sic_section %in% !!sel)
      df %>% dplyr::arrange(dplyr::desc(value))
    })

    # ---- Outputs ----
    output$sql_query <- renderText({ dat()$sql })

    # Aggregate stat cards — All jobs and Total services
    output$card_all_jobs <- renderUI({
      agg <- aggregates_df()
      row <- agg %>% dplyr::filter(sic_section == "A-T") %>% dplyr::slice(1)
      req(nrow(row) == 1)
      snap_lbl <- format(row$time_period[[1]], "%b %Y")
      govuk_stats_card(
        id       = session$ns("all_jobs"),
        title    = "All Workforce Jobs (000s)",
        headline = row$value[[1]],
        delta    = NULL,
        period   = snap_lbl,
        good_if_increase = TRUE,
        format_headline  = govuk_format_number,
        show_info  = TRUE,
        info_text  = "Total workforce jobs across all industries (000s). Provisional values marked (p).",
        info_icon  = "i"
      )
    })

    output$card_total_services <- renderUI({
      agg <- aggregates_df()
      row <- agg %>% dplyr::filter(sic_section == "G-T") %>% dplyr::slice(1)
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
        info_text  = "Aggregate of all service industry sections (000s).",
        info_icon  = "i"
      )
    })

    output$wf_table <- reactable::renderReactable({
      df <- industry_df(); req(nrow(df) > 0)
      reactable::reactable(
        df %>% dplyr::select(industry, sic_section, time_period, value) %>%
               dplyr::mutate(value = round(value, 1)),
        sortable   = TRUE,
        filterable = TRUE,
        resizable  = TRUE,
        pagination = TRUE,
        highlight  = TRUE,
        striped    = TRUE,
        columns    = list(
          value = reactable::colDef(name = "Jobs (000s)")
        )
      )
    })

    output$wf_plot <- plotly::renderPlotly({
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
        labels     = ~industry,
        parents    = ~"",
        values     = ~value,
        textinfo   = textinfo_val,
        hovertemplate = paste0(
          "<b>%{label}</b><br>",
          "SIC section: ", df$sic_section, "<br>",
          "Jobs (000s): %{value:,.1f}<br>",
          "Share: %{percentParent:.1%}<extra></extra>"
        ),
        marker     = marker_cfg,
        textfont   = list(family = "GDS Transport, Arial, sans-serif", size = 11)
      ) %>%
        plotly::layout(
          title = list(
            text = paste0("<b>Workforce Jobs by Industry</b><br>",
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
            filename = paste0("workforce_jobs_", snap_lbl)
          )
        )
    })
  })
}

## WORKFORCE JOBS DATA ## ------

# Workforce jobs table: ons.labour_market__workforce_jobs
# time_period like "Jun 24 (p)" — strip "(p)" suffix then parse as Mon YY.
get_workforce_jobs_tbl <- function() {
  base <- dplyr::tbl(APP_DB$pool,
                     dbplyr::in_schema("ons", "labour_market__workforce_jobs"))

  period_sql <- dbplyr::sql("
    to_date(
      initcap(btrim(regexp_replace(time_period::text, '\\\\s*\\\\(p\\\\)\\\\s*$', '', 'i'))),
      'Mon YY'
    )::date
  ")

  base %>%
    dplyr::mutate(time_period = !!period_sql) %>%
    dplyr::select(
      time_period,
      industry,
      sic_section,
      value
    )
}
