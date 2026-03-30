### AWE STATS CARD ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## AWE UI ## -----
employment_awe_stats_card_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # ---- Filters Row ----
    #tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "equal-height-buttons.css")),

    # ---- Your existing card ----
    mod_govuk_data_vis_card_ui(
      id = ns("awe_trend_card"),
      title = "Average Weekly Earnings",
      help_text = "Seasonally Adjusted Nominal Average Weekly Earnings in Great Britian",
      help_text_source = "Source: ONS - A01 Labour market statistics summary data tables 13 & 15",
      help_link = "https://data.trade.gov.uk/datasets/4609dc12-0dfa-4734-8ecb-6c50b59d163d", 
      help_link_text = "ONS Labour Market Overview",
      table_content = reactableOutput(ns("table"), height = "350px"),
      visual_content = plotlyOutput(ns("plot"), height = "350px"), 
      query = ns("sql_query"), 
      controls =  list(
                      shinyWidgets::radioGroupButtons(
                       inputId = ns("which_measure"),
                       label = "Measure",
                       choices = c("Weekly Earnings", 
                        "Annual % change - Single Month","Annual % change - 3 month average"),
                       justified = TRUE,
                       status = "danger"
                    ),
                    shinyWidgets::radioGroupButtons(
                       inputId = ns("which_table"),
                       label = "Regular VS Total Pay",
                       choices = c("Regular", 
                        "Total","Both"),
                       justified = TRUE,
                       status = "danger"
                    ),
                    
                     mod_quick_date_range_ui(
                      id = ns("awe_dates"), 
                      label_quick = "Time Period", 
                      label_picker  = "Time period",
                      custom_picker = "calendar",     
                      presets = c("custom", "ytd", "past_year", "3y", "5y", "none")
                    ), 
                    
                    mod_filter_picker_ui(
                      id                 = ns("sector_filter"),
                      label              = "Sector Filtering",
                      multiple           = TRUE,
                      actions_box        = TRUE,
                      live_search        = TRUE,
                      virtual_scroll     = 10,
                      selected_text_format = "count > 2"
                    )
      ),
        accordion_controls = list(
                    mod_annotation_line_ui(ns("date_lines"),
                          type = "date",
                          title = "Add key dates",
                          add_label = "Add key date",
                          show_delete = TRUE,
                          auto_mask_date = TRUE),
    
                    mod_annotation_line_ui(ns("value_lines"),
                          type = "value",
                          title = "Add key values",
                          add_label = "Add key values",
                          show_delete = TRUE,
                          auto_mask_date = TRUE)
                  )
    )
  )
}

## AWE SERVER ## ----
employment_awe_stats_card_server <- function(id, conn = APP_DB$pool) {
  moduleServer(id, function(input, output, session){

    # Defaults + annotations
    shinyWidgets::updateRadioGroupButtons(session, "which_measure", selected = "Weekly Earnings")
    shinyWidgets::updateRadioGroupButtons(session, "which_table",   selected = "Regular")
    date_lines  <- mod_annotation_line_server("date_lines",  type = "date")
    value_lines <- mod_annotation_line_server("value_lines", type = "value")

    # UI -> measure -> codes + labels
    measure_key <- reactive({
      switch(input$which_measure %||% "Weekly Earnings",
        "Weekly Earnings"                   = "weekly_earnings",
        "Annual % change - Single Month"    = "annual_single_month",
        "Annual % change - 3 month average" = "annual_three_month_avg",
        "weekly_earnings"
      )
    })
    y_axis_label <- reactive({
      switch(measure_key(),
        weekly_earnings        = "Average Weekly Earnings (£)",
        annual_single_month    = "% change year on year (single month)",
        annual_three_month_avg = "% change year on year (3-month average)",
        "Average Weekly Earnings (£)"
      )
    })
    mode <- reactive({ input$which_table %||% "Both" })

    AWE_CODE_SETS <- list(
      weekly_earnings = list(
        total   = c("KAB9","KAC4","KAC7","K5BZ","K5C4","KAD8","K5CA","K5CD","K5CG"),
        regular = c("KAI7","KAJ2","KAJ5","K5DL","K5DO","KAK6","K5DU","K5DX","K5E2")
      ),
      annual_single_month = list(
        total   = c("KAC2","KAC5","KAC8","K5C2","K5C5","KAD9","K5CB","K5CE","K5CH"),
        regular = c("KAI8","KAJ3","KAJ6","K5DM","K5DP","KAK7","K5DV","K5DY","K5E3")
      ),
      annual_three_month_avg = list(
        total   = c("KAC3","KAC6","KAC9","K5C3","K5C6","KAE2","K5CC","K5CF","K5CI"),
        regular = c("KAI9","KAJ4","KAJ7","K5DN","K5DQ","KAK8","K5DW","K5DZ","K5E4")
      )
    )
    codes_cfg <- reactive({ req(AWE_CODE_SETS[[measure_key()]]) ; AWE_CODE_SETS[[measure_key()]] })

    # Base cleaned view (lazy)
    cleaned_full_tbl <- reactive({
      cc <- codes_cfg()
      get_awe_table(total_codes = cc$total, regular_codes = cc$regular)
      # %>% dplyr::compute(temporary = TRUE)
    }) %>% bindCache(measure_key())

    # Picker & dates
    sector <- mod_filter_picker_server(
      id       = "sector_filter",
      data_tbl = cleaned_full_tbl,
      column   = "sector",
      defaults_pretty = c("Whole Economy", "Public sector", "Private sector")
    )
    dates <- mod_quick_date_range_server(
      id        = "awe_dates",
      data_tbl  = cleaned_full_tbl,
      presets   = c("custom", "ytd", "past_year", "3y", "5y", "none"),
      default   = "5y", frequency = "monthly",
      custom_picker = "slider", 
      preserve_selection = TRUE
    )
    
    
    # 1) Collect all inputs that drive the query
    query_inputs <- reactive({
      list(
        measure_key = measure_key(),           # already bound-cached upstream
        dates       = dates$date_range(),
        sector      = sector$selected(),
        mode        = mode()
      )
    })
    
    # 2) Debounce the *grouped* inputs
    query_inputs_deb <- debounce(query_inputs, 300)


   
    # 3) Build data lazily off the debounced trigger
    dat <- eventReactive(query_inputs_deb(), {
      dr <- query_inputs_deb()$dates
      req(!is.null(dr), length(dr) == 2, !any(is.na(dr)))
      date_from <- as.Date(dr[[1]])
      date_to   <- as.Date(dr[[2]])
    
      where_in <- list(sector = query_inputs_deb()$sector)
      if (!identical(query_inputs_deb()$mode, "Both")) {
        where_in$type <- query_inputs_deb()$mode
      }
    
      t <- cleaned_full_tbl() %>%
        apply_filters_general(
          date_col  = "time_period",
          date_from = date_from,
          date_to   = date_to,
          where_in  = where_in
        )
    
      list(
        data = t %>% dplyr::collect(),
        sql  = sql_render_pool_safe(t)
      )
    })


    # Outputs
    output$table <- reactable::renderReactable({
      out <- dat(); req(nrow(out$data) > 0)
      reactable::reactable(out$data, sortable = TRUE, filterable = TRUE, resizable = TRUE,
                           pagination = TRUE, highlight = TRUE, striped = TRUE)
    })
    output$sql_query <- renderText({ dat()$sql })

    output$plot <- plotly::renderPlotly({
      out <- dat(); req(nrow(out$data) > 0)
      df_all <- out$data
      ylab <- y_axis_label()
      group_order_vec <- c(
        "Whole Economy","Private sector","Public sector",
        "Public Sector - excluding financial services","Manufacturing",
        "Construction","Services",
        "Wholesaling, retailing, hotels & restaurants","Finance and Business Services"
      )

      if (identical(mode(), "Both")) {
        df_total   <- dplyr::filter(df_all, .data$type == "Total")
        df_regular <- dplyr::filter(df_all, .data$type == "Regular")
        dbt_ts_plot(
          df  = df_total, df2 = df_regular, chart_type = "line",
          y_title = ylab, palette = dbt_palettes$gaf, initial_legend_mode = "hidden",
          group_order = group_order_vec,
          dataset1_name = "Total", dataset2_name = "Regular",
          line_dash1 = "solid", line_dash2 = "dash",
          vlines = date_lines$values_out(), vline_labels = date_lines$labels_out(),
          hlines = value_lines$values_out(), hline_labels = value_lines$labels_out()
        )
      } else {
        df <- dplyr::filter(df_all, .data$type == mode())
        dbt_ts_plot(
          df, chart_type = "line", y_title = ylab, palette = dbt_palettes$gaf,
          initial_legend_mode = "hidden", group_order = group_order_vec,
          vlines = date_lines$values_out(), vline_labels = date_lines$labels_out(),
          hlines = value_lines$values_out(), hline_labels = value_lines$labels_out()
        )
      }
    })
  })
}

## AWE DATA ## ----
get_awe_table <- function(total_codes, regular_codes) {
  stopifnot(is.character(total_codes), length(total_codes) > 0)
  stopifnot(is.character(regular_codes), length(regular_codes) > 0)

  total_tbl <- dplyr::tbl(APP_DB$pool, dbplyr::in_schema("ons", "labour_market__weekly_earnings_total"))
  reg_tbl   <- dplyr::tbl(APP_DB$pool, dbplyr::in_schema("ons", "labour_market__weekly_earnings_regular"))
  cast_date_sql <- dbplyr::sql("CAST(time_period AS DATE)")

  total_clean <- total_tbl %>%
    dplyr::filter(dataset_identifier_code %in% !!total_codes) %>%
    dplyr::mutate(time_period = !!cast_date_sql, type = "Total",   dataset_id = dataset_identifier_code) %>%
    dplyr::select(time_period, type, sector, dataset_id, value)

  regular_clean <- reg_tbl %>%
    dplyr::filter(dataset_identifier_code %in% !!regular_codes) %>%
    dplyr::mutate(time_period = !!cast_date_sql, type = "Regular", dataset_id = dataset_identifier_code) %>%
    dplyr::select(time_period, type, sector, dataset_id, value)

  joined <- dplyr::union_all(total_clean, regular_clean)

  # Normalize raw sector and prettify via CASE WHEN
  sector_norm_sql <- dbplyr::sql("
    regexp_replace(
      regexp_replace(
        btrim(regexp_replace(sector::text, '\\u00A0', ' ', 'g')),
        '\\\\s+', ' ', 'g'
      ),
      '&amp;', '&', 'g'
    )
  ")

  mapping_tbl <- tibble::tibble(
    sector_norm = c(
      "Private sector 3 4 5 6",
      "Public sector 3 4 5 6",
      "Services, SIC 2007 sections G-S",
      "Finance and business services,SIC 2007 sections K-N",
      "Public sector excluding financial services 5 6",
      "Construction, SIC 2007 section F",
      "Manufacturing, SIC 2007 section C",
      "Wholesaling, retailing, hotels & restaurants, SIC 2007 sections G & I"
    ),
    sector_pretty = c(
      "Private sector",
      "Public sector",
      "Services",
      "Finance and Business Services",
      "Public Sector - excluding financial services",
      "Construction",
      "Manufacturing",
      "Wholesaling, retailing, hotels & restaurants"
    )
  )

  cases <- purrr::map2(
    mapping_tbl$sector_norm, mapping_tbl$sector_pretty,
    ~ rlang::expr(.data$sector_norm == !!.x ~ !!.y)
  )

  joined %>%
    dplyr::mutate(sector_norm = !!sector_norm_sql) %>%
    dplyr::mutate(
      sector = dplyr::case_when(
        !!!cases,
        TRUE ~ .data$sector_norm
      )
    ) %>%
    dplyr::select(time_period, type, sector, dataset_id, value)
}
