## FULL TIME/ PART TIME UI ## -----
employment_ftpt_stats_card_ui <- function(id) {
  ns <- NS(id)
tags$script("$(function() {$('[data-toggle=\"tooltip\"]').tooltip();});")
   # ---- Your existing card ----
mod_govuk_data_vis_card_ui(
  id = ns("ftpt_trend_card"),
  title = "Employment Status by Full‑Time/Part‑Time",
  help_text = "Seasonally Adjsuted Full-time, part-time and temporary workers",
  help_text_source = "Source: ONS - A01 Labour market statistics summary data table 3",
  help_link = "https://data.trade.gov.uk/datasets/4609dc12-0dfa-4734-8ecb-6c50b59d163d", 
  help_link_text = "ONS Labour Market Overview",
  table_content  = reactableOutput(ns("ftpt_table"), height = "350px"),
  visual_content = plotlyOutput(ns("ftpt_plot"),  height = "350px"),
  query = ns("sql_query"),

  # Plain controls (stay visible in the dropdown)
  controls = list(
    shinyWidgets::radioGroupButtons(
      inputId = ns("which_group"),
      label = "Employment Type or Full Time Part Time",
      choices = c("Ungrouped", "By employment type", "By Full-time/Part Time"),
      justified = TRUE,
      status = "danger"
    ),
  
    mod_quick_date_range_ui(
      id = ns("ftpt_dates"),
      label_quick = "Quick Ranges",
      label_picker  = "Time period",
      custom_picker = "slider",   
      presets = c("custom", "ytd","past_year","3y","5y","none")
    ),
     mod_filter_picker_ui(
      id = ns("grouping_filter"),
      label = "Group Filtering",
      multiple = TRUE,
      actions_box = TRUE,
      live_search = TRUE,
      virtual_scroll = 10,
      selected_text_format = "count > 2"
    )
  ),

  # New: inner UI for the accordion (module wraps/stylizes it)
  accordion_controls = list(
    shinyWidgets::radioGroupButtons(
      inputId = ns("chart_type"),
      label   = "Choose a graph :",
      choiceNames = list(
            tags$span(`data-toggle`="tooltip", title = "Stacked Bar Chart", tags$i(class = "fa fa-bar-chart")),
            tags$span(`data-toggle`="tooltip", title = "Line Chart",        tags$i(class = "fa fa-line-chart")),
            tags$span(`data-toggle`="tooltip", title = "Area Chart",        tags$i(class = "fa fa-area-chart"))
          ),
      choiceValues = c("stacked_bar","line","stacked_area"),
      justified = TRUE,
      size = "sm",
      status = "danger"  # <-- chart type buttons in Danger style
    ),
    
    conditionalPanel(
      condition = sprintf("input['%s'] == 'stacked_bar'", ns("chart_type")),
      shinyWidgets::sliderTextInput(
        inputId = ns("stack_mode"),
        label = "Time Interval between bars",
        choices = c("monthly","quarterly","annually","5year","decade"),
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

  
}
## FULL TIME/ PART TIME SERVER ## -----
employment_ftpt_stats_card_server <- function(id, conn = APP_DB$pool) {
  moduleServer(id, function(input, output, session) {

    # UI defaults + annotations
    shinyWidgets::updateRadioGroupButtons(session, "which_group", selected = "Ungrouped")
    date_lines  <- mod_annotation_line_server("date_lines",  type = "date")
    value_lines <- mod_annotation_line_server("value_lines", type = "value")

    # Reactive codes + grouping column
    codes <- reactive(c("YCBK","YCBN","YCBQ","YCBT","MGRT","MGRW"))
    grouping_col <- reactive({
      switch(input$which_group %||% "Ungrouped",
        "Ungrouped"               = "employment_subgroup",
        "By employment type"      = "employment_type",
        "By Full-time/Part Time"  = "contract_type",
        "employment_subgroup"
      )
    })

    # Base cleaned view (lazy)
    cleaned_full_tbl <- reactive({
      get_ftpt_tbl(codes())
      # %>% dplyr::compute(temporary = TRUE)
    }) %>% bindCache(codes())

    # Picker and Date modules
    sector <- mod_filter_picker_server(
      id       = "grouping_filter",
      data_tbl = cleaned_full_tbl,
      column   = grouping_col
    )
    dates <- mod_quick_date_range_server(
      id        = "ftpt_dates",
      data_tbl  = cleaned_full_tbl,
      presets   = c("custom", "ytd", "past_year", "3y", "5y", "none"),
      default   = "5y", frequency = "monthly",
      custom_picker = "calendar", 
      preserve_selection = TRUE
    )
    

    # Final data (lazy filters -> collect + SQL)
    dat <- reactive({
      dr <- dates$date_range()
      date_from <- if (!is.null(dr) && length(dr) == 2) as.Date(dr[[1]]) else NULL
      date_to   <- if (!is.null(dr) && length(dr) == 2) as.Date(dr[[2]]) else NULL
      vals      <- sector$selected()

      t <- cleaned_full_tbl() %>%
        apply_filters_general(
          date_col     = "time_period",
          date_from    = date_from,
          date_to      = date_to,
          where_in     = setNames(list(vals), grouping_col())  # list(<col> = <values>)
        )

      list(
        data = t %>% dplyr::collect(),
        sql  = sql_render_pool_safe(t)
      )
    })

    # Outputs (unchanged)
    output$ftpt_table <- reactable::renderReactable({
      out <- dat(); req(nrow(out$data) > 0)
      reactable::reactable(out$data, sortable = TRUE, filterable = TRUE, resizable = TRUE,
                           pagination = TRUE, highlight = TRUE, striped = TRUE)
    })
    output$sql_query <- renderText({ dat()$sql })
    output$ftpt_plot <- plotly::renderPlotly({
      out <- dat(); req(nrow(out$data) > 0)
      dbt_ts_plot(
        df = out$data,
        chart_type = input$chart_type,
        bar_interval = input$stack_mode,       
        bar_agg = "last",
        x_title = "Time period (YYYY‑MM)", y_title = "No. 000s",
        palette = dbt_palettes$gaf, initial_legend_mode = "hidden",
        group_col = grouping_col(),
        vlines = date_lines$values_out(), vline_labels = date_lines$labels_out(),
        hlines = value_lines$values_out(), hline_labels = value_lines$labels_out()
      )
    })
  })
}
## FULL TIME/ PART TIME DATA ## ------
get_ftpt_tbl <- function(codes) {
  stopifnot(is.character(codes), length(codes) >= 1L)

  base <- dplyr::tbl(APP_DB$pool, dbplyr::in_schema("ons", "labour_market__employment_group"))

  period_sql <- dbplyr::sql("
    to_date(
      initcap(substr(
        btrim(split_part(
          regexp_replace(
            btrim(regexp_replace(time_period::text, '\\\\s+', ' ', 'g')),
            '[–—]', '-', 'g'
          ),
          '-', 2
        )),
        1, 8
      )),
      'Mon YYYY'
    )::date
  ")

  base %>%
    dplyr::filter(dataset_identifier_code %in% !!codes) %>%
    dplyr::mutate(time_period = !!period_sql) %>%
    dplyr::mutate(
      employment_type = dplyr::case_when(
        dataset_identifier_code %in% c("YCBK","YCBN") ~ "Employee",
        dataset_identifier_code %in% c("YCBQ","YCBT") ~ "Self-employed",
        dataset_identifier_code == "MGRT"            ~ "Unpaid Family workers",
        dataset_identifier_code == "MGRW"            ~ "Government supported training & employment programmes",
        TRUE ~ NA_character_
      ),
      contract_type = dplyr::case_when(
        dataset_identifier_code %in% c("YCBK","YCBQ") ~ "Full-time",
        dataset_identifier_code %in% c("YCBN","YCBT") ~ "Part-time",
        dataset_identifier_code == "MGRT"            ~ "Unpaid Family workers",
        dataset_identifier_code == "MGRW"            ~ "Government supported training & employment programmes",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(
      time_period,
      dataset_id = dataset_identifier_code,
      employment_type,
      contract_type,
      employment_subgroup,
      value
    )
}
