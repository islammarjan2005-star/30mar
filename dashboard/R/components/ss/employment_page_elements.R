# ### AWE STATS CARD ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# ## AWE UI ## -----
# employment_awe_stats_card_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     # ---- Filters Row ----
#     #tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "equal-height-buttons.css")),
# 
#     # ---- Your existing card ----
#     mod_govuk_data_vis_card_ui(
#       id = ns("awe_trend_card"),
#       title = "Average Weekly Earnings",
#       help_text = "Seasonally Adjusted Nominal Average Weekly Earnings in Great Britian",
#       help_text_source = "Source: ONS - A01 Labour market statistics summary data tables 13 & 15",
#       help_link = "https://data.trade.gov.uk/datasets/4609dc12-0dfa-4734-8ecb-6c50b59d163d", 
#       help_link_text = "ONS Labour Market Overview",
#       table_content = reactableOutput(ns("table"), height = "350px"),
#       visual_content = plotlyOutput(ns("plot"), height = "350px"), 
#       query = ns("sql_query"), 
#       controls =  list(
#                       shinyWidgets::radioGroupButtons(
#                        inputId = ns("which_measure"),
#                        label = "Measure",
#                        choices = c("Weekly Earnings", 
#                         "Annual % change - Single Month","Annual % change - 3 month average"),
#                        justified = TRUE,
#                        status = "danger"
#                     ),
#                     shinyWidgets::radioGroupButtons(
#                        inputId = ns("which_table"),
#                        label = "Regular VS Total Pay",
#                        choices = c("Regular", 
#                         "Total","Both"),
#                        justified = TRUE,
#                        status = "danger"
#                     ),
#                     
#                      mod_quick_date_range_ui(
#                       id = ns("awe_dates"), 
#                       label_quick = "Time Period", 
#                       label_picker  = "Time period",
#                       custom_picker = "calendar",     
#                       presets = c("custom", "ytd", "past_year", "3y", "5y", "none")
#                     ), 
#                     
#                     mod_filter_picker_ui(
#                       id                 = ns("sector_filter"),
#                       label              = "Sector Filtering",
#                       multiple           = TRUE,
#                       actions_box        = TRUE,
#                       live_search        = TRUE,
#                       virtual_scroll     = 10,
#                       selected_text_format = "count > 2"
#                     )
#       ),
#         accordion_controls = list(
#                     mod_annotation_line_ui(ns("date_lines"),
#                           type = "date",
#                           title = "Add key dates",
#                           add_label = "Add key date",
#                           show_delete = TRUE,
#                           auto_mask_date = TRUE),
#     
#                     mod_annotation_line_ui(ns("value_lines"),
#                           type = "value",
#                           title = "Add key values",
#                           add_label = "Add key values",
#                           show_delete = TRUE,
#                           auto_mask_date = TRUE)
#                   )
#     )
#   )
# }
# ## AWE SERVER ## ----
# employment_awe_stats_card_server <- function(id, conn = APP_DB$pool) {
#   moduleServer(id, function(input, output, session){
# 
#     # Defaults + annotations
#     shinyWidgets::updateRadioGroupButtons(session, "which_measure", selected = "Weekly Earnings")
#     shinyWidgets::updateRadioGroupButtons(session, "which_table",   selected = "Regular")
#     date_lines  <- mod_annotation_line_server("date_lines",  type = "date")
#     value_lines <- mod_annotation_line_server("value_lines", type = "value")
# 
#     # UI -> measure -> codes + labels
#     measure_key <- reactive({
#       switch(input$which_measure %||% "Weekly Earnings",
#         "Weekly Earnings"                   = "weekly_earnings",
#         "Annual % change - Single Month"    = "annual_single_month",
#         "Annual % change - 3 month average" = "annual_three_month_avg",
#         "weekly_earnings"
#       )
#     })
#     y_axis_label <- reactive({
#       switch(measure_key(),
#         weekly_earnings        = "Average Weekly Earnings (£)",
#         annual_single_month    = "% change year on year (single month)",
#         annual_three_month_avg = "% change year on year (3-month average)",
#         "Average Weekly Earnings (£)"
#       )
#     })
#     mode <- reactive({ input$which_table %||% "Both" })
# 
#     AWE_CODE_SETS <- list(
#       weekly_earnings = list(
#         total   = c("KAB9","KAC4","KAC7","K5BZ","K5C4","KAD8","K5CA","K5CD","K5CG"),
#         regular = c("KAI7","KAJ2","KAJ5","K5DL","K5DO","KAK6","K5DU","K5DX","K5E2")
#       ),
#       annual_single_month = list(
#         total   = c("KAC2","KAC5","KAC8","K5C2","K5C5","KAD9","K5CB","K5CE","K5CH"),
#         regular = c("KAI8","KAJ3","KAJ6","K5DM","K5DP","KAK7","K5DV","K5DY","K5E3")
#       ),
#       annual_three_month_avg = list(
#         total   = c("KAC3","KAC6","KAC9","K5C3","K5C6","KAE2","K5CC","K5CF","K5CI"),
#         regular = c("KAI9","KAJ4","KAJ7","K5DN","K5DQ","KAK8","K5DW","K5DZ","K5E4")
#       )
#     )
#     codes_cfg <- reactive({ req(AWE_CODE_SETS[[measure_key()]]) ; AWE_CODE_SETS[[measure_key()]] })
# 
#     # Base cleaned view (lazy)
#     cleaned_full_tbl <- reactive({
#       cc <- codes_cfg()
#       get_awe_table(total_codes = cc$total, regular_codes = cc$regular)
#       # %>% dplyr::compute(temporary = TRUE)
#     }) %>% bindCache(measure_key())
# 
#     # Picker & dates
#     sector <- mod_filter_picker_server(
#       id       = "sector_filter",
#       data_tbl = cleaned_full_tbl,
#       column   = "sector",
#       defaults_pretty = c("Whole Economy", "Public sector", "Private sector")
#     )
#     dates <- mod_quick_date_range_server(
#       id        = "awe_dates",
#       data_tbl  = cleaned_full_tbl,
#       presets   = c("custom", "ytd", "past_year", "3y", "5y", "none"),
#       default   = "5y", frequency = "monthly",
#       custom_picker = "slider", 
#       preserve_selection = TRUE
#     )
#     
#     
#     # 1) Collect all inputs that drive the query
#     query_inputs <- reactive({
#       list(
#         measure_key = measure_key(),           # already bound-cached upstream
#         dates       = dates$date_range(),
#         sector      = sector$selected(),
#         mode        = mode()
#       )
#     })
#     
#     # 2) Debounce the *grouped* inputs
#     query_inputs_deb <- debounce(query_inputs, 300)
# 
# 
#    
#     # 3) Build data lazily off the debounced trigger
#     dat <- eventReactive(query_inputs_deb(), {
#       dr <- query_inputs_deb()$dates
#       req(!is.null(dr), length(dr) == 2, !any(is.na(dr)))
#       date_from <- as.Date(dr[[1]])
#       date_to   <- as.Date(dr[[2]])
#     
#       where_in <- list(sector = query_inputs_deb()$sector)
#       if (!identical(query_inputs_deb()$mode, "Both")) {
#         where_in$type <- query_inputs_deb()$mode
#       }
#     
#       t <- cleaned_full_tbl() %>%
#         apply_filters_general(
#           date_col  = "time_period",
#           date_from = date_from,
#           date_to   = date_to,
#           where_in  = where_in
#         )
#     
#       list(
#         data = t %>% dplyr::collect(),
#         sql  = sql_render_pool_safe(t)
#       )
#     })
# 
# 
#     # Outputs
#     output$table <- reactable::renderReactable({
#       out <- dat(); req(nrow(out$data) > 0)
#       reactable::reactable(out$data, sortable = TRUE, filterable = TRUE, resizable = TRUE,
#                            pagination = TRUE, highlight = TRUE, striped = TRUE)
#     })
#     output$sql_query <- renderText({ dat()$sql })
# 
#     output$plot <- plotly::renderPlotly({
#       out <- dat(); req(nrow(out$data) > 0)
#       df_all <- out$data
#       ylab <- y_axis_label()
#       group_order_vec <- c(
#         "Whole Economy","Private sector","Public sector",
#         "Public Sector - excluding financial services","Manufacturing",
#         "Construction","Services",
#         "Wholesaling, retailing, hotels & restaurants","Finance and Business Services"
#       )
# 
#       if (identical(mode(), "Both")) {
#         df_total   <- dplyr::filter(df_all, .data$type == "Total")
#         df_regular <- dplyr::filter(df_all, .data$type == "Regular")
#         dbt_ts_plot(
#           df  = df_total, df2 = df_regular, chart_type = "line",
#           y_title = ylab, palette = dbt_palettes$gaf, initial_legend_mode = "hidden",
#           group_order = group_order_vec,
#           dataset1_name = "Total", dataset2_name = "Regular",
#           line_dash1 = "solid", line_dash2 = "dash",
#           vlines = date_lines$values_out(), vline_labels = date_lines$labels_out(),
#           hlines = value_lines$values_out(), hline_labels = value_lines$labels_out()
#         )
#       } else {
#         df <- dplyr::filter(df_all, .data$type == mode())
#         dbt_ts_plot(
#           df, chart_type = "line", y_title = ylab, palette = dbt_palettes$gaf,
#           initial_legend_mode = "hidden", group_order = group_order_vec,
#           vlines = date_lines$values_out(), vline_labels = date_lines$labels_out(),
#           hlines = value_lines$values_out(), hline_labels = value_lines$labels_out()
#         )
#       }
#     })
#   })
# }
# ## AWE DATA ## ----
# get_awe_table <- function(total_codes, regular_codes) {
#   stopifnot(is.character(total_codes), length(total_codes) > 0)
#   stopifnot(is.character(regular_codes), length(regular_codes) > 0)
# 
#   total_tbl <- dplyr::tbl(APP_DB$pool, dbplyr::in_schema("ons", "labour_market__weekly_earnings_total"))
#   reg_tbl   <- dplyr::tbl(APP_DB$pool, dbplyr::in_schema("ons", "labour_market__weekly_earnings_regular"))
#   cast_date_sql <- dbplyr::sql("CAST(time_period AS DATE)")
# 
#   total_clean <- total_tbl %>%
#     dplyr::filter(dataset_identifier_code %in% !!total_codes) %>%
#     dplyr::mutate(time_period = !!cast_date_sql, type = "Total",   dataset_id = dataset_identifier_code) %>%
#     dplyr::select(time_period, type, sector, dataset_id, value)
# 
#   regular_clean <- reg_tbl %>%
#     dplyr::filter(dataset_identifier_code %in% !!regular_codes) %>%
#     dplyr::mutate(time_period = !!cast_date_sql, type = "Regular", dataset_id = dataset_identifier_code) %>%
#     dplyr::select(time_period, type, sector, dataset_id, value)
# 
#   joined <- dplyr::union_all(total_clean, regular_clean)
# 
#   # Normalize raw sector and prettify via CASE WHEN
#   sector_norm_sql <- dbplyr::sql("
#     regexp_replace(
#       regexp_replace(
#         btrim(regexp_replace(sector::text, '\\u00A0', ' ', 'g')),
#         '\\\\s+', ' ', 'g'
#       ),
#       '&amp;', '&', 'g'
#     )
#   ")
# 
#   mapping_tbl <- tibble::tibble(
#     sector_norm = c(
#       "Private sector 3 4 5 6",
#       "Public sector 3 4 5 6",
#       "Services, SIC 2007 sections G-S",
#       "Finance and business services,SIC 2007 sections K-N",
#       "Public sector excluding financial services 5 6",
#       "Construction, SIC 2007 section F",
#       "Manufacturing, SIC 2007 section C",
#       "Wholesaling, retailing, hotels & restaurants, SIC 2007 sections G & I"
#     ),
#     sector_pretty = c(
#       "Private sector",
#       "Public sector",
#       "Services",
#       "Finance and Business Services",
#       "Public Sector - excluding financial services",
#       "Construction",
#       "Manufacturing",
#       "Wholesaling, retailing, hotels & restaurants"
#     )
#   )
# 
#   cases <- purrr::map2(
#     mapping_tbl$sector_norm, mapping_tbl$sector_pretty,
#     ~ rlang::expr(.data$sector_norm == !!.x ~ !!.y)
#   )
# 
#   joined %>%
#     dplyr::mutate(sector_norm = !!sector_norm_sql) %>%
#     dplyr::mutate(
#       sector = dplyr::case_when(
#         !!!cases,
#         TRUE ~ .data$sector_norm
#       )
#     ) %>%
#     dplyr::select(time_period, type, sector, dataset_id, value)
# }

### FULL TIME/ PART TIME CARD ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ## FULL TIME/ PART TIME UI ## -----
# employment_ftpt_stats_card_ui <- function(id) {
#   ns <- NS(id)
# tags$script("$(function() {$('[data-toggle=\"tooltip\"]').tooltip();});")
#    # ---- Your existing card ----
# mod_govuk_data_vis_card_ui(
#   id = ns("ftpt_trend_card"),
#   title = "Employment Status by Full‑Time/Part‑Time",
#   help_text = "Seasonally Adjsuted Full-time, part-time and temporary workers",
#   help_text_source = "Source: ONS - A01 Labour market statistics summary data table 3",
#   help_link = "https://data.trade.gov.uk/datasets/4609dc12-0dfa-4734-8ecb-6c50b59d163d", 
#   help_link_text = "ONS Labour Market Overview",
#   table_content  = reactableOutput(ns("ftpt_table"), height = "350px"),
#   visual_content = plotlyOutput(ns("ftpt_plot"),  height = "350px"),
#   query = ns("sql_query"),
# 
#   # Plain controls (stay visible in the dropdown)
#   controls = list(
#     shinyWidgets::radioGroupButtons(
#       inputId = ns("which_group"),
#       label = "Employment Type or Full Time Part Time",
#       choices = c("Ungrouped", "By employment type", "By Full-time/Part Time"),
#       justified = TRUE,
#       status = "danger"
#     ),
#   
#     mod_quick_date_range_ui(
#       id = ns("ftpt_dates"),
#       label_quick = "Quick Ranges",
#       label_picker  = "Time period",
#       custom_picker = "slider",   
#       presets = c("custom", "ytd","past_year","3y","5y","none")
#     ),
#      mod_filter_picker_ui(
#       id = ns("grouping_filter"),
#       label = "Group Filtering",
#       multiple = TRUE,
#       actions_box = TRUE,
#       live_search = TRUE,
#       virtual_scroll = 10,
#       selected_text_format = "count > 2"
#     )
#   ),
# 
#   # New: inner UI for the accordion (module wraps/stylizes it)
#   accordion_controls = list(
#     shinyWidgets::radioGroupButtons(
#       inputId = ns("chart_type"),
#       label   = "Choose a graph :",
#       choiceNames = list(
#             tags$span(`data-toggle`="tooltip", title = "Stacked Bar Chart", tags$i(class = "fa fa-bar-chart")),
#             tags$span(`data-toggle`="tooltip", title = "Line Chart",        tags$i(class = "fa fa-line-chart")),
#             tags$span(`data-toggle`="tooltip", title = "Area Chart",        tags$i(class = "fa fa-area-chart"))
#           ),
#       choiceValues = c("stacked_bar","line","stacked_area"),
#       justified = TRUE,
#       size = "sm",
#       status = "danger"  # <-- chart type buttons in Danger style
#     ),
#     
#     conditionalPanel(
#       condition = sprintf("input['%s'] == 'stacked_bar'", ns("chart_type")),
#       shinyWidgets::sliderTextInput(
#         inputId = ns("stack_mode"),
#         label = "Time Interval between bars",
#         choices = c("monthly","quarterly","annually","5year","decade"),
#         selected = "annually", 
#         grid = TRUE
#       )
#     ),
# 
#     mod_annotation_line_ui(ns("date_lines"),
#       type = "date", title = "Add key dates",
#       add_label = "Add key date", show_delete = TRUE, auto_mask_date = TRUE
#     ),
#     mod_annotation_line_ui(ns("value_lines"),
#       type = "value", title = "Add key values",
#       add_label = "Add key values", show_delete = TRUE, auto_mask_date = TRUE
#     )
#    
#   )
# )
# 
#   
# }
# ## FULL TIME/ PART TIME SERVER ## -----
# employment_ftpt_stats_card_server <- function(id, conn = APP_DB$pool) {
#   moduleServer(id, function(input, output, session) {
# 
#     # UI defaults + annotations
#     shinyWidgets::updateRadioGroupButtons(session, "which_group", selected = "Ungrouped")
#     date_lines  <- mod_annotation_line_server("date_lines",  type = "date")
#     value_lines <- mod_annotation_line_server("value_lines", type = "value")
# 
#     # Reactive codes + grouping column
#     codes <- reactive(c("YCBK","YCBN","YCBQ","YCBT","MGRT","MGRW"))
#     grouping_col <- reactive({
#       switch(input$which_group %||% "Ungrouped",
#         "Ungrouped"               = "employment_subgroup",
#         "By employment type"      = "employment_type",
#         "By Full-time/Part Time"  = "contract_type",
#         "employment_subgroup"
#       )
#     })
# 
#     # Base cleaned view (lazy)
#     cleaned_full_tbl <- reactive({
#       get_ftpt_tbl(codes())
#       # %>% dplyr::compute(temporary = TRUE)
#     }) %>% bindCache(codes())
# 
#     # Picker and Date modules
#     sector <- mod_filter_picker_server(
#       id       = "grouping_filter",
#       data_tbl = cleaned_full_tbl,
#       column   = grouping_col
#     )
#     dates <- mod_quick_date_range_server(
#       id        = "ftpt_dates",
#       data_tbl  = cleaned_full_tbl,
#       presets   = c("custom", "ytd", "past_year", "3y", "5y", "none"),
#       default   = "5y", frequency = "monthly",
#       custom_picker = "calendar", 
#       preserve_selection = TRUE
#     )
#     
# 
#     # Final data (lazy filters -> collect + SQL)
#     dat <- reactive({
#       dr <- dates$date_range()
#       date_from <- if (!is.null(dr) && length(dr) == 2) as.Date(dr[[1]]) else NULL
#       date_to   <- if (!is.null(dr) && length(dr) == 2) as.Date(dr[[2]]) else NULL
#       vals      <- sector$selected()
# 
#       t <- cleaned_full_tbl() %>%
#         apply_filters_general(
#           date_col     = "time_period",
#           date_from    = date_from,
#           date_to      = date_to,
#           where_in     = setNames(list(vals), grouping_col())  # list(<col> = <values>)
#         )
# 
#       list(
#         data = t %>% dplyr::collect(),
#         sql  = sql_render_pool_safe(t)
#       )
#     })
# 
#     # Outputs (unchanged)
#     output$ftpt_table <- reactable::renderReactable({
#       out <- dat(); req(nrow(out$data) > 0)
#       reactable::reactable(out$data, sortable = TRUE, filterable = TRUE, resizable = TRUE,
#                            pagination = TRUE, highlight = TRUE, striped = TRUE)
#     })
#     output$sql_query <- renderText({ dat()$sql })
#     output$ftpt_plot <- plotly::renderPlotly({
#       out <- dat(); req(nrow(out$data) > 0)
#       dbt_ts_plot(
#         df = out$data,
#         chart_type = input$chart_type,
#         bar_interval = input$stack_mode,       
#         bar_agg = "last",
#         x_title = "Time period (YYYY‑MM)", y_title = "No. 000s",
#         palette = dbt_palettes$gaf, initial_legend_mode = "hidden",
#         group_col = grouping_col(),
#         vlines = date_lines$values_out(), vline_labels = date_lines$labels_out(),
#         hlines = value_lines$values_out(), hline_labels = value_lines$labels_out()
#       )
#     })
#   })
# }
# ## FULL TIME/ PART TIME DATA ## ------
# get_ftpt_tbl <- function(codes) {
#   stopifnot(is.character(codes), length(codes) >= 1L)
# 
#   base <- dplyr::tbl(APP_DB$pool, dbplyr::in_schema("ons", "labour_market__employment_group"))
# 
#   period_sql <- dbplyr::sql("
#     to_date(
#       initcap(substr(
#         btrim(split_part(
#           regexp_replace(
#             btrim(regexp_replace(time_period::text, '\\\\s+', ' ', 'g')),
#             '[–—]', '-', 'g'
#           ),
#           '-', 2
#         )),
#         1, 8
#       )),
#       'Mon YYYY'
#     )::date
#   ")
# 
#   base %>%
#     dplyr::filter(dataset_identifier_code %in% !!codes) %>%
#     dplyr::mutate(time_period = !!period_sql) %>%
#     dplyr::mutate(
#       employment_type = dplyr::case_when(
#         dataset_identifier_code %in% c("YCBK","YCBN") ~ "Employee",
#         dataset_identifier_code %in% c("YCBQ","YCBT") ~ "Self-employed",
#         dataset_identifier_code == "MGRT"            ~ "Unpaid Family workers",
#         dataset_identifier_code == "MGRW"            ~ "Government supported training & employment programmes",
#         TRUE ~ NA_character_
#       ),
#       contract_type = dplyr::case_when(
#         dataset_identifier_code %in% c("YCBK","YCBQ") ~ "Full-time",
#         dataset_identifier_code %in% c("YCBN","YCBT") ~ "Part-time",
#         dataset_identifier_code == "MGRT"            ~ "Unpaid Family workers",
#         dataset_identifier_code == "MGRW"            ~ "Government supported training & employment programmes",
#         TRUE ~ NA_character_
#       )
#     ) %>%
#     dplyr::select(
#       time_period,
#       dataset_id = dataset_identifier_code,
#       employment_type,
#       contract_type,
#       employment_subgroup,
#       value
#     )
# }

### EMPLOYMENT BY AGE CARD ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ## EMPLOYMENT BY AGE UI ## -----
# employment_age_stats_card_ui <- function(id) {
#   ns <- NS(id)
#   tags$script("$(function() {$('[data-toggle=\"tooltip\"]').tooltip();});")
# 
#   mod_govuk_data_vis_card_ui(
#     id = ns("age_trend_card"),
#     title = "Employment by Age Group",
#     help_text = "Employment levels and rates by age group.",
#     help_text_source = "Source: ONS - A01 Labour market statistics summary data table 2",
#     help_link = "https://data.trade.gov.uk/datasets/4609dc12-0dfa-4734-8ecb-6c50b59d163d", 
#     help_link_text = "ONS Labour Market Overview",
#     table_content  = reactableOutput(ns("age_table"), height = "350px"),
#     visual_content = plotlyOutput(ns("age_plot"),  height = "350px"),
#     query = ns("sql_query"),
# 
#     # Plain controls (stay visible in the dropdown)
#     controls = list(
#       shinyWidgets::radioGroupButtons(
#         inputId = ns("which_measure"),
#         label = "Level or Rate",
#         choices = c("Level (000s)", "Rate (%)"),
#         justified = TRUE,
#         status = "danger"
#       ),
# 
#       mod_quick_date_range_ui(
#         id = ns("age_dates"),
#         label_quick = "Quick Ranges",
#         label_picker  = "Time period",
#         custom_picker = "slider",
#         presets = c("custom", "ytd", "past_year", "3y", "5y", "none")
#       ),
#       mod_filter_picker_ui(
#         id = ns("age_group_filter"),
#         label = "Age Group Filtering",
#         multiple = TRUE,
#         actions_box = TRUE,
#         live_search = TRUE,
#         virtual_scroll = 10,
#         selected_text_format = "count > 2"
#       )
#     ),
# 
#     # Inner UI for the accordion (module wraps/stylizes it)
#     accordion_controls = list(
#       shinyWidgets::radioGroupButtons(
#         inputId = ns("chart_type"),
#         label   = "Choose a graph :",
#         choiceNames = list(
#           tags$span(`data-toggle`="tooltip", title = "Stacked Bar Chart", tags$i(class = "fa fa-bar-chart")),
#           tags$span(`data-toggle`="tooltip", title = "Line Chart",        tags$i(class = "fa fa-line-chart")),
#           tags$span(`data-toggle`="tooltip", title = "Area Chart",        tags$i(class = "fa fa-area-chart"))
#         ),
#         choiceValues = c("stacked_bar","line","stacked_area"),
#         justified = TRUE,
#         size = "sm",
#         status = "danger"
#       ),
# 
#       conditionalPanel(
#         condition = sprintf("input['%s'] == 'stacked_bar'", ns("chart_type")),
#         shinyWidgets::sliderTextInput(
#           inputId = ns("stack_mode"),
#           label = "Time Interval between bars",
#           choices = c("monthly","quarterly","annually","5year","decade"),
#           selected = "annually",
#           grid = TRUE
#         )
#       ),
# 
#       mod_annotation_line_ui(ns("date_lines"),
#         type = "date", title = "Add key dates",
#         add_label = "Add key date", show_delete = TRUE, auto_mask_date = TRUE
#       ),
#       mod_annotation_line_ui(ns("value_lines"),
#         type = "value", title = "Add key values",
#         add_label = "Add key values", show_delete = TRUE, auto_mask_date = TRUE
#       )
#     )
#   )
# }
# 
# ## EMPLOYMENT BY AGE SERVER ## -----
# employment_age_stats_card_server <- function(id, conn = APP_DB$pool) {
#   moduleServer(id, function(input, output, session) {
# 
#     # UI defaults + annotations
#     shinyWidgets::updateRadioGroupButtons(session, "which_measure", selected = "Level (000s)")
#     shinyWidgets::updateRadioGroupButtons(session, "chart_type", selected = "stacked_area")
#     date_lines  <- mod_annotation_line_server("date_lines",  type = "date")
#     value_lines <- mod_annotation_line_server("value_lines", type = "value")
# 
#     # Reactive codes based on Level vs Rate selection
#     codes <- reactive({
#       sel <- input$which_measure %||% "Level (000s)"
#       if (sel == "Level (000s)") {
#         AGE_LEVEL_CODES
#       } else {
#         AGE_RATE_CODES
#       }
#     })
# 
#     # Y-axis label
#     y_axis_label <- reactive({
#       sel <- input$which_measure %||% "Level (000s)"
#       if (sel == "Level (000s)") "Employment (000s)" else "Employment Rate (%)"
#     })
# 
#     # Base cleaned view (lazy)
#     cleaned_full_tbl <- reactive({
#       get_age_tbl(codes())
#     }) %>% bindCache(input$which_measure)
# 
#     # Picker and Date modules
#     sector <- mod_filter_picker_server(
#       id       = "age_group_filter",
#       data_tbl = cleaned_full_tbl,
#       column   = "age_group"
#     )
#     dates <- mod_quick_date_range_server(
#       id        = "age_dates",
#       data_tbl  = cleaned_full_tbl,
#       presets   = c("custom", "ytd", "past_year", "3y", "5y", "none"),
#       default   = "5y", frequency = "monthly",
#       custom_picker = "slider",
#       preserve_selection = TRUE
#     )
# 
#     # Final data (lazy filters -> collect + SQL)
#     dat <- reactive({
#       dr <- dates$date_range()
#       date_from <- if (!is.null(dr) && length(dr) == 2) as.Date(dr[[1]]) else NULL
#       date_to   <- if (!is.null(dr) && length(dr) == 2) as.Date(dr[[2]]) else NULL
#       vals      <- sector$selected()
# 
#       t <- cleaned_full_tbl() %>%
#         apply_filters_general(
#           date_col     = "time_period",
#           date_from    = date_from,
#           date_to      = date_to,
#           where_in     = list(age_group = vals)
#         )
# 
#       list(
#         data = t %>% dplyr::collect(),
#         sql  = sql_render_pool_safe(t)
#       )
#     })
# 
#     # Outputs
#     output$age_table <- reactable::renderReactable({
#       out <- dat(); req(nrow(out$data) > 0)
#       reactable::reactable(out$data, sortable = TRUE, filterable = TRUE, resizable = TRUE,
#                            pagination = TRUE, highlight = TRUE, striped = TRUE)
#     })
#     output$sql_query <- renderText({ dat()$sql })
#     output$age_plot <- plotly::renderPlotly({
#       out <- dat(); req(nrow(out$data) > 0)
#       dbt_ts_plot(
#         df = out$data,
#         chart_type = input$chart_type,
#         bar_interval = input$stack_mode,
#         bar_agg = "last",
#         x_title = "Time period (YYYY\u2011MM)", y_title = y_axis_label(),
#         palette = dbt_palettes$gaf, initial_legend_mode = "hidden",
#         group_col = "age_group",
#         vlines = date_lines$values_out(), vline_labels = date_lines$labels_out(),
#         hlines = value_lines$values_out(), hline_labels = value_lines$labels_out()
#       )
#     })
#   })
# }
# 
# ### EMPLOYMENT BY AGE CODES ## ------
# 
# # Level codes: employment levels (000s) by age group
# AGE_LEVEL_CODES <- c("YBTO", "YBTR", "YBTU", "YBTX", "LF26", "LFK4")
# 
# # Rate codes: employment rates (%) by age group
# AGE_RATE_CODES  <- c("YBUA", "YBUD", "YBUG", "YBUJ", "LF2U", "LFK6")
# 
# # Code-to-age-group mapping (shared by both level and rate codes)
# AGE_CODE_MAP <- c(
#   # Level codes
#   "YBTO" = "16-17", "YBTR" = "18-24", "YBTU" = "25-34",
#   "YBTX" = "35-49", "LF26" = "50-64", "LFK4" = "65+",
#   # Rate codes
#   "YBUA" = "16-17", "YBUD" = "18-24", "YBUG" = "25-34",
#   "YBUJ" = "35-49", "LF2U" = "50-64", "LFK6" = "65+"
# )
# 
# 
# ### EMPLOYMENT BY AGE DATA ## ------
# get_age_tbl <- function(codes) {
#   stopifnot(is.character(codes), length(codes) >= 1L)
# 
#   base <- dplyr::tbl(APP_DB$pool, dbplyr::in_schema("ons", "labour_market__age_group"))
# 
#   period_sql <- dbplyr::sql("
#     to_date(
#       initcap(substr(
#         btrim(split_part(
#           regexp_replace(
#             btrim(regexp_replace(time_period::text, '\\\\s+', ' ', 'g')),
#             '[\\u2013\\u2014]', '-', 'g'
#           ),
#           '-', 2
#         )),
#         1, 8
#       )),
#       'Mon YYYY'
#     )::date
#   ")
# 
#   base %>%
#     dplyr::filter(dataset_identifier_code %in% !!codes) %>%
#     dplyr::mutate(time_period = !!period_sql) %>%
#     dplyr::mutate(
#       age_group = dplyr::case_when(
#         dataset_identifier_code %in% c("YBTO", "YBUA") ~ "16-17",
#         dataset_identifier_code %in% c("YBTR", "YBUD") ~ "18-24",
#         dataset_identifier_code %in% c("YBTU", "YBUG") ~ "25-34",
#         dataset_identifier_code %in% c("YBTX", "YBUJ") ~ "35-49",
#         dataset_identifier_code %in% c("LF26", "LF2U") ~ "50-64",
#         dataset_identifier_code %in% c("LFK4", "LFK6") ~ "65+",
#         TRUE ~ NA_character_
#       )
#     ) %>%
#     dplyr::select(
#       time_period,
#       dataset_id = dataset_identifier_code,
#       age_group,
#       value
#     )
# }

### TOTAL EMPLOYED KPI CARD ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ## TOTAL EMPLOYED KPI UI ## -----
# employment_employ_kpi_card_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     uiOutput(ns("card"))     # this renders the KPI card UI
#   )
# }
# 
# ## TOTAL EMPLOYED KPI SERVER ## -----
# employment_employ_kpi_card_server <- function(id) {
#   moduleServer(id, function(input, output, session) {
# 
#     output$card <- build_kpi_card(
#       id              = session$ns("employ"),
#       title           = "Total Employed",
#       schema          = "ons",
#       table           = "labour_market__age_group",
#       filter_col      = "dataset_identifier_code",
#       filter_value    = "LF2G",
#       parse_mode      = "MMM-MMM YYYY",
#       good_if_increase = TRUE,
#       format_headline = govuk_format_millions_10k,
#       format_delta    = govuk_format_number
#     )
#   })
# }

### EMPLOYMENT RATE KPI CARD ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ## EMPLOYMENT RATE KPI UI ## -----
# employment_employ_rate_kpi_card_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     uiOutput(ns("card"))
#   )
# }
# 
# ## EMPLOYMENT RATE KPI SERVER ## -----
# employment_employ_rate_kpi_card_server <- function(id) {
#   moduleServer(id, function(input, output, session) {
# 
#     output$card <- build_kpi_card(
#       id              = session$ns("employ_rate"),
#       title           = "Employment Rate",
#       schema          = "ons",
#       table           = "labour_market__age_group",
#       filter_col      = "dataset_identifier_code",
#       filter_value    = "LF24",
#       parse_mode      = "MMM-MMM YYYY",
#       good_if_increase = TRUE,
#       format_headline = govuk_format_percent1,
#       format_delta    = govuk_format_percent1
#     )
#   })
# }

### AWE TOTAL PAY KPI CARD ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ## AWE TOTAL PAY KPI UI ## -----
# employment_awe_totalpay_kpi_card_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     uiOutput(ns("card"))
#   )
# }
# 
# ## AWE TOTAL PAY KPI SERVER ## -----
# employment_awe_totalpay_kpi_card_server <- function(id) {
#   moduleServer(id, function(input, output, session) {
# 
#     output$card <- build_kpi_card(
#       id              = session$ns("awe_totalpay"),
#       title           = "Average Weekly Earnings (Total Pay)",
#       schema          = "ons",
#       table           = "labour_market__weekly_earnings_total",
#       filter_col      = "dataset_identifier_code",
#       filter_value    = "KAB9",
#       parse_mode      = "MMM YYYY",
#       good_if_increase = TRUE,
#       format_headline  = govuk_format_gbp,
#       format_delta     = govuk_format_gbp
#     )
#   })
# }