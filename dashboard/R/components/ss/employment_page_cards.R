# 
# #--- AWE CARD UI ----
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
#       help_text = "Seasonally Adjusted Nominal Average Weekly Earnings in Great Britian. Source: ONS - A01 Labour market statistics summary data tables 13 & 15",
#       table_content = reactableOutput(ns("table"), height = "350px"),
#       visual_content = plotlyOutput(ns("plot"), height = "350px"), 
#       query = tags$div(
#                 class = "code-block",
#                 shiny::verbatimTextOutput(ns("sql_query"))
#               ), 
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
#                     mod_filter_picker_ui(
#                       id                 = ns("sector_filter"),
#                       label              = "Sector Filtering",
#                       multiple           = TRUE,
#                       actions_box        = TRUE,
#                       live_search        = TRUE,
#                       virtual_scroll     = 10,
#                       selected_text_format = "count > 2"
#                     ),
# 
#                     mod_quick_date_range_ui(
#                       id = ns("awe_dates"), 
#                       label_quick = "Quick Ranges", 
#                       label_calendar = "Custom Date Range", 
#                       inline_calendar = TRUE,
#                       presets = c("latest", "ytd", "past_year", "3y", "5y", "none")
#                     ), 
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
# 
# #--- AWE CARD SERVER ----
# employment_awe_stats_card_server <- function(
#   id,
#   conn
# ) {
#   moduleServer(id, function(input, output, session){
# 
#     first_bounds_applied <- reactiveVal(FALSE)
# 
#     mode <- reactive({
#       req(input$which_table)  # "Total" | "Regular" | "Both"
#       input$which_table
#     })
# 
#     # -- Map UI choice to internal key ----------------------------------------
#     measure_key <- reactive({
#       req(input$which_measure)
#       switch(input$which_measure,
#         "Weekly Earnings"                      = "weekly_earnings",
#         "Annual % change - Single Month"       = "annual_single_month",
#         "Annual % change - 3 month average"    = "annual_three_month_avg",
#         "weekly_earnings"
#       )
#     })
# 
#     # Y-axis label placeholder switching
#     y_axis_label <- reactive({
#       switch(measure_key(),
#         weekly_earnings        = "Average Weekly Earnings (£)",
#         annual_single_month    = "% change year on year (single month)",
#         annual_three_month_avg = "% change year on year (3-month average)",
#         "TODO: Y-axis label"
#       )
#     })
# 
#     # Codes config for the current measure (Total & Regular)
#     codes_cfg <- reactive({
#       cfg <- AWE_CODE_SETS[[measure_key()]]
#       req(!is.null(cfg), length(cfg$total) > 0, length(cfg$regular) > 0)
#       cfg
#     })
# 
#     # --- Fixed dataset configs used for choices/bounds only -------------------
#     total_cfg <- reactive({
#       list(
#         schema     = "ons",
#         table_name = "labour_market__weekly_earnings_total",
#         codes      = codes_cfg()$total
#       )
#     })
#     regular_cfg <- reactive({
#       list(
#         schema     = "ons",
#         table_name = "labour_market__weekly_earnings_regular",
#         codes      = codes_cfg()$regular
#       )
#     })
# 
#     # 1) Sector choices (union across BOTH tables) ----------------------------
#     
#       # NEW: modular sector picker — parity with previous behaviour
#       sector <- mod_filter_picker_server(
#         id = "sector_filter",
#         tables = reactive(list(
#           list(schema = total_cfg()$schema,   table = total_cfg()$table_name),
#           list(schema = regular_cfg()$schema, table = regular_cfg()$table_name)
#         )),
#         column = "sector",
#         # This mapping provider reproduces your existing pretty map behaviour
#         mapping_provider = function(raw_vals) {
#           build_mapping_index(
#             raw_values = raw_vals,
#             pretty_map = awe_default_sector_map()
#           )
#         },
#         # Same defaults you had before
#         defaults_pretty = c("Whole Economy", "Public sector", "Private sector"),
#         # Same normalisation used previously
#         normalise_fn    = .normalise_label
#       )
#       
#           # >>>> NEW: Bounds reactive using your helper (union Total & Regular)
#           bounds_reac <- reactive({
#             bt <- fetch_date_bounds(
#               schema               = total_cfg()$schema,
#               table                = total_cfg()$table_name,
#               dataset_id_codes     = total_cfg()$codes,
#               date_col             = "time_period"
#             )
#             br <- fetch_date_bounds(
#               schema               = regular_cfg()$schema,
#               table                = regular_cfg()$table_name,
#               dataset_id_codes     = regular_cfg()$codes,
#               date_col             = "time_period"
#             )
# 
#       mins <- as.Date(na.omit(c(bt$min, br$min)))
#       maxs <- as.Date(na.omit(c(bt$max, br$max)))
# 
#       list(
#         min = if (length(mins)) min(mins) else NA_Date_,
#         max = if (length(maxs)) max(maxs) else NA_Date_
#       )
#     })
# 
#     # >>>> NEW: Instantiate modular quick ranges control (server side)
#     dates <- mod_quick_date_range_server(
#       id                 = "awe_dates",
#       bounds_reactive    = bounds_reac,           # your union MIN/MAX
#       presets            = c("latest", "ytd", "past_year", "3y", "5y", "none"),
#       default            = "5y",
#       frequency          = "monthly",
#       preserve_selection = TRUE
#     )
# 
#     # 4) Data (always combined; filter by `type` later) -----------------------
#     dat <- reactive({
#       chosen_canonical <- {
#         cs <- input$sectors
#         if (is.null(cs) || !length(cs)) NULL else .normalise_label(as.character(cs))
#       }
# 
#       raw_filter <- {
#               sel <- sector$selected()
#               if (is.null(sel)) NULL else sector$expand_raw()(sel)
#             }
# 
#       # >>>> Use the module-provided date range
#       dr <- dates$date_range()
#       date_from <- if (!is.null(dr) && length(dr) == 2) as.Date(dr[[1]]) else NULL
#       date_to   <- if (!is.null(dr) && length(dr) == 2) as.Date(dr[[2]]) else NULL
# 
#       cc <- codes_cfg()
# 
#       get_awe_table(
#         total_codes   = cc$total,
#         regular_codes = cc$regular,
#         sector_filter = raw_filter,
#         date_from     = date_from,
#         date_to       = date_to
#       )
#     })
# 
#     # Server side of adding vertical and horizontal lines 
#     date_lines <- mod_annotation_line_server("date_lines", type = "date")
#     value_lines <- mod_annotation_line_server("value_lines", type = "value")
# 
#     # 5) Outputs ---------------------------------------------------------------
# 
#     # Table
#     output$table <- reactable::renderReactable({
#       out <- dat()
#       req(!is.null(out), !is.null(out$data))
#       df_all <- out$data
#       req(nrow(df_all) > 0)
# 
#       df_show <- if (identical(mode(), "Both")) df_all else dplyr::filter(df_all, .data$type == mode())
# 
#       reactable::reactable(
#         df_show,
#         sortable   = TRUE,
#         filterable = TRUE,
#         resizable  = TRUE,
#         pagination = TRUE,
#         highlight  = TRUE,
#         striped    = TRUE,
#         bordered   = FALSE
#       )
#     })
# 
#     output$sql_query <- renderText({
#       out <- dat()
#       out$sql
#     })
# 
#     # Plot
#     output$plot <- plotly::renderPlotly({
#       out <- dat()
#       req(!is.null(out), !is.null(out$data))
#       df_all <- out$data
#       req(nrow(df_all) > 0)
# 
#       group_order_vec <- c(
#         "Whole Economy",
#         "Private sector",
#         "Public sector",
#         "Public Sector - excluding financial services",
#         "Manufacturing",
#         "Construction",
#         "Services",
#         "Wholesaling, retailing, hotels & restaurants",
#         "Finance and Business Services"
#       )
# 
#       ylab <- y_axis_label()
# 
#       if (identical(mode(), "Both")) {
#         df_total   <- dplyr::filter(df_all, .data$type == "Total")
#         df_regular <- dplyr::filter(df_all, .data$type == "Regular")
#         dbt_ts_plot(
#           df  = df_total,
#           df2 = df_regular,
#           chart_type = "line",
#           y_title = ylab,
#           palette = dbt_palettes$gaf,
#           initial_legend_mode = "hidden",
#           group_order = group_order_vec,
#           dataset1_name = "Total",
#           dataset2_name = "Regular",
#           line_dash1    = "solid",
#           line_dash2    = "dash",
#           vlines       = date_lines$values_out(),
#           vline_labels = date_lines$labels_out()
#         )
#       } else if (identical(mode(), "Total")) {
#         df <- dplyr::filter(df_all, .data$type == "Total")
#         dbt_ts_plot(
#           df,
#           chart_type = "line",
#           y_title = ylab,
#           palette = dbt_palettes$gaf,
#           initial_legend_mode = "hidden",
#           group_order = group_order_vec,
#           vlines       = date_lines$values_out(),
#           vline_labels = date_lines$labels_out()
#         )
#       } else { # "Regular"
#         df <- dplyr::filter(df_all, .data$type == "Regular")
#         dbt_ts_plot(
#           df,
#           chart_type = "line",
#           y_title = ylab,
#           palette = dbt_palettes$gaf,
#           initial_legend_mode = "hidden",
#           group_order = group_order_vec,
#           vlines       = date_lines$values_out(),
#           vline_labels = date_lines$labels_out()
#         )
#       }
#     })
#   })
# }
# 
# #--- AWE DATASET -----------------------------------
# # For each measure, define the code lists to use for the Total and Regular tables.
# AWE_CODE_SETS <- list(
#   weekly_earnings = list(
#     total   = c("KAB9","KAC4","KAC7","K5BZ","K5C4","KAD8","K5CA","K5CD","K5CG"),  
#     regular = c("KAI7","KAJ2","KAJ5","K5DL","K5DO","KAK6","K5DU","K5DX","K5E2")   
#   ),
#   annual_single_month = list(
#     total   = c("KAC2", "KAC5", "KAC8", "K5C2", "K5C5", "KAD9", "K5CB", "K5CE", "K5CH"),  
#     regular = c("KAI8", "KAJ3", "KAJ6", "K5DM", "K5DP", "KAK7", "K5DV", "K5DY", "K5E3")   
#   ),
#   annual_three_month_avg = list(
#     total   = c("KAC3", "KAC6", "KAC9", "K5C3", "K5C6", "KAE2", "K5CC", "K5CF", "K5CI"),          
#     regular = c("KAI9", "KAJ4", "KAJ7", "K5DN", "K5DQ", "KAK8", "K5DW", "K5DZ", "K5E4")           
#   )
# )
# 
# # Fetch AWE data from BOTH Total and Regular tables, with optional filters.
# # Returns a combined tibble with a `type` column: "Total" or "Regular".
# # Sector names are ALWAYS mapped to pretty labels used by the picker/plots.
# get_awe_table <- function(
#   total_codes,              # character vector of codes for TOTAL table (required)
#   regular_codes,            # character vector of codes for REGULAR table (required)
#   sector_filter = NULL,     # RAW labels to include (from mapping_index$expand)
#   date_from     = NULL,
#   date_to       = NULL
# ) {
#   stopifnot(is.character(total_codes), length(total_codes) > 0)
#   stopifnot(is.character(regular_codes), length(regular_codes) > 0)
# 
#   total_schema <- "ons"
#   total_table  <- "labour_market__weekly_earnings_total"
# 
#   regular_schema <- "ons"
#   regular_table  <- "labour_market__weekly_earnings_regular"
# 
#   # Build WHERE fragments shared by both queries (date/sector), plus code set per table
#   common_clauses <- list()
#   if (!is.null(date_from)) {
#     date_from <- as.Date(date_from)
#     common_clauses <- append(common_clauses, list(
#       glue::glue_sql("CAST(time_period AS DATE) >= {date_from}", .con = APP_DB$pool)
#     ))
#   }
#   if (!is.null(date_to)) {
#     date_to <- as.Date(date_to)
#     common_clauses <- append(common_clauses, list(
#       glue::glue_sql("CAST(time_period AS DATE) <= {date_to}", .con = APP_DB$pool)
#     ))
#   }
#   if (!is.null(sector_filter) && length(sector_filter) > 0) {
#     common_clauses <- append(common_clauses, list(
#       glue::glue_sql("sector IN ({sectors*})", sectors = sector_filter, .con = APP_DB$pool)
#     ))
#   }
# 
#   where_total <- c(
#     list(glue::glue_sql("dataset_indentifier_code IN ({vals*})", vals = total_codes,   .con = APP_DB$pool)),
#     common_clauses
#   )
#   where_regular <- c(
#     list(glue::glue_sql("dataset_indentifier_code IN ({vals*})", vals = regular_codes, .con = APP_DB$pool)),
#     common_clauses
#   )
# 
#   where_total_sql   <- glue::glue_sql_collapse(where_total, sep = " AND ")
#   where_regular_sql <- glue::glue_sql_collapse(where_regular, sep = " AND ")
# 
#   # Union both datasets and tag with a `type` column
#   sql <- glue::glue_sql("
#     WITH total AS (
#       SELECT
#         'Total'::text                 AS type,
#         sector,
#         CAST(time_period AS DATE)     AS time_period,
#         dataset_indentifier_code      AS dataset_id,
#         value
#       FROM {`total_schema`}.{`total_table`}
#       WHERE {where_total_sql}
#     ),
#     regular AS (
#       SELECT
#         'Regular'::text               AS type,
#         sector,
#         CAST(time_period AS DATE)     AS time_period,
#         dataset_indentifier_code      AS dataset_id,
#         value
#       FROM {`regular_schema`}.{`regular_table`}
#       WHERE {where_regular_sql}
#     )
#     SELECT * FROM total
#     UNION ALL
#     SELECT * FROM regular
#   ", .con = APP_DB$pool)
# 
#   data <- DBI::dbGetQuery(APP_DB$pool, sql)
# 
#   # Always apply the SAME mapping used for the picker/plots
#   if (!is.null(data) && nrow(data) > 0) {
#     idx  <- build_mapping_index(raw_values = data$sector, pretty_map = awe_default_sector_map())
#     data <- idx$map_data(data, "sector")
#   }
# 
#   list(
#     data = tibble::as_tibble(data),
#     sql  = as.character(sql)
#   )
# }
# 
# #--- FULLTIME, PARTTIME, TEMPORARY WORKERS UI----
# employment_ftpt_stats_card_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     # ---- Your existing card ----
#     mod_govuk_data_vis_card_ui(
#       id = ns("ftpt_trend_card"),
#       title = "Employment Status by Full‑Time/Part‑Time",
#       help_text = "Placeholder",
#       table_content = reactableOutput(ns("ftpt_table"), height = "350px"),
#       visual_content = plotlyOutput(ns("ftpt_plot"), height = "350px"), 
#       query = textOutput(ns("ftpt_sql_query")), 
#       controls =  list(
#                       shinyWidgets::radioGroupButtons(
#                        inputId = ns("which_group"),
#                        label = "Employment Type or Full Time Part Time",
#                        choices = c("Ungrouped", 
#                         "By employment type","By Full-time/Part Time"),
#                        justified = TRUE,
#                        status = "danger"
#                     ),
#                       mod_filter_picker_ui(
#                         id                 = ns("grouping_filter"),
#                         label              = "Group Filtering",
#                         multiple           = TRUE,
#                         actions_box        = TRUE,
#                         live_search        = TRUE,
#                         virtual_scroll     = 10,
#                         selected_text_format = "count > 2"
#                     ),
#                     mod_quick_date_range_ui(
#                       id = ns("ftpt_dates"), 
#                       label_quick = "Quick Ranges", 
#                       label_calendar = "Custom Date Range", 
#                       inline_calendar = TRUE,
#                       presets = c("latest", "ytd", "past_year", "3y", "5y", "none")
#                     ), 
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
# 
# #--- FULLTIME, PARTTIME, TEMPORARY WORKERS SERVER----
# 
# employment_ftpt_stats_card_server <- function(
#   id,
#   conn
# ) {
#   moduleServer(id, function(input, output, session){
#     # Pre Load Default Radio Button Selection ---- 
#     shinyWidgets::updateRadioGroupButtons(session, "which_group", selected = "Ungrouped")
# 
# 
#     #Define config -----------
#     base_cfg <- reactive({
#       list(
#         schema             = "ons",
#         table_name         = "labour_market__employment_group",
#         codes              = c("YCBK","YCBN","YCBQ","YCBT","MGRT","MGRW"),
#         pre_filter_column  = "dataset_indentifier_code"  # <- ensure spelling matches your DB
#       )
#     })
#     
#     # 2) Map radio selection -> grouping column (reactive)
#     grouping_col <- reactive({
#       sel <- input$which_group
#       if (is.null(sel)) sel <- "Ungrouped"  # default/fallback
#     
#       if (sel == "Ungrouped") {
#         "employment_subgroup"
#       } else if (sel == "By employment type") {
#         "employment_type"
#       } else if (sel == "By Full-time/Part Time") {
#         "contract_type"
#       } else {
#         "employment_subgroup"  # safe fallback
#       }
#     })
#     
#     
#     # 3) Final reactive config that other parts of the app can consume
#     cfg <- reactive({
#       b <- base_cfg()
#       b$grouping <- grouping_col()
#       b
#     })
# 
# 
# 
#     
#     #Vertical Label -------------
#     
#     date_lines <- mod_annotation_line_server("date_lines", type = "date")
#     value_lines <- mod_annotation_line_server("value_lines", type = "value")
#   
#     #Get Date Bounds ---------
# 
#     bounds_reac <- reactive({
#       bounds <- fetch_date_bounds(
#         schema             = cfg()$schema,
#         table              = cfg()$table_name,
#         dataset_id_codes   = cfg()$codes,
#         date_col           = "time_period",
#         period_is_castable = FALSE
# 
#       )
#     
#       # Derive safe min/max for the date-range module
#       mins <- as.Date(na.omit(bounds$min))
#       maxs <- as.Date(na.omit(bounds$max))
#   
#     
#       list(
#         min = if (length(mins)) min(mins) else NA_Date_,
#         max = if (length(maxs)) max(maxs) else NA_Date_
#       )
#     })
#     
#       # 1) Sector choices (union across BOTH tables) ----------------------------
#     
#       # NEW: modular sector picker — parity with previous behaviour
#       sector <- mod_filter_picker_server(
#         id = "grouping_filter",
#         tables = reactive(list(
#           list(schema = cfg()$schema, table = cfg()$table_name)
#         )),
#         column = reactive(cfg()$grouping),
#         pre_filter_column = reactive(cfg()$pre_filter_column),
#         pre_filter_values = reactive(cfg()$codes)
#       )
# 
# 
#     # >>>> NEW: Instantiate modular quick ranges control (server side)
#     dates <- mod_quick_date_range_server(
#       id                 = "ftpt_dates",
#       bounds_reactive    = bounds_reac,           # your union MIN/MAX
#       presets            = c("latest", "ytd", "past_year", "3y", "5y", "none"),
#       default            = "5y",
#       frequency          = "monthly",
#       preserve_selection = TRUE
#     )
# 
#             
#     # Get Final Reduced Data set -----------------------
#     dat <- reactive({
#       
#        # >>>> Use the module-provided date range
#       dr <- dates$date_range()
#       date_from <- if (!is.null(dr) && length(dr) == 2) as.Date(dr[[1]]) else NULL
#       date_to   <- if (!is.null(dr) && length(dr) == 2) as.Date(dr[[2]]) else NULL
#       
#       selected_vals <- sector$selected() 
#       
#       get_ftpt_table(
#         codes = cfg()$codes,
#         filter_column = reactive(cfg()$grouping), 
#         filter_values = selected_vals,
#         date_from = date_from,
#         date_to = date_to
#       )
#     })
#     
#     # OUTPUTS ------------
#     # Table: filter the combined data by type; for "Both" do not filter
#     output$ftpt_table <- reactable::renderReactable({
#       out <- dat()
#       req(!is.null(out), !is.null(out$data))
#       df_all <- out$data
# 
#       reactable::reactable(
#         df_all,
#         sortable   = TRUE,
#         filterable = TRUE,
#         resizable  = TRUE,
#         pagination = TRUE,
#         highlight  = TRUE,
#         striped    = TRUE,
#         bordered   = FALSE
#       )
#     })
#     
#     output$ftpt_sql_query <- renderText({
#       out <- dat() 
#       out$sql
#       })
#         
#         # Plot: for "Both" pass split dfs to enable solid vs dashed; else single
#     output$ftpt_plot <- plotly::renderPlotly({
#       out <- dat()
#       req(!is.null(out), !is.null(out$data))
#       df <- out$data
#       
#         dbt_ts_plot(
#           df  = df,
#           chart_type = "stacked_area",
#           x_title = "Time period (YYYY‑MM)",
#           y_title = "No. 000s",
#           palette = dbt_palettes$gaf,
#           initial_legend_mode = "hidden",
#           group_col = "employment_subgroup",
#           vlines       = date_lines$values_out(),
#           vline_labels = date_lines$labels_out()
#         )
#     })
# 
#   })
# }
# 
# #--- FULLTIME, PARTTIME, TEMPORARY WORKERS DATA SET------
# # For each measure, define the code lists to use for the Total and Regular tables.
# FTPT_CODE_SET <- c("YCBK", "YCBN", "YCBQ", "YCBT", "MGRT", "MGRW")
# 
# # Optionally accepts static values or Shiny reactives (if called inside a reactive context)
# as_value <- function(x) if (shiny::is.reactive(x)) x() else x
# 
# get_ftpt_table <- function(
#   codes, 
#   filter_values = NULL,      # <- renamed from sectors_filter
#   filter_column = NULL,      # <- NEW: which column to apply the IN() filter to
#   date_from = NULL, 
#   date_to   = NULL
# ){
#   # Unwrap if reactives were passed (ensure this is called from a reactive context if so)
#   codes         <- as_value(codes)
#   filter_values <- as_value(filter_values)
#   filter_column <- as_value(filter_column)
#   date_from     <- as_value(date_from)
#   date_to       <- as_value(date_to)
# 
#   # ---- Validate / normalize inputs ----
#   stopifnot(is.character(codes), length(codes) >= 1L)
# 
#   if (!is.null(filter_column)) {
#     stopifnot(is.character(filter_column), length(filter_column) == 1L, nzchar(filter_column))
#   }
# 
#   if (!is.null(filter_values)) {
#     filter_values <- as.character(unlist(filter_values, use.names = FALSE))
#     filter_values <- filter_values[!is.na(filter_values) & nzchar(filter_values)]
#     if (!length(filter_values)) filter_values <- NULL
#   }
# 
#   # Define the *same* date parsing expression used in SELECT and WHERE
#   period_expr <- DBI::SQL("
#     to_date(
#       initcap(substr(
#         btrim(split_part(
#           regexp_replace(
#             btrim(regexp_replace(time_period::text, '\\s+', ' ', 'g')), -- normalise spaces
#             '[–—]', '-', 'g'                                            -- normalise dash
#           ),
#           '-', 2                                                        -- take the part after '-'
#         )),
#         1, 8                                                            -- take exactly 'Mon YYYY'
#       )),
#       'Mon YYYY'
#     )::date
#   ")
# 
#   # Build WHERE fragments (codes, dates, optional column filter)
#   conds <- c(
#     glue::glue_sql("dataset_indentifier_code IN ({codes*})", codes = codes, .con = APP_DB$pool),
#     if (!is.null(date_from)) glue::glue_sql("{period_expr} >= {as.Date(date_from)}", period_expr = period_expr, .con = APP_DB$pool),
#     if (!is.null(date_to))   glue::glue_sql("{period_expr} <= {as.Date(date_to)}",   period_expr = period_expr, .con = APP_DB$pool),
#     if (!is.null(filter_column) && !is.null(filter_values))
#       glue::glue_sql("{`filter_column`} IN ({vals*})", filter_column = filter_column, vals = filter_values, .con = APP_DB$pool)
#   )
# 
#   where_clauses <- glue::glue_sql_collapse(conds, sep = " AND ")
# 
#   # Query: SELECT uses the same period_expr as WHERE
#   sql <- glue::glue_sql("
#     SELECT
#       {period_expr}                 AS time_period,
#       dataset_indentifier_code      AS dataset_id,
# 
#       /* 1) employment_type */
#       CASE
#         WHEN dataset_indentifier_code IN ('YCBK', 'YCBN') THEN 'Employee'
#         WHEN dataset_indentifier_code IN ('YCBQ', 'YCBT') THEN 'Self-employed'
#         WHEN dataset_indentifier_code = 'MGRT'            THEN 'Unpaid Family workers'
#         WHEN dataset_indentifier_code = 'MGRW'            THEN 'Government supported training & employment programmes'
#         ELSE NULL
#       END AS employment_type,
# 
#       /* 2) contract_type */
#       CASE
#         WHEN dataset_indentifier_code IN ('YCBK', 'YCBQ') THEN 'Full-time'
#         WHEN dataset_indentifier_code IN ('YCBN', 'YCBT') THEN 'Part-time'
#         WHEN dataset_indentifier_code = 'MGRT'            THEN 'Unpaid Family workers'
#         WHEN dataset_indentifier_code = 'MGRW'            THEN 'Government supported training & employment programmes'
#         ELSE NULL
#       END AS contract_type,
# 
#       employment_subgroup,
#       value 
#     FROM ons.labour_market__employment_group
#     WHERE {where_clauses}
#   ",
#     period_expr   = period_expr,
#     where_clauses = where_clauses,
#     .con          = APP_DB$pool
#   )
# 
#   data <- DBI::dbGetQuery(APP_DB$pool, sql)
# 
#   list(
#     data = tibble::as_tibble(data),
#     sql  = as.character(sql)
#   )
# }
# 
# 
# #--- GENERIC HELPERS (TO BE MOVED TO SEPERATE R SCRIPT) ----
# 
# # Generic: fetch distinct values from {schema}.{table}.{column}
# # Adds optional pre-filter: where_col IN (where_vals)
# fetch_distinct_values <- function(
#   schema,
#   table,
#   column,
#   where_col  = NULL,   # optional: name of a column to pre-filter on
#   where_vals = NULL    # optional: vector of values to keep
# ) {
#   # Normalise filter values (NULL/empty => no filter)
#   if (!is.null(where_vals)) {
#     where_vals <- unlist(where_vals, use.names = FALSE)
#     where_vals <- where_vals[!is.na(where_vals)]
#     if (!length(where_vals)) where_vals <- NULL
#   }
# 
#   # Build optional AND ... IN (...) clause
#   filter_sql <- if (!is.null(where_col) && !is.null(where_vals)) {
#     glue::glue_sql(
#       " AND {`where_col`} IN ({vals*})",
#       where_col = where_col,
#       vals      = where_vals,
#       .con      = APP_DB$pool
#     )
#   } else {
#     DBI::SQL("")
#   }
# 
#   sql <- glue::glue_sql("
#     SELECT DISTINCT {`column`} AS value
#     FROM {`schema`}.{`table`}
#     WHERE {`column`} IS NOT NULL
#     {filter_sql}
#     ORDER BY {`column`}
#   ",
#     schema     = schema,
#     table      = table,
#     column     = column,
#     filter_sql = filter_sql,
#     .con       = APP_DB$pool
#   )
# 
#   DBI::dbGetQuery(APP_DB$pool, sql)[["value"]]
# }
# 
# # Minimal: bounds via parsed date, using slice parser when not castable
# # - period_is_castable = TRUE  -> CAST(date_col AS DATE)
# # - period_is_castable = FALSE -> slice parser:
# #     normalize spaces + dashes, take part after '-', trim, first 8 chars, initcap -> 'Mon YYYY'
# #     then to_date('Mon YYYY')::date
# fetch_date_bounds <- function(
#   schema,
#   table,
#   dataset_id_codes,
#   date_col = "time_period",
#   period_is_castable = TRUE
# ) {
#   # Build period expression per mode
#   period_expr <- if (isTRUE(period_is_castable)) {
#     glue::glue_sql(
#       "CAST({`date_col`} AS DATE)",
#       date_col = date_col, .con = APP_DB$pool
#     )
#   } else {
#     # Slice parser: 8 chars after '-', normalized, then parse as 'Mon YYYY'
#     glue::glue_sql("
#       to_date(
#         initcap(substr(
#           btrim(split_part(
#             regexp_replace(
#               btrim(regexp_replace({`date_col`}::text, '\\\\s+', ' ', 'g')),
#               '[–—]', '-', 'g'
#             ),
#             '-', 2
#           )),
#           1, 8
#         )),
#         'Mon YYYY'
#       )::date
#     ", date_col = date_col, .con = APP_DB$pool)
#   }
# 
#   # Compute MIN/MAX from parsed_date, ignoring NULLs
#   sql <- glue::glue_sql("
#     WITH base AS (
#       SELECT {period_expr} AS parsed_date
#       FROM {`schema`}.{`table`}
#       WHERE dataset_indentifier_code IN ({codes*})
#     )
#     SELECT
#       MIN(parsed_date) AS min_date,
#       MAX(parsed_date) AS max_date
#     FROM base
#     WHERE parsed_date IS NOT NULL
#   ",
#     schema = schema,
#     table  = table,
#     period_expr = period_expr,
#     codes  = dataset_id_codes,
#     .con   = APP_DB$pool
#   )
# 
#   res <- DBI::dbGetQuery(APP_DB$pool, sql)
# 
#   list(
#     min = as.Date(res$min_date[[1]]),
#     max = as.Date(res$max_date[[1]])
#   )
# }
# 
# # Build a named vector for the picker: names = pretty, values = raw
# build_sector_choices_from_map <- function(raw_sectors, pretty_map = awe_default_sector_map()) {
#   raw <- trimws(raw_sectors)
#   names(pretty_map) <- trimws(names(pretty_map))
#   pretty <- dplyr::coalesce(unname(pretty_map[raw]), raw)
#   stats::setNames(raw, pretty)
# }
# 
# 
# # --- UNIFIED MAPPING UTILITIES ----
# 
# # Normalise text lightly: trim, collapse whitespace, replace non-breaking space
# .normalise_label <- function(x) {
#   x <- gsub("\u00A0", " ", x, fixed = TRUE) # NBSP -> space
#   x <- gsub("\\s+", " ", trimws(x))
#   x
# }
# 
# # Centralised sector mapping — keys must match raw DB labels post-normalisation
# # Use the exact pretty labels you want in UI/plot (no HTML entities).
# awe_default_sector_map <- function() {
#   c(
#     "Private sector 3 4 5 6"                              = "Private sector",
#     "Public sector 3 4 5 6"                               = "Public sector",
#     "Services, SIC 2007 sections G-S"                     = "Services",
#     "Finance and business services,SIC 2007 sections K-N" = "Finance and Business Services",
#     "Public sector excluding financial services 5 6"      = "Public Sector - excluding financial services",
#     "Construction, SIC 2007 section F"                    = "Construction",
#     "Manufacturing, SIC 2007 section C"                   = "Manufacturing",
#     "Wholesaling, retailing, hotels & restaurants, SIC 2007 sections G & I" =
#       "Wholesaling, retailing, hotels & restaurants"
#   )
# }
# 
# # Build a canonical mapping for ANY categorical variable.
# # Inputs:
# #   raw_values   = character vector of observed values (from DB or distincts)
# #   pretty_map   = named character vector: names(raw) = pretty
# #   normaliser   = function(x) normalise labels (default = .normalise_label)
# #
# # Outputs:
# #   $choices        = pretty-labelled unique choices (canonical)
# #   $index          = raw → pretty → canonical lookup table
# #   $canon_levels   = canonical factor levels
# #   $map_data()     = function(df, var) : apply mapping to a data frame column
# #   $expand()       = function(canonical) : expand canonical → raw values
# #
# build_mapping_index <- function(
#   raw_values,
#   pretty_map,
#   normaliser = .normalise_label
# ) {
#   if (is.null(raw_values) || !length(raw_values)) {
#     return(list(
#       choices      = character(0),
#       index        = data.frame(raw=character(0), pretty=character(0), canonical=character(0)),
#       canon_levels = character(0),
#       map_data     = function(df, var) df,
#       expand       = function(x) NULL
#     ))
#   }
# 
#   # 1) Normalise raw values
#   norm_raw <- normaliser(as.character(raw_values))
# 
#   # 2) Normalise map keys and apply mapping (fallback = raw)
#   map_norm <- pretty_map
#   names(map_norm) <- normaliser(names(map_norm))
#   pretty <- unname(map_norm[norm_raw])
#   pretty[is.na(pretty)] <- norm_raw[is.na(pretty)]
# 
#   # 3) Canonical = pretty
#   canonical    <- pretty
#   canon_levels <- unique(canonical)
# 
#   # 4) Build lookup index
#   index_df <- data.frame(
#     raw       = norm_raw,
#     pretty    = pretty,
#     canonical = canonical,
#     stringsAsFactors = FALSE
#   )
# 
#   # 5) Picker choices (1:1: names = pretty; values = canonical)
#   choices <- stats::setNames(canon_levels, canon_levels)
# 
#   # 6) Function to expand canonical → raw
#   expand_fun <- function(selected_canonical) {
#     if (is.null(selected_canonical) || !length(selected_canonical)) return(NULL)
#     sel <- unique(normaliser(as.character(selected_canonical)))
#     unique(index_df$raw[index_df$canonical %in% sel])
#   }
# 
#   # 7) Function to apply mapping to a data frame’s column
#   map_data_fun <- function(df, var) {
#     df[[var]] <- normaliser(df[[var]])
#     mapped    <- unname(map_norm[df[[var]]])
#     df[[var]] <- dplyr::coalesce(mapped, df[[var]])
#     df
#   }
# 
#   list(
#     choices      = choices,
#     index        = index_df,
#     canon_levels = canon_levels,
#     expand       = expand_fun,
#     map_data     = map_data_fun
#   )
# }
# 
# 
