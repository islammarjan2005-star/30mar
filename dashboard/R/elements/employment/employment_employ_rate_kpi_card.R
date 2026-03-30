## EMPLOYMENT RATE KPI UI ## -----
employment_employ_rate_kpi_card_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("card"))
  )
}

## EMPLOYMENT RATE KPI SERVER ## -----
employment_employ_rate_kpi_card_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$card <- build_kpi_card(
      id              = session$ns("employ_rate"),
      title           = "Employment Rate",
      schema          = "ons",
      table           = "labour_market__age_group",
      filter_col      = "dataset_identifier_code",
      filter_value    = "LF24",
      parse_mode      = "MMM-MMM YYYY",
      good_if_increase = TRUE,
      format_headline = govuk_format_percent1,
      format_delta    = govuk_format_percent1_signed
    )
  })
}
