## TOTAL EMPLOYED KPI UI ## -----
employment_employ_kpi_card_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("card"))     # this renders the KPI card UI
  )
}

## TOTAL EMPLOYED KPI SERVER ## -----
employment_employ_kpi_card_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$card <- build_kpi_card(
      id              = session$ns("employ"),
      title           = "Total Employed",
      schema          = "ons",
      table           = "labour_market__age_group",
      filter_col      = "dataset_identifier_code",
      filter_value    = "LF2G",
      parse_mode      = "MMM-MMM YYYY",
      good_if_increase = TRUE,
      format_headline = govuk_format_millions_10k,
      format_delta    = function(x) govuk_format_number_signed(x, digits = -3)
    )
  })
}
