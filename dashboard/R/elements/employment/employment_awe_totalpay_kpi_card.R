## AWE TOTAL PAY KPI UI ## -----
employment_awe_totalpay_kpi_card_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("card"))
  )
}

## AWE TOTAL PAY KPI SERVER ## -----
employment_awe_totalpay_kpi_card_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$card <- build_kpi_card(
      id              = session$ns("awe_totalpay"),
      title           = "Average Weekly Earnings (Total Pay)",
      schema          = "ons",
      table           = "labour_market__weekly_earnings_total",
      filter_col      = "dataset_identifier_code",
      filter_value    = "KAB9",
      parse_mode      = "MMM YYYY",
      good_if_increase = TRUE,
      format_headline  = govuk_format_gbp,
      format_delta     = govuk_format_gbp_signed
    )
  })
}