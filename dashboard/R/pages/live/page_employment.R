
# R/pages/page_employment.R


# ---- Employment page ----

library(ggplot2)
library(scales)
library(plotly)

# ROUTER PAGE ----------
employment_ui <- function(id) {
  ns <- NS(id)

  toc_sections <- list(
    list(
      heading = "General",
      items = c("Average Weekly Earnings"      = "section-awe",
                "Employment by type"           = "section-ftpt",
                "Employment by age group"      = "section-age",
                "Workforce Jobs by Industry"   = "section-workforce-industry")
    )
    )

  tagList(
    # Load in the SideBar Nav
    side_nav(ns, sections = toc_sections, title = "On this page"),
    #Set Up Page
    div(class = "govuk-width-container",
        tags$main(class = "govuk-main-wrapper",
                  tags$h1(class = "govuk-heading-xl", "Employment"),
                  tags$h1(class = "mb-0 text-inherit", "General"),
#===== Stats Cards =====
                                       div(class = "govuk-grid-row",
                    employment_employ_kpi_card_ui(ns("employ_card")),
                    employment_employ_rate_kpi_card_ui(ns("employ_rate_card")),
                    employment_awe_totalpay_kpi_card_ui(ns("awe_totalpay_card"))
                                       ),

#===== Overview =====
                  div(class = "govuk-grid-row",
                    div(class = "govuk-grid-column-full",
                    tags$section(id = "section-awe",
                    employment_awe_stats_card_ui(ns("awe_card"))
                    ))),
#===== Section A =====
                  div(class = "govuk-grid-row",
                    div(class = "govuk-grid-column-full",
                    tags$section(id = "section-ftpt",
                    employment_ftpt_stats_card_ui(ns("ftpt_card"))
                    ))),
#===== Section B =====
                  div(class = "govuk-grid-row",
                    div(class = "govuk-grid-column-full",
                    tags$section(id = "section-age",
                    employment_age_stats_card_ui(ns("age_trend_card"))
                    ))),
#====== Workforce Jobs by Industry (block chart) ======
                  div(class = "govuk-grid-row",
                    div(class = "govuk-grid-column-full",
                    tags$section(id = "section-workforce-industry",
                    employment_workforce_industry_ui(ns("workforce_industry_card"))
                    )))
)))
}

employment_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #KPI Cards
    employment_employ_kpi_card_server("employ_card")
    employment_employ_rate_kpi_card_server("employ_rate_card")
    employment_awe_totalpay_kpi_card_server("awe_totalpay_card")

    #Stats Cards
    mod_govuk_data_vis_card_server("trend_card")
    employment_awe_stats_card_server("awe_card")
    employment_ftpt_stats_card_server("ftpt_card")
    employment_age_stats_card_server("age_trend_card")
    employment_workforce_industry_server("workforce_industry_card")

  })
}
