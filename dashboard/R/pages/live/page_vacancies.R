
# R/pages/page_vacancies.R

# ---- Vacancies page ----

library(ggplot2)
library(scales)
library(plotly)

# ROUTER PAGE ----------
vacancies_ui <- function(id) {
  ns <- NS(id)

  toc_sections <- list(
    list(
      heading = "General",
      items = c("Vacancies by Industry" = "section-vacancies-industry")
    )
  )

  tagList(
    side_nav(ns, sections = toc_sections, title = "On this page"),
    div(class = "govuk-width-container",
        tags$main(class = "govuk-main-wrapper",
                  tags$h1(class = "govuk-heading-xl", "Vacancies"),
                  tags$h1(class = "mb-0 text-inherit", "General"),

#===== Vacancies by Industry =====
                  div(class = "govuk-grid-row",
                    div(class = "govuk-grid-column-full",
                    tags$section(id = "section-vacancies-industry",
                    vacancies_industry_ui(ns("vacancies_industry_card"))
                    )))
        ))
  )
}

vacancies_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    vacancies_industry_server("vacancies_industry_card")
  })
}
