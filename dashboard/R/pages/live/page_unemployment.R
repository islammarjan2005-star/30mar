
# R/pages/page_unemployment.R

# ---- Unemployment page ----

library(ggplot2)
library(scales)
library(plotly)

# ROUTER PAGE ----------
unemployment_ui <- function(id) {
  ns <- NS(id)

  toc_sections <- list(
    list(
      heading = "General",
      items = c("Unemployment by age group" = "section-unemp-age")
    )
  )

  tagList(
    side_nav(ns, sections = toc_sections, title = "On this page"),
    div(class = "govuk-width-container",
        tags$main(class = "govuk-main-wrapper",
                  tags$h1(class = "govuk-heading-xl", "Unemployment"),
                  tags$h1(class = "mb-0 text-inherit", "General"),

#===== Unemployment by Age =====
                  div(class = "govuk-grid-row",
                    div(class = "govuk-grid-column-full",
                    tags$section(id = "section-unemp-age",
                    unemployment_age_stats_card_ui(ns("unemp_age_card"))
                    )))
        ))
  )
}

unemployment_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    unemployment_age_stats_card_server("unemp_age_card")
  })
}
