
#' GOV.UK / UKHSA data-visualisation card (UI-only)
#'
#' Title + hint + optional filter dropdown (regular controls and/or accordion),
#' then tabs with panels for a chart, a table, and a download area.
#' No server logic here; wire outputs in your server module/app.
#'
#' @param id Module id.
#' @param title Card title (shown in header).
#' @param help_text Optional hint paragraph under the title.
#' @param visual_content UI for the visual (e.g., plotOutput, highchartOutput).
#' @param table_content UI for the table (e.g., reactableOutput, DT::dataTableOutput).
#' @param controls Optional list of general controls (e.g., sliderInput, selectInput).
#' @param accordion_controls FALSE or UI tags to render inside a collapsible panel.
#' @param query Optional output id (character) to show in the download tab as verbatim text.
#' @return An `htmltools::tagList`.
#'
#' @examples
#' # ui <- fluidPage(
#' #   mod_govuk_data_vis_card_ui(
#' #     id = "jobs",
#' #     title = "Jobs dashboard",
#' #     help_text = "Explore trends and export data.",
#' #     visual_content = plotOutput("jobs_plot"),
#' #     table_content  = reactable::reactableOutput("jobs_tbl"),
#' #     controls = list(selectInput("geo", "Area", choices = c("UK","England"))),
#' #     accordion_controls = list(dateRangeInput("rng", "Period"))
#' #   )
#' # )
mod_govuk_data_vis_card_ui <- function(
  id,
  title,
  help_text = NULL,
  help_text_source = NULL, 
  help_link = NULL,
  help_link_text = NULL,
  visual_content = NULL,
  table_content = NULL,
  controls = NULL,            # plain controls (shown in dropdown)
  accordion_controls = FALSE, # set to UI tags to enable accordion; FALSE => none
  query = NULL
) {
  ns <- shiny::NS(id)

  # ---- General controls (if any) ----
  controls_resolved <- NULL
  if (!is.null(controls) && length(controls) > 0) {
    controls_resolved <- htmltools::tagList(controls)
  }

  # ---- Optional accordion (closed by default) ----
  accordion_ui     <- NULL
  accordion_assets <- NULL

  has_accordion <-
    !identical(accordion_controls, FALSE) &&
    !is.null(accordion_controls) &&
    length(accordion_controls) > 0

  if (has_accordion) {
    accordion_inner <- htmltools::tagList(accordion_controls)

    accordion_ui <- htmltools::div(
      id = ns("chart-controls-acc"),
      shinyBS::bsCollapse(
        id   = ns("chart_controls_collapse"),
        open = NULL,  # closed initially
        shinyBS::bsCollapsePanel(
          title = "Chart Controls",
          value = "chart-controls",
          accordion_inner
        )
      )
    )

    # Scoped CSS/JS for accordion cosmetics and dropdown behaviour
    accordion_assets <- htmltools::singleton(
      htmltools::tags$head(
        htmltools::tags$style(
          htmltools::HTML(sprintf("
            /* ======================= ACCORDION SCOPE ======================= */
            #%s .panel {
              border: 0 !important; box-shadow: none !important; background: transparent !important;
              margin-bottom: 0.5rem;
            }
            #%s .panel-heading {
              background: transparent !important; border: 0 !important; padding: 8px 0;
            }
            #%s .panel-title a {
              display: block; position: relative; padding-right: 28px;
              text-decoration: none; color: inherit;
              font-size: 20px;
              line-height: 1.3; cursor: pointer;
              user-select: none; -webkit-user-select: none; -ms-user-select: none;
              -webkit-tap-highlight-color: transparent;
              background: transparent !important; outline: none !important; box-shadow: none !important;
            }
            #%s .panel-title a:hover,
            #%s .panel-title a:focus,
            #%s .panel-title a:active,
            #%s .panel-title a:visited {
              text-decoration: none !important; color: inherit !important;
              background: transparent !important; outline: none !important; box-shadow: none !important;
            }
            #%s .panel-title a:focus-visible {
              outline: 2px dotted rgba(0,0,0,0.35); outline-offset: 2px;
            }
            #%s .panel-body {
              border-top: 0 !important; padding: 8px 0 0 0; background: transparent !important;
            }
            /* Chevron using Font Awesome 4 */
            #%s .panel-title a:after {
              content: '\\f078'; font-family: 'FontAwesome';
              position: absolute; right: 0; top: 50%%;
              transform: translateY(-50%%) rotate(0deg); transition: transform .2s ease;
              font-size: 12px; opacity: 0.8;
            }
            #%s .panel-title a[aria-expanded='true']::after {
              transform: translateY(-50%%) rotate(180deg);
            }

            /* ======================= DROPDOWN SCOPE ======================== */
            .lm-card-ukhsa .dropdown-menu {
              padding: 12px 14px;
              font-size: 14px;
              line-height: 1.35;
            }
            .lm-card-ukhsa .dropdown-menu .shiny-input-container > label,
            .lm-card-ukhsa .dropdown-menu .control-label {
              font-size: 14px;
              margin-bottom: 6px;
            }
            .lm-card-ukhsa .dropdown-menu .btn-group .btn {
              font-size: 13px;
            }
            .lm-card-ukhsa .dropdown-menu .btn:focus,
            .lm-card-ukhsa .dropdown-menu .btn:active,
            .lm-card-ukhsa .dropdown-menu .btn:hover {
              box-shadow: none !important; outline: none !important;
            }
          ",
          ns("chart-controls-acc"),
          ns("chart-controls-acc"),
          ns("chart-controls-acc"),
          ns("chart-controls-acc"),
          ns("chart-controls-acc"),
          ns("chart-controls-acc"),
          ns("chart-controls-acc"),
          ns("chart-controls-acc"),
          ns("chart-controls-acc"),
          ns("chart-controls-acc"),
          ns("chart-controls-acc")
          ))
        ),
        # Keep dropdown open when toggling accordion header inside shinyWidgets::dropdown
        htmltools::tags$script(htmltools::HTML("
          $(document).on('click', '.sw-dropdown .dropdown-menu .panel-heading, \
                                 .sw-dropdown .dropdown-menu .panel-title a', function(e){
            e.stopPropagation();
          });
        "))
      )
    )
  }

  # ---- Filters dropdown: combine general controls and accordion (if any) ----
  dropdown_body <- htmltools::tagList(controls_resolved, accordion_ui)

  dropdown_btn <- NULL
  if (length(dropdown_body) > 0) {
    dropdown_btn <- shinyWidgets::dropdown(
      dropdown_body,
      label   = "Filters",
      style   = "material-circle",
      icon    = shiny::icon("sliders-h"),
      status  = "danger",
      size    = "md",
      right   = TRUE,
      tooltip = shinyWidgets::tooltipOptions(title = "Show filters")
    )
  }

  # ---- Card structure: header + tabs + panels ----
  htmltools::tagList(
    # Load accordion assets only when used
    accordion_assets,

    # Asset hook for tab behaviour (provide this helper elsewhere)
    ukhsa_card_tabs_assets(),

    htmltools::tags$div(
      class = "lm-card-ukhsa",

     # Header
htmltools::tags$div(
  class = "ukhsa-card-header",

  htmltools::tags$div(
    class = "ukhsa-card-header__left",

    htmltools::tags$h2(class = "govuk-heading-m", title),

    # Help text (3 lines: description, Source, Link)
    if (!is.null(help_text)) htmltools::tags$p(
      class = "govuk-hint",

      # Line 1
      htmltools::tags$span(help_text),

      # Line 2 (Source)
      if (!is.null(help_text_source)) htmltools::tagList(
        htmltools::tags$br(),
        htmltools::tags$span(help_text_source)
      ),

      # Line 3 (Link)
      if (!is.null(help_link)) htmltools::tagList(
        htmltools::tags$br(),
        htmltools::tags$span("Link: "),
        htmltools::tags$a(
          href   = help_link,                 # e.g. "https://www.google.co.uk"
          target = "_blank",
          rel    = "noopener noreferrer",
          help_link_text
        )
      )
    )
  ),

  htmltools::tags$div(
    class = "ukhsa-card-header__right",
    dropdown_btn
  )
),

      # Tabs + panels
      htmltools::tags$div(
        class = "ukhsa-tabs",

        # Tab buttons
        htmltools::tags$div(
          class = "ukhsa-tabs__list", role = "tablist",
          htmltools::tags$a(
            class = "ukhsa-tabs__tab",
            role = "tab", `aria-selected` = "true", tabindex = "0",
            `data-target` = ns("chart"), "Chart"
          ),
          htmltools::tags$a(
            class = "ukhsa-tabs__tab",
            role = "tab", `aria-selected` = "false", tabindex = "-1",
            `data-target` = ns("table"), "Tabular data"
          ),
          htmltools::tags$a(
            class = "ukhsa-tabs__tab",
            role = "tab", `aria-selected` = "false", tabindex = "-1",
            `data-target` = ns("download"), "Download"
          )
        ),

        # Panels
        htmltools::tags$div(
          id = ns("chart"), class = "ukhsa-tabs__panel", role = "tabpanel",
          if (is.null(visual_content))
            htmltools::tags$p(class = "govuk-hint", "Visual Content placeholder")
          else visual_content
        ),
        htmltools::tags$div(
          id = ns("table"), class = "ukhsa-tabs__panel is-hidden", role = "tabpanel",
          if (is.null(table_content))
            htmltools::tags$p(class = "govuk-hint", "Table placeholder")
          else table_content
        ),
        htmltools::tags$div(
          id = ns("download"), class = "ukhsa-tabs__panel is-hidden", role = "tabpanel",
          if (is.null(query))
            htmltools::tags$p(class = "govuk-hint", "Download placeholder")
          else htmltools::tags$div(class = "code-block", shiny::verbatimTextOutput(query))
        )
      )
    ),
    
   
    htmltools::div(style = "height: 30px;"),


    # Card-level layout tweaks
    htmltools::tags$style(htmltools::HTML("
      .lm-card-ukhsa .ukhsa-card-header {
        display: flex; align-items: flex-start; justify-content: space-between;
        gap: 12px; margin-bottom: 6px;
      }
      .lm-card-ukhsa .ukhsa-card-header__left { min-width: 0; }
      .lm-card-ukhsa .ukhsa-card-header__right { display: flex; gap: 8px; align-items: flex-start; }
      .dropdown-menu { padding: 12px 14px; }
      .dropdown-menu .form-group, .dropdown-menu .sw-control, .dropdown-menu .shiny-input-container { margin-bottom: 10px; }
      @media (max-width: 640px) {
        .lm-card-ukhsa .ukhsa-card-header { flex-direction: column; align-items: stretch; gap: 8px; }
        .lm-card-ukhsa .ukhsa-card-header__right { justify-content: flex-end; }
      }
    "))
  )
}


#' GOV.UK / UKHSA data-visualisation card (server)
#'
#' Minimal server module; triggers client-side initialisation of custom tabs.
#' Extend this to wire your outputs (tables, downloads, etc.).
#'
#' @param id Module id.
#' @return Invisibly initialises tab behaviour (side effects).
#'
#' @examples
#' # server <- function(input, output, session) {
#' #   mod_govuk_data_vis_card_server("jobs")
#' # }
mod_govuk_data_vis_card_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Ensure the tab JS binds after render passes
    session$onFlushed(function() {
      session$sendCustomMessage("ukhsa-tabs-init", list())
    }, once = FALSE)
  })
}
