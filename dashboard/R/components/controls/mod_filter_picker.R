

# Filter Picker Server ---- 
# ---- UI: sector picker -------------------------------------------------------
  mod_filter_picker_ui <- function(
    id,
    label                = "Sector Filtering",
    multiple             = TRUE,
    actions_box          = TRUE,
    live_search          = TRUE,
    virtual_scroll       = 10,
    selected_text_format = "count > 2",
    width                = NULL
  ) {
    ns <- NS(id)
    shinyWidgets::pickerInput(
      inputId = ns("sectors"),
      label   = label,
      choices = NULL,            # populated by the server module below
      multiple = multiple,
      options  = list(
        `actions-box`           = actions_box,
        `live-search`           = live_search,
        `virtual-scroll`        = virtual_scroll,
        `selected-text-format`  = selected_text_format
      ),
      width = width
    )
  }



# ---- Server: sector picker ---------------------------------------------------

mod_filter_picker_server <- function(
  id,
  data_tbl,                  # lazy tbl_sql (or tibble)
  column,                    # string or reactive string
  defaults_pretty   = character(0),
  mapping_provider  = NULL,  # optional custom ordering/label mapping
  normalise_fn      = identity
) {
  moduleServer(id, function(input, output, session) {
    to_reactive <- function(x) if (shiny::is.reactive(x)) x else shiny::reactive(x)
    data_r   <- to_reactive(data_tbl)
    column_r <- to_reactive(column)

    raw_values <- shiny::reactive({
      d   <- data_r()
      col <- column_r()
      validate(shiny::need(is.character(col) && length(col) == 1L && nzchar(col),
                           "Filter column must be a single, non-empty string"))

      if (inherits(d, "tbl_sql")) {
        vals <- d %>%
          dplyr::filter(!is.na(.data[[col]]), .data[[col]] != "") %>%
          dplyr::distinct(.data[[col]]) %>%
          dplyr::arrange(.data[[col]]) %>%
          dplyr::pull(1)  # pulls only the vector
        return(vals %||% character(0))
      }

      # in-memory fallback
      if (!is.data.frame(d) || !col %in% names(d)) return(character(0))
      unique(sort(as.character(stats::na.omit(d[[col]]))))
    })

    mapping <- shiny::reactive({
      rv <- raw_values()
      if (!length(rv)) {
        return(list(
          choices      = stats::setNames(character(0), character(0)),
          canon_levels = character(0),
          expand       = function(x) character(0)
        ))
      }
      if (is.null(mapping_provider)) {
        list(
          choices      = stats::setNames(rv, rv),
          canon_levels = rv,
          expand       = function(x) x
        )
      } else {
        out <- mapping_provider(rv)
        stopifnot(is.list(out), !is.null(out$choices), !is.null(out$canon_levels),
                  !is.null(out$expand), is.function(out$expand))
        out
      }
    })

    shiny::observeEvent(mapping(), {
      m <- mapping()
      current_sel <- input$sectors
      current_sel <- if (is.null(current_sel)) character(0) else normalise_fn(as.character(current_sel))
      valid_sel   <- intersect(current_sel, m$canon_levels)

      if (!length(valid_sel) && length(defaults_pretty)) {
        def_norm  <- normalise_fn(as.character(defaults_pretty))
        valid_sel <- intersect(def_norm, m$canon_levels)
        if (!length(valid_sel)) valid_sel <- NULL
      } else if (!length(valid_sel)) valid_sel <- NULL

      shinyWidgets::updatePickerInput(
        session  = session,
        inputId  = "sectors",
        choices  = m$choices,
        selected = valid_sel
      )
    }, ignoreInit = FALSE)

    selected <- shiny::reactive({
      sel <- input$sectors
      if (is.null(sel) || !length(sel)) NULL else as.character(sel)
    })

    list(
      choices    = shiny::reactive(mapping()$choices),
      selected   = selected,
      expand_raw = shiny::reactive(mapping()$expand)
    )
  })
}
