
### Quick Date Ranges ----

# ---- Quick Date Range Module: UI (calendar/slider shown only for "Custom") ----
mod_quick_date_range_ui <- function(
  id,
  label_quick    = "Quick ranges",
  label_picker   = "Time period",                 # label shown above calendar/slider
  custom_picker  = c("calendar", "slider"),       # <-- new param, not exposed to end users
  presets        = c("custom", "ytd", "past_year", "3y", "5y", "none")  # default removes "latest"
) {
  ns <- NS(id)
  custom_picker <- match.arg(custom_picker)

  # Map *keys* -> labels (we will use keys as the input value)
  preset_labels <- c(
    custom     = "Custom",                         # <-- new "Custom" quick button
    ytd        = "Year to date",
    past_year  = "Past year",
    `3y`       = "3 years",
    `5y`       = "5 years",
    none       = "No filter",
    latest     = "Latest"                          # still supported if you pass it in `presets`
  )

  # Ensure valid non-empty presets in the given order
  presets <- intersect(presets, names(preset_labels))
  if (!length(presets)) presets <- "none"

  # radioGroupButtons returns the *value* of each element in `choices`;
  # using a named vector → names = labels (shown), values = keys (returned).
  choice_names  <- unname(preset_labels[presets])
  choice_values <- presets
  choices_vec   <- stats::setNames(choice_values, choice_names)

  tagList(
    shinyWidgets::radioGroupButtons(
      inputId   = ns("quick_range"),
      label     = label_quick,
      choices   = choices_vec,                     # value is a *key* such as "custom"
      status    = "danger",
      justified = TRUE,
      width     = "100%"
    ),
    # Placeholder for the on-demand picker (calendar OR slider).
    # It is rendered *only* when "Custom" is selected.
    uiOutput(ns("custom_picker"))
  )
}

# ---- Quick Date Range Module: Server (danger styling fixed) ----------------
# bounds_reactive: reactive() returning list(min = Date, max = Date)
# ---- Quick Date Range Module: Server (calendar/slider only for "Custom") ----
# `data_tbl` can be a reactive or a static tbl_sql with a Date column `time_period`.
mod_quick_date_range_server <- function(
  id,
  data_tbl,
  presets            = c("custom", "ytd", "past_year", "3y", "5y", "none"),
  default            = c("5y", "3y", "past_year", "ytd", "custom", "none"),
  frequency          = c("monthly", "quarterly", "daily", "weekly"),
  custom_picker      = c("calendar", "slider"),   # <-- choose the hidden control type
  label_picker       = "Time period",             # <-- label shown above the picker
  preserve_selection = TRUE
) {
  moduleServer(id, function(input, output, session) {
    # ----- Validate args -------------------------------------------------------
    valid_freq <- c("monthly", "quarterly", "daily", "weekly")
    frequency  <- if (length(frequency) == 1 && frequency %in% valid_freq) frequency else "monthly"

    #custom_picker <- match.arg(custom_picker)

    preset_labels <- c(
      custom     = "Custom",
      ytd        = "Year to date",
      past_year  = "Past year",
      `3y`       = "3 years",
      `5y`       = "5 years",
      none       = "No filter",
      latest     = "Latest"  # supported if you allow it in `presets`
    )

    presets <- intersect(presets, names(preset_labels))
    if (!length(presets)) presets <- "none"

    valid_default <- c("5y", "3y", "past_year", "ytd", "custom", "latest", "none")
    default <- if (length(default) == 1 && default %in% valid_default) default else "5y"
    if (!(default %in% presets)) default <- presets[[1]]

    to_reactive <- function(x) if (shiny::is.reactive(x)) x else shiny::reactive(x)
    data_r <- to_reactive(data_tbl)

    # ----- Bounds from DB (no na.rm to avoid pool issues) ---------------------
    bounds <- shiny::reactive({
      d <- data_r(); req(inherits(d, "tbl_sql"))
      b <- suppressWarnings(
        d %>%
        dplyr::summarise(
          min = min(.data$time_period),
          max = max(.data$time_period)
        ) %>%
        dplyr::collect()
      )
      list(min = as.Date(b$min[[1]]), max = as.Date(b$max[[1]]))
    })

    # ----- Helper: date range for a preset, clamped to bounds -----------------
    .range_for_preset <- function(key, b) {
      minD <- b$min; maxD <- b$max
      if (is.null(minD) || is.null(maxD) || anyNA(c(minD, maxD))) return(NULL)
      clamp <- function(d) max(minD, min(d, maxD))
      ytd_start <- as.Date(sprintf("%s-01-01", lubridate::year(maxD)))

      rng <- switch(key,
        latest    = c(maxD, maxD),
        ytd       = c(clamp(ytd_start), maxD),
        past_year = c(clamp(maxD %m-% lubridate::years(1)), maxD),
        `3y`      = c(clamp(maxD %m-% lubridate::years(3)), maxD),
        `5y`      = c(clamp(maxD %m-% lubridate::years(5)), maxD),
        none      = c(minD, maxD),
        custom    = NULL,  # handled by picker
        NULL
      )
      if (is.null(rng)) NULL else as.Date(rng)
    }

    # Helper for slider: build a sequence of "tick" dates by frequency
    .seq_by_freq <- function(minD, maxD, freq) {
      minD <- as.Date(minD); maxD <- as.Date(maxD)
      switch(freq,
        daily     = seq(minD, maxD, by = "1 day"),
        weekly    = seq(lubridate::floor_date(minD, "week", week_start = 1),
                        lubridate::floor_date(maxD, "week", week_start = 1), by = "1 week"),
        quarterly = seq(lubridate::floor_date(minD, "quarter"),
                        lubridate::floor_date(maxD, "quarter"), by = "3 months"),
        monthly   = seq(lubridate::floor_date(minD, "month"),
                        lubridate::floor_date(maxD, "month"), by = "1 month")
      )
    }

    first_bounds_applied <- shiny::reactiveVal(FALSE)

    # This is the single source of truth for the currently effective range
    current_range <- shiny::reactiveVal(NULL)

    # ----- Initialise radio with keys (so input$quick_range returns a key) ----
    shiny::observeEvent(TRUE, {
      choice_names  <- unname(preset_labels[presets])
      choice_values <- presets
      choices_vec   <- stats::setNames(choice_values, choice_names)
      shinyWidgets::updateRadioGroupButtons(
        session, "quick_range",
        choices   = choices_vec,
        selected  = default,
        status    = "danger",
        justified = TRUE
      )
    }, once = TRUE)

    # ----- Apply bounds; set initial current_range from the selected preset ----
    shiny::observeEvent(bounds(), {
      b <- bounds(); if (is.null(b) || anyNA(c(b$min, b$max))) return()

      # Determine starting range based on default selection (or keep preserved)
      if (!isTRUE(first_bounds_applied())) {
        start_key <- default
        rng <- if (identical(start_key, "custom")) .range_for_preset("none", b) else .range_for_preset(start_key, b)
        current_range(rng)
        first_bounds_applied(TRUE)
      } else if (isTRUE(preserve_selection)) {
        key <- input$quick_range
        if (length(key) == 1 && !is.na(key) && key != "custom") {
          current_range(.range_for_preset(key, b))
        }
      }
    }, ignoreInit = FALSE)

    # ----- Render the picker *only* for "custom" --------------------------------
    output$custom_picker <- shiny::renderUI({
      req(bounds())
      if (!identical(input$quick_range, "custom")) return(NULL)

      b   <- bounds()
      rng <- current_range()
      if (is.null(rng) || length(rng) != 2 || anyNA(rng)) rng <- c(b$min, b$max)

      if (identical(custom_picker, "calendar")) {

      shinyWidgets::airDatepickerInput(
          inputId        = session$ns("date_range"),
          label          = label_picker,
          value          = rng,
          range          = TRUE,
          autoClose      = TRUE,
          clearButton    = TRUE,
          separator      = " to ",
          inline         = TRUE,              # shows only while "Custom" is selected
          toggleSelected = FALSE,
          todayButton    = TRUE,
          minDate        = as.Date(b$min),    # <-- move here
          maxDate        = as.Date(b$max),    # <-- move here
          width          = "100%"
        )

      } else {
        # Two-way slider from shinyWidgets (labels are ISO dates)
        ticks <- .seq_by_freq(b$min, b$max, frequency)
        labs  <- format(ticks, "%Y-%m-%d")
        shinyWidgets::sliderTextInput(
          inputId   = session$ns("date_range"),
          label     = label_picker,
          choices   = labs,
          selected  = format(rng, "%Y-%m-%d"),  # length 2 => range mode
          dragRange = TRUE,
          width     = "100%"
        )
      }
    })

    # ----- When user clicks a quick preset (non-custom) -> compute range -------
    shiny::observeEvent(input$quick_range, {
      b <- bounds(); if (is.null(b)) return()
      key <- input$quick_range
      if (length(key) != 1 || is.na(key)) return()

      if (!identical(key, "custom")) {
        rng <- .range_for_preset(key, b)
        current_range(rng)
        shinyWidgets::updateRadioGroupButtons(
          session, "quick_range",
          selected = key,
          status   = "danger"
        )
      }
      # If "custom", we do nothing here — the picker (when rendered) will seed
      # itself from current_range() and any change will be captured below.
    }, ignoreInit = TRUE)

    # ----- Manual edits in the picker (only visible for "custom") --------------
    shiny::observeEvent(input$date_range, {
      req(identical(input$quick_range, "custom"))
      v <- input$date_range
      if (is.null(v) || length(v) != 2) return()

      # Calendar yields Date; slider yields character (ISO). Normalize to Date.
      if (inherits(v, "Date")) {
        rng <- as.Date(v)
      } else {
        rng <- as.Date(v, format = "%Y-%m-%d")
      }

      # Clamp to bounds defensively
      b <- bounds()
      rng <- c(max(b$min, min(rng[1], b$max)), max(b$min, min(rng[2], b$max)))
      current_range(as.Date(rng))
    }, ignoreInit = TRUE)

    # ----- Public API -----------------------------------------------------------
    list(
      date_range = shiny::reactive({
        rng <- current_range()
        if (is.null(rng) || length(rng) != 2 || anyNA(rng)) return(NULL)
        as.Date(rng)
      }),
      selected_quick_range = shiny::reactive({
        key <- input$quick_range
        if (length(key) != 1 || is.na(key)) return(NA_character_)
        key
      }),
      bounds = shiny::reactive(bounds())
    )
  })
}

