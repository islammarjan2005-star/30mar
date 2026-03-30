
#' GOV.UK-style statistics summary card
#'
#' Displays a headline value and optional delta tag with a short period label.
#'
#' @param id HTML id for the card.
#' @param title Short title above the headline.
#' @param headline Character or numeric; main value (use `format_headline` for numeric).
#' @param delta Optional character or numeric change (use `format_delta` for numeric).
#' @param period Character label appended after delta (default "vs last month").
#' @param accent_hex Hex colour for the top border (default "#cf102d").
#' @param good_if_increase Logical; TRUE -> positive is good (green).
#' @param tag_colour Optional override: "green"|"red"|"blue".
#' @param width_class GOV.UK grid class for width.
#' @param classes Extra classes on the summary card.
#' @param format_headline Optional function to format numeric `headline`.
#' @param format_delta Optional function to format numeric `delta`.
#' @return An `htmltools` tag `<div>`.
#' @examples
#' govuk_stats_card(
#'   id = "rate",
#'   title = "Unemployment rate",
#'   headline = 4.9,
#'   delta = -0.3,
#'   period = "vs last month",
#'   format_headline = govuk_format_percent1,
#'   format_delta = govuk_format_percent1,
#'   good_if_increase = FALSE
#' )
#' @export
govuk_stats_card <- function(
  id,
  title,
  headline,                  # formatted string OR numeric; main value
  delta = NULL,              # formatted string OR numeric; shown as govuk-tag
  period = "vs last month",  # label after delta
  accent_hex = "#cf102d",    # top border accent (DBT red)
  good_if_increase = TRUE,   # TRUE: increase => green; FALSE: increase => red
  tag_colour = NULL,         # override: 'green'|'red'|'blue'
  width_class = "govuk-grid-column-one-third",
  classes = NULL,            # extra classes on the card
  format_headline = NULL,    # formatter for numeric headline
  format_delta = NULL,       # formatter for numeric delta

  # NEW: info control
  show_info = FALSE,         # default off
  info_text = NULL,          # tooltip content
  info_icon = c("i", "?"),   # corner icon
  info_corner = c("top-right", "top-left")
) {
  info_icon   <- match.arg(info_icon)
  info_corner <- match.arg(info_corner)

  # Headline
  headline_out <- if (!is.null(format_headline)) {
    format_headline(headline)
  } else headline

  # Delta (optional)
  delta_out <- NULL
  if (!is.null(delta)) {
    delta_out <- if (!is.null(format_delta)) format_delta(delta) else delta
  }

  # Tag colour (override > inferred)
  tag_col <- if (!is.null(tag_colour)) tag_colour else .govuk_tag_colour(delta, good_if_increase)
  if (!tag_col %in% c("green", "red", "blue")) tag_col <- "blue"

  htmltools::tags$div(
    class = width_class,
    htmltools::tags$div(
      id    = id,
      class = paste("govuk-summary-card", if (!is.null(classes)) classes else ""),
      style = paste0(
        "position:relative;",  # needed for corner positioning
        "padding:15px; background:#f3f2f1; border-top:4px solid ", accent_hex, ";"
      ),

      # Optional info button in the corner
      if (isTRUE(show_info) && !is.null(info_text) && nzchar(info_text)) {
        govuk_info_button(
          id     = paste0(id, "-info"),
          text   = info_text,
          icon   = info_icon,
          corner = info_corner
        )
      },

      htmltools::tags$h3(
          class = "govuk-heading-s",
          style = "white-space: normal; line-height: 1.2;",
          htmltools::HTML(split_title_at(title, threshold = 25))
        ),
      htmltools::tags$h2(class = "govuk-heading-l", headline_out),
      if (!is.null(delta_out)) htmltools::tags$strong(
        class = paste0("govuk-tag govuk-tag--", tag_col),
        paste(delta_out, period)
      )
    )
  )
}

# Build KPI Card Wrapper 
build_kpi_card <- function(id,
                                title,
                                schema, table,
                                filter_col, filter_value,
                                parse_mode,
                                value_col = "value",
                                good_if_increase = TRUE,
                                format_headline,
                                format_delta,
                                period = "vs last month",
                                info_icon = "i",
                                date_fmt = "%Y-%m-%d") {

  # Use session to namespace id when called inside moduleServer
  ns <- shiny::NS(id)

  shiny::renderUI({
    res <- get_latest_value_eq(
      schema       = schema,
      table        = table,
      date_col     = "time_period",
      filter_col   = filter_col,
      filter_value = filter_value,
      value_col    = value_col,
      mode         = "latest",
      parse_mode   = parse_mode,
      with_tooltip = TRUE,
      date_fmt     = date_fmt
    )

    govuk_stats_card(
      id       = ns("card"),
      title    = title,
      headline = res$value,
      delta    = res$delta,
      period   = period,
      good_if_increase = good_if_increase,
      format_headline  = format_headline,
      format_delta     = format_delta,
      show_info = TRUE,
      info_text = res$tooltip,
      info_icon = info_icon
    )
  })
}
