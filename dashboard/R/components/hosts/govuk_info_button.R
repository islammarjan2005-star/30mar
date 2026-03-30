

# Small, self-contained info icon + tooltip (no JS required)
govuk_info_button <- function(
  id,
  text,
  icon = c("i", "?"),
  title = NULL,                 # optional: native browser tooltip on long hover
  corner = c("top-right", "top-left"),  # placement corner inside a relatively-positioned parent
  offset_px = 12,               # spacing from edges
  fg_hex = "#505a5f",           # GOV.UK border/text grey 1
  bg_hex = "rgba(0,0,0,0.05)",  # subtle background circle fill
  tooltip_bg = "#0b0c0c",       # GOV.UK black
  tooltip_fg = "#ffffff"        # tooltip text colour
) {
  icon <- match.arg(icon)
  corner <- match.arg(corner)

  corner_style <- switch(
    corner,
    "top-right" = paste0("top:", offset_px, "px; right:", offset_px, "px;"),
    "top-left"  = paste0("top:", offset_px, "px; left:",  offset_px, "px;")
  )

  htmltools::tags$span(
    class = "govuk-info-wrap",
    style = paste("position:absolute;", corner_style, "z-index:2;"),
    # The "button" – accessible, keyboard focusable
    htmltools::tags$span(
      id = id,
      role = "button",
      tabindex = "0",
      `aria-describedby` = paste0(id, "-tooltip"),
      class = "govuk-info-btn",
      title = if (!is.null(title)) title else NULL,
      style = paste0(
        "display:inline-flex; align-items:center; justify-content:center;",
        "width:22px; height:22px; border-radius:50%;",
        "border:1px solid ", fg_hex, "; color:", fg_hex, ";",
        "background:", bg_hex, ";",
        "font-weight:700; font-family:inherit; font-size:13px; line-height:1;",
        "cursor:default; user-select:none;"
      ),
      icon
    ),
    # Tooltip – revealed on :hover / :focus-within via CSS in DBT_widget_styling()
    htmltools::tags$span(
      id = paste0(id, "-tooltip"),
      role = "tooltip",
      class = "govuk-info-tooltip",
      style = paste0(
        "position:absolute;",
        "min-width:220px; max-width:320px;",
        "padding:10px 12px; border-radius:3px;",
        "background:#0b0c0c; color:#ffffff;",
        "box-shadow:0 4px 12px rgba(0,0,0,0.2);",
        "font-size:14px; line-height:1.4;"
      ),
      text
    )
  )
}
