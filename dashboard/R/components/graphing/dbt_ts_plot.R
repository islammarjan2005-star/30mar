
# =============================================================================
# Register supported chart types (current)
# =============================================================================
dbt_register_chart("line",         dbt_build_line)
dbt_register_chart("stacked_area", dbt_build_stacked_area)
dbt_register_chart("stacked_bar",  dbt_build_stacked_bar)

# (Future) You can enable once ready:
# dbt_register_chart("area",    dbt_build_area)
# dbt_register_chart("heatmap", dbt_build_heatmap)


# =============================================================================
# Main function: dbt_ts_plot (dispatcher-based)
# =============================================================================
dbt_ts_plot <- function(
  df,
  df2 = NULL,
  x_title = "Time period (YYYY‑MM)",
  y_title = "Value",
  palette = dbt_palettes$gaf,
  group_order = NULL,
  initial_legend_mode = c("hidden","overlay","export"),
  y_zero = TRUE,
  y_pad = 0.05,
  line_width = 3,
  # column mapping for primary dataset
  value_col = "value",
  group_col = "sector",
  time_col  = "time_period",
  # column mapping for second dataset (defaults to primary mappings if NULL)
  value_col2 = NULL,
  group_col2 = NULL,
  time_col2  = NULL,
  # legend labels & line dashes per dataset
  dataset1_name  = NULL,
  dataset2_name  = NULL,
  line_dash1     = "solid",
  line_dash2     = "dash",
  # legend grouping titles (shown in legend)
  dataset1_group = NULL,
  dataset2_group = NULL,
  # reference lines (vertical on time axis, horizontal on value axis)
  vlines       = NULL,
  vline_labels = NULL,
  vline_color  = "rgba(0,0,0,0.3)", # ONS light grey for vertical lines
  vline_width  = 1.5,
  vline_dash   = "dot",
  hlines       = NULL,
  hline_labels = NULL,
  hline_color  = "#444444",
  hline_width  = 1.5,
  hline_dash   = "dot",
  # Y-axis label wrapping controls
  y_title_wrap       = TRUE,
  y_title_wrap_width = 15L,
  # chart type toggle (keep current options for backward compatibility)
  chart_type = c("line","stacked_area","stacked_bar"),
  # ---- Bar options ----
  bar_interval = c("daily","weekly","monthly","quarterly","annually","5year","decade"),
  bar_agg      = c("sum","mean","last"),
  # ---- Formatting controls ----
  y_tickformat = ",.0f",
  y_prefix = "",
  y_suffix = "",
  # Gap handling for lines
  connect_gaps = FALSE
) {
  initial_legend_mode <- match.arg(initial_legend_mode)
  chart_type <- match.arg(chart_type)
  bar_interval <- match.arg(bar_interval)
  bar_agg <- match.arg(bar_agg)

  # --- ONS/DBT style tokens ---
  ons_text_grey   <- "rgba(0,0,0,0.45)"   # axis titles, x ticks, legend, vline labels
  ons_axis_grey   <- "rgba(0,0,0,0.30)"   # axis lines and tick marks
  black           <- "#000000"            # y tick values
  axis_title_size <- 11

  # --- vline label overlap controls ---
  min_gap_days  <- 120L
  stack_step_px <- 16L
  
  # --- Reference label defaults (applied to **hline** labels only) ---
  REF_LABEL_WRAP_WIDTH  <- 16L
  REF_LABEL_BG          <- "rgba(255,255,255,0.85)"
  REF_LABEL_BORDERCOLOR <- "rgba(0,0,0,0.1)"
  REF_LABEL_BORDERWIDTH <- 0L
  REF_LABEL_BORDERPAD   <- 1L

  # --- Fixed right padding (in pixels) for hline labels & docked-right legend ---
  RIGHT_PAD_PX <- 16L
  LABEL_INNER_OFFSET_PX <- 6L  # xshift used to nudge label slightly into margin

  shapes <- list()
  annots <- list()
  extra_right_margin_px <- 0L   # will grow if any hline label exists

  stopifnot(is.data.frame(df))
  stopifnot(all(c(value_col, group_col, time_col) %in% names(df)))
  if (!is.null(df2)) stopifnot(is.data.frame(df2))

  # --- Resolve mappings for df2 (fallback to primary if not provided) ---
  if (!is.null(df2)) {
    value_col2 <- if (is.null(value_col2)) value_col else value_col2
    group_col2 <- if (is.null(group_col2)) group_col else group_col2
    time_col2  <- if (is.null(time_col2))  time_col  else time_col2
    if (!all(c(value_col2, group_col2, time_col2) %in% names(df2))) {
      stop("df2 is missing one or more of the mapped columns: ",
           paste(c(value_col2, group_col2, time_col2), collapse = ", "))
    }
  }

  # --- Standardize primary data ---
  df1 <- dplyr::rename(
    df,
    value       = !!rlang::sym(value_col),
    group       = !!rlang::sym(group_col),
    time_period = !!rlang::sym(time_col)
  )
  if (is.factor(df1$time_period)) df1$time_period <- as.character(df1$time_period)
  if (!inherits(df1$time_period, "Date")) df1$time_period <- as.Date(df1$time_period)
  if (!is.numeric(df1$value)) df1$value <- suppressWarnings(as.numeric(df1$value))
  if (!is.null(group_order)) df1$group <- factor(df1$group, levels = group_order)

  # --- Drop NA early ---
  df1 <- df1 |>
    dplyr::filter(!is.na(time_period), !is.na(value), !is.na(group))

  # --- Aggregate duplicates (sum within same time_period, group) ---
  df1 <- df1 |>
    dplyr::group_by(time_period, group) |>
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(time_period, group)

  # --- Standardize comparison data ---
  df2_std <- NULL
  if (!is.null(df2)) {
    df2_std <- dplyr::rename(
      df2,
      value       = !!rlang::sym(value_col2),
      group       = !!rlang::sym(group_col2),
      time_period = !!rlang::sym(time_col2)
    )
    if (is.factor(df2_std$time_period)) df2_std$time_period <- as.character(df2_std$time_period)
    if (!inherits(df2_std$time_period, "Date")) df2_std$time_period <- as.Date(df2_std$time_period)
    if (!is.numeric(df2_std$value)) df2_std$value <- suppressWarnings(as.numeric(df2_std$value))
    if (!is.null(group_order)) df2_std$group <- factor(df2_std$group, levels = group_order)

    df2_std <- df2_std |>
      dplyr::filter(!is.na(time_period), !is.na(value), !is.na(group)) |>
      dplyr::group_by(time_period, group) |>
      dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(time_period, group)
  }

  # --- Palette handling (include "ONS Full") ---
  palette_options <- list(
    "gaf"                 = unname(dbt_palettes$gaf),
    "dbt_default"         = unname(dbt_palettes$dbt$default),
    "dbt_extended"        = unname(dbt_palettes$dbt$extended),
    "dbt_reverse"         = unname(dbt_palettes$dbt$reverse),
    "blues"               = unname(dbt_palettes$blues),
    "corporate"           = unname(dbt_palettes$corporate),
    "sequential_default"  = unname(dbt_palettes$sequential$default),
    "sequential_blue"     = unname(dbt_palettes$sequential$blue),
    "ONS Full"            = c("#f39431", "#206095", "#a8bd3a", "#871a5b", "#f66068",
                              "#05341a", "#27a0cc", "#003c57", "#22d0b6", "#746cb1")
  )
  if (is.character(palette) && length(palette) == 1 && palette %in% names(palette_options)) {
    palette_vec <- palette_options[[palette]]
  } else {
    palette_vec <- unname(palette)
  }

  # --- Group list (defer order if stacked charts & no explicit group_order) ---
  base_groups <- unique(df1$group) |> as.character()
  groups <- base_groups
  n_groups <- length(groups)
  col_vec <- rep(palette_vec, length.out = n_groups); names(col_vec) <- groups

  # --- Financial formatting for hover (3 sig figs for m/bn, thousands whole, sub-1k 1 dp) ---
  .trim_zeros <- function(s) sub("\\.?0+$", "", s)
  .sig3 <- function(x) .trim_zeros(formatC(signif(x, 3), format = "fg", digits = 3))
  fmt_financial3 <- function(x, prefix = "", suffix = "") {
    if (is.na(x)) return(NA_character_)
    ax <- abs(x); sgn <- if (x < 0) "-" else ""
    if (ax >= 1e9)      paste0(sgn, prefix, .sig3(ax/1e9), "bn", suffix)
    else if (ax >= 1e6) paste0(sgn, prefix, .sig3(ax/1e6), "m",  suffix)
    else if (ax >= 1e3) paste0(sgn, prefix, formatC(ax, big.mark = ",", format = "f", digits = 0), suffix)
    else                paste0(sgn, prefix, formatC(ax, big.mark = ",", format = "f", digits = 1), suffix)
  }

  # --- Precompute hover text for df1
  df_area <- df1
  df_area$.__hover_y__. <- vapply(df_area$value, fmt_financial3, "", prefix = y_prefix, suffix = y_suffix)

  # --- Decide groups and their order
  .latest_by_group <- function(dd) {
    if (nrow(dd) == 0) return(setNames(numeric(0), character(0)))
    last_t <- max(dd$time_period, na.rm = TRUE)
    vals <- dd |>
      dplyr::filter(time_period == last_t) |>
      dplyr::group_by(group) |>
      dplyr::summarise(v = sum(value, na.rm = TRUE), .groups = "drop")
    setNames(vals$v, as.character(vals$group))
  }

  if (!is.null(group_order)) {
    groups <- unique(c(as.character(group_order), setdiff(base_groups, as.character(group_order))))
  } else if (chart_type %in% c("stacked_area","stacked_bar")) {
    latest_vals <- if (chart_type == "stacked_area") {
      .latest_by_group(df_area)
    } else {
      .latest_by_group(df_area)
    }
    latest_vals <- latest_vals[intersect(names(latest_vals), base_groups)]
    missing <- setdiff(base_groups, names(latest_vals))
    if (length(missing)) latest_vals <- c(latest_vals, setNames(rep(0, length(missing)), missing))
    groups <- names(sort(latest_vals, decreasing = TRUE))
  } else {
    groups <- base_groups
  }

  # Colors aligned to ordered groups
  n_groups <- length(groups)
  col_vec <- rep(palette_vec, length.out = n_groups); names(col_vec) <- groups

  # --- Build base plotly and dispatch to builder ---
  plt <- plotly::plot_ly()
  builder <- dbt_get_chart_builder(chart_type)

  build_res <- switch(
    chart_type,
    line = builder(
      plt = plt,
      df1 = df_area,
      df2 = df2_std,
      groups = groups,
      col_vec = col_vec,
      line_width = line_width,
      connect_gaps = connect_gaps,
      dataset1_name = dataset1_name,
      dataset2_name = dataset2_name,
      dataset1_group = dataset1_group,
      dataset2_group = dataset2_group,
      line_dash1 = line_dash1,
      line_dash2 = line_dash2,
      fmt_financial3 = fmt_financial3,
      y_prefix = y_prefix, y_suffix = y_suffix
    ),
    stacked_area = builder(
      plt = plt,
      df1 = df_area,
      groups = groups,
      col_vec = col_vec,
      dataset1_group = dataset1_group
    ),
    stacked_bar = builder(
      plt = plt,
      df1 = df_area,
      groups = groups,
      col_vec = col_vec,
      dataset1_group = dataset1_group,
      bar_interval = bar_interval,
      bar_agg = bar_agg,
      fmt_financial3 = fmt_financial3, y_prefix = y_prefix, y_suffix = y_suffix
    ),
    stop("Unhandled chart_type: ", chart_type)
  )

  plt <- build_res$plt
  indices_for_group <- build_res$indices_for_group

  # --- Apply DBT theme ---
  plt <- theme_dbt_plotly(plt)

  # --- y-axis base config ---
  yaxis_cfg <- list(
    title     = list(text = NULL),
    showline  = FALSE,
    ticks     = "outside",
    tickcolor = ons_axis_grey,
    ticklen   = 4,
    tickfont  = list(color = black),
    zeroline  = FALSE,
    tickformat = y_tickformat
  )

  # --- Robust y range logic (line/stacked_area/stacked_bar) ---
  if (chart_type == "stacked_area") {
    totals <- df_area |>
      dplyr::group_by(time_period) |>
      dplyr::summarise(
        total_pos = sum(pmax(value, 0), na.rm = TRUE),
        total_neg = sum(pmin(value, 0), na.rm = TRUE),
        .groups = "drop"
      )
    top <- suppressWarnings(max(totals$total_pos, na.rm = TRUE))
    bot <- if (isTRUE(y_zero)) 0 else suppressWarnings(min(totals$total_neg, 0, na.rm = TRUE))
    if (!is.finite(top)) top <- 0
    if (!is.finite(bot)) bot <- 0
    span <- max(1e-9, top - bot)
    yaxis_cfg$range <- c(bot - y_pad * span, top + y_pad * span)

  } else if (chart_type == "stacked_bar") {
    totals <- build_res$range_hint$df_total
    top <- suppressWarnings(max(totals$total_pos, na.rm = TRUE))
    bot <- if (isTRUE(y_zero)) 0 else suppressWarnings(min(totals$total_neg, 0, na.rm = TRUE))
    if (!is.finite(top)) top <- 0
    if (!is.finite(bot)) bot <- 0
    span <- max(1e-9, top - bot)
    yaxis_cfg$range <- c(bot - y_pad * span, top + y_pad * span)

  } else {
    y_vals <- df_area$value
    if (!is.null(df2_std)) y_vals <- c(y_vals, df2_std$value)
    ymin <- suppressWarnings(min(y_vals, na.rm = TRUE))
    ymax <- suppressWarnings(max(y_vals, na.rm = TRUE))
    if (!is.finite(ymin)) ymin <- 0
    if (!is.finite(ymax)) ymax <- 0
    if (isTRUE(y_zero)) {
      lo <- min(0, ymin)
      hi <- max(0, ymax)
      span <- max(1e-9, hi - lo)
      yaxis_cfg$range <- c(lo, hi + y_pad * span)
    } else {
      span <- max(1e-9, ymax - ymin)
      yaxis_cfg$range <- c(ymin - y_pad * span, ymax + y_pad * span)
    }
  }

  # --- x-axis (zoom-friendly date tickformatstops) ---
  xaxis_cfg <- list(
    type = "date",
    title = list(text = x_title, font = list(color = ons_text_grey, size = axis_title_size)),
    showline = TRUE, linecolor = ons_axis_grey, linewidth = 1,
    ticks = "outside", tickcolor = ons_axis_grey, ticklen = 4,
    tickfont = list(color = ons_text_grey),
    zeroline = FALSE,
    tickformatstops = list(
      list(dtickrange = list(NULL, 86400000), value = "%Y-%m-%d"), # < 1 day
      list(dtickrange = list(86400000, "M1"), value = "%d %b %Y"), # day to < 1 month
      list(dtickrange = list("M1", "M12"),   value = "%b %Y"),     # month to < 1 year
      list(dtickrange = list("M12", NULL),   value = "%Y")         # >= 1 year
    )
  )

  plt <- plt |>
    plotly::layout(
      title     = list(text = NULL),
      hovermode = "x unified",
      xaxis = c(
        xaxis_cfg,
        list(
          showspikes = TRUE,
          spikemode = "lines",
          spikesnap = "cursor",
          spikedash = "dot",
          spikethickness = 1,
          spikecolor = "rgba(0,0,0,0.5)"
        )
      ),
      yaxis = yaxis_cfg,
      hoverlabel = list(
        bgcolor = "rgba(240,240,240,0.95)",   # ONS light grey box
        font = list(color = "black"),
        bordercolor = "rgba(0,0,0,0.1)"
      ),
      legend = list(groupclick = "toggleitem", font = list(color = ons_text_grey)),
      separators = ",."
    )

  # --- Legend position modes ---
  overlay_layout <- list(
    "legend.orientation" = "v",
    "legend.x" = 0.98, "legend.y" = 0.92,
    "legend.xanchor" = "right", "legend.yanchor" = "top",
    "legend.bgcolor" = "rgba(255,255,255,0.85)",
    "legend.bordercolor" = "rgba(0,0,0,0.2)",
    "legend.borderwidth" = 1
  )
  export_right_layout <- list(
    "legend.orientation" = "v",
    "legend.x" = 1.02, "legend.xanchor" = "left",
    "legend.y" = 1.0,  "legend.yanchor" = "top"
  )
  if (initial_legend_mode == "hidden") {
    plt <- plt |> plotly::layout(showlegend = FALSE)
  } else if (initial_legend_mode == "overlay") {
    plt <- plt |> plotly::layout(c(list(showlegend = TRUE), overlay_layout))
  } else if (initial_legend_mode == "export") {
    plt <- plt |> plotly::layout(c(list(showlegend = TRUE), export_right_layout))
  }

  # --- Helpers for reference lines & annotation text wrapping ---
  parse_dates <- function(x) {
    if (inherits(x, "Date")) return(x)
    out <- try(as.Date(x), silent = TRUE)
    if (inherits(out, "try-error") || any(is.na(out))) {
      stop("Unable to parse one or more 'vlines' dates; please supply as Date or 'YYYY-MM-DD'.")
    }
    out
  }

  wrap_text <- function(txt, width = 15L) {
    if (is.null(txt) || !nzchar(txt)) return(txt)
    if (grepl("\n", txt, fixed = TRUE) || grepl("<br>", txt, fixed = TRUE)) {
      return(gsub("\n", "<br>", txt, fixed = TRUE))
    }
    words <- strsplit(txt, "\\s+")[[1]]
    lines <- character(0)
    cur <- ""
    for (w in words) {
      add <- if (nzchar(cur)) paste(cur, w) else w
      if (nchar(add) <= width) {
        cur <- add
      } else {
        if (nzchar(cur)) lines <- c(lines, cur)
        cur <- w
      }
    }
    if (nzchar(cur)) lines <- c(lines, cur)
    paste(lines, collapse = "<br>")
  }

  # --- Vertical reference lines & labels (stacked to avoid overlap) ---
  if (!is.null(vlines) && length(vlines) > 0) {
    vx <- parse_dates(vlines)
    vlab <- vline_labels; if (is.null(vlab)) vlab <- rep("", length(vx))
    if (length(vlab) == 1L && length(vx) > 1L) vlab <- rep(vlab, length(vx))
    if (length(vlab) != length(vx)) stop("'vline_labels' must be length 1 or match length of 'vlines'.")
    vx_num <- as.numeric(vx)
    ord <- order(vx_num)
    levels <- integer(length(vx))
    if (length(vx) >= 2L) {
      levels[ord[1]] <- 0L
      for (k in 2:length(vx)) {
        levels[ord[k]] <- if ((vx_num[ord[k]] - vx_num[ord[k-1]]) < min_gap_days) {
          levels[ord[k-1]] + 1L
        } else 0L
      }
    }
    for (i in seq_along(vx)) {
      shapes[[length(shapes) + 1L]] <- list(
        type = "line", xref = "x", yref = "paper",
        x0 = vx[i], x1 = vx[i], y0 = 0, y1 = 1,
        line = list(color = vline_color, width = vline_width, dash = vline_dash),
        layer = "above"
      )
      if (!is.null(vlab[i]) && nzchar(vlab[i])) {
        yshift_px <- - (levels[i] * stack_step_px)
        annots[[length(annots) + 1L]] <- list(
          x = vx[i], xref = "x",
          y = 1,     yref = "paper",
          xanchor = "right",
          yanchor = "top",
          text = vlab[i],
          align = "right",
          showarrow = FALSE,
          yshift = yshift_px,
          font = list(color = ons_text_grey, size = axis_title_size)
        )
      }
    }
  }

  # --- Horizontal reference lines (with robust right margin & legend shift) ---
  if (!is.null(hlines) && length(hlines) > 0) {
    hy   <- as.numeric(hlines)
    hlab <- hline_labels; if (is.null(hlab)) hlab <- rep("", length(hy))
    if (length(hlab) == 1L && length(hy) > 1L) hlab <- rep(hlab, length(hy))
    if (length(hlab) != length(hy)) stop("'hline_labels' must be length 1 or match length of 'hlines'.")

    # Measure max required width for any hline label (in px) using wrapped text
    if (any(nzchar(hlab))) {
      char_px <- ceiling(axis_title_size * 0.62) # heuristic char width in px for the current font size
      for (i in seq_along(hy)) {
        if (!nzchar(hlab[i])) next
        htext_tmp <- wrap_text(hlab[i], width = REF_LABEL_WRAP_WIDTH)
        lines <- strsplit(htext_tmp, "<br>", fixed = TRUE)[[1]]
        max_chars <- max(1L, max(nchar(lines, type = "width")))
        # box width: text + small inner padding + border paddings
        label_w_px <- (max_chars * char_px) + 10L + (2L * as.integer(REF_LABEL_BORDERPAD))
        # allow for the xshift we apply to nudge into the margin
        extra_right_margin_px <- max(extra_right_margin_px, label_w_px + LABEL_INNER_OFFSET_PX)
      }
    }

    for (i in seq_along(hy)) {
      # draw the line
      shapes[[length(shapes) + 1L]] <- list(
        type = "line", xref = "paper", yref = "y",
        x0 = 0, x1 = 1, y0 = hy[i], y1 = hy[i],
        line = list(color = hline_color, width = hline_width, dash = hline_dash),
        layer = "above"
      )

      # label: pinned to right edge inside plot area, offset a bit into the margin
      if (!is.null(hlab[i]) && nzchar(hlab[i])) {
        htext <- wrap_text(hlab[i], width = REF_LABEL_WRAP_WIDTH)
        annots[[length(annots) + 1L]] <- list(
          x = 1, xref = "paper", y = hy[i], yref = "y",
          xanchor = "left", yanchor = "middle",
          xshift = LABEL_INNER_OFFSET_PX,
          text = htext, showarrow = FALSE, align = "left",
          font = list(color = ons_text_grey, size = axis_title_size),
          bgcolor = REF_LABEL_BG,
          bordercolor = REF_LABEL_BORDERCOLOR,
          borderwidth = REF_LABEL_BORDERWIDTH,
          borderpad = REF_LABEL_BORDERPAD,
          cliponaxis = FALSE
        )
      }
    }
  }

  # --- Y-axis title as top-left annotation (left of axis), wrapped ---
  if (!is.null(y_title) && nzchar(y_title)) {
    y_text <- if (grepl("\n|<br>", y_title)) {
      gsub("\\n", "<br>", y_title)
    } else if (isTRUE(y_title_wrap)) {
      wrap_text(y_title, width = y_title_wrap_width)
    } else {
      y_title
    }
    annots[[length(annots) + 1L]] <- list(
      x = -0.03, xref = "paper",
      y = 1.015, yref = "paper",
      xanchor = "left",
      yanchor = "bottom",
      text = y_text,
      showarrow = FALSE,
      align = "left",
      font = list(color = ons_text_grey, size = axis_title_size),
      borderpad = 0,
      cliponaxis = FALSE
    )
  }
  
  # --- Extra annotations coming from the chart builder (e.g., stacked-bar totals) ---
  if (!is.null(build_res$extra_annotations) && length(build_res$extra_annotations)) {
    annots <- c(annots, build_res$extra_annotations)
  }

  base_margin <- list(t = 60)
  if (extra_right_margin_px > 0L) {
    base_margin$r <- max(0L, extra_right_margin_px)
  }
  plt <- plotly::layout(
    plt,
    shapes = shapes,
    annotations = annots,
    margin = base_margin
  )

  # --- Keep a fixed RIGHT_PAD_PX gap between plot edge and right-docked legend (export posture)
  # Shift the legend by the **measured** label width (extra_right_margin_px) + small fixed pad.
  if (extra_right_margin_px > 0L) {
    plt <- htmlwidgets::onRender(plt, sprintf("
      function(el){
        var EXTRA_PX = %d;   // measured label width (px)
        var PAD_PX   = %d;   // small safety pad (px)
        var gd = document.getElementById(el.id);
        if(!gd) return;

        function placeLegend(){
          try{
            var fl = gd._fullLayout;
            if (!fl || !gd.layout.showlegend) return;
            var L = fl.legend;
            if (!L) return;

            // Only adjust when legend is docked-right (xanchor='left' and x >= 1)
            if (L.xanchor !== 'left' || (L.x || 0) < 1) return;

            var plotW = (fl._size && fl._size.w) ? fl._size.w : 0;
            if (plotW <= 0) return;

            var dx = (EXTRA_PX + PAD_PX) / plotW;  // convert pixels -> paper units
            var target = 1 + dx;
            if (Math.abs((L.x || 0) - target) > 1e-4) {
              Plotly.relayout(gd, {'legend.x': target});
            }
          } catch(e) {}
        }

        gd.on('plotly_afterplot', placeLegend);
        gd.on('plotly_relayout',  placeLegend);
        setTimeout(placeLegend, 0);
      }
    ", as.integer(extra_right_margin_px), as.integer(RIGHT_PAD_PX)))
  }

  # If the initial legend mode is 'export' and we needed right margin for hline labels,
  # a small initial nudge; the onRender code above will compute the exact final position.
  if (identical(initial_legend_mode, "export") && extra_right_margin_px > 0L) {
    plt <- plotly::layout(plt, legend = list(x = 1.06, xanchor = "left"))
  }

  # --- Palette dropdown (line / stacked_area / stacked_bar) ---
  all_indices <- unlist(indices_for_group, use.names = FALSE)
  if (length(all_indices) > 0) {
    palette_buttons <- lapply(names(palette_options), function(nm) {
      pal <- palette_options[[nm]]
      group_cols <- unname(rep(pal, length.out = n_groups))
      if (chart_type == "line") {
        cols_for_indices <- unlist(mapply(function(gc, idxs) rep(gc, length(idxs)),
                                          gc = group_cols, idxs = indices_for_group,
                                          SIMPLIFY = FALSE, USE.NAMES = FALSE))
        list(method = "restyle",
             args = list(list("line.color" = cols_for_indices), as.list(all_indices)),
             label = nm)
      } else if (chart_type == "stacked_area") {
        fill_for_indices <- unlist(mapply(function(gc, idxs) rep(gc, length(idxs)),
                                          gc = group_cols, idxs = indices_for_group,
                                          SIMPLIFY = FALSE, USE.NAMES = FALSE))
        line_cols_for_indices <- fill_for_indices
        list(method = "restyle",
             args = list(list("fillcolor" = fill_for_indices,
                              "line.color" = line_cols_for_indices),
                         as.list(all_indices)),
             label = nm)
      } else {
        cols_for_indices <- unlist(mapply(function(gc, idxs) rep(gc, length(idxs)),
                                          gc = group_cols, idxs = indices_for_group,
                                          SIMPLIFY = FALSE, USE.NAMES = FALSE))
        list(method = "restyle",
             args = list(list("marker.color" = cols_for_indices), as.list(all_indices)),
             label = nm)
      }
    })
    plt <- plt |>
      plotly::layout(
        updatemenus = list(
          list(
            type = "dropdown",
            direction = "down",
            x = 0, y = 1.18, xanchor = "left", yanchor = "top",
            buttons = palette_buttons,
            showactive = TRUE,
            bgcolor = "white",
            bordercolor = "rgba(0,0,0,0.2)",
            visible = FALSE
          )
        )
      )
  }

  # --- Modebar icons & handlers (legend/palette only) ---
  icon_overlay <- htmlwidgets::JS(
    "{ width:1000, height:1000, ascent:1000, descent:0,
       path: [
         'M140 290 L200 290 L200 370 L140 370 Z',
         'M260 290 L320 290 L320 370 L260 370 Z',
         'M380 290 L440 290 L440 370 L380 370 Z',
         'M500 290 L560 290 L560 370 L500 370 Z',
         'M620 290 L680 290 L680 370 L620 370 Z',
         'M740 290 L800 290 L800 370 L740 370 Z',
         'M140 460 L200 460 L200 540 L140 540 Z',
         'M260 460 L320 460 L320 540 L260 540 Z',
         'M380 460 L440 460 L440 540 L380 540 Z',
         'M500 460 L560 460 L560 540 L500 540 Z',
         'M620 460 L680 460 L680 540 L620 540 Z',
         'M740 460 L800 460 L800 540 L740 540 Z',
         'M140 630 L200 630 L200 710 L140 710 Z',
         'M260 630 L320 630 L320 710 L260 710 Z',
         'M380 630 L440 630 L440 710 L380 710 Z',
         'M500 630 L560 630 L560 710 L500 710 Z',
         'M620 630 L680 630 L680 710 L620 710 Z',
         'M740 630 L800 630 L800 710 L740 710 Z'
       ].join(' ')
     }"
  )
  icon_export <- htmlwidgets::JS(
    "{ width:1000, height:1000, ascent:1000, descent:0,
       path: [
         'M120 260 L150 260 L150 760 L120 760 Z',
         'M160 300 L560 300 L560 380 L160 380 Z',
         'M160 500 L760 500 L760 580 L160 580 Z',
         'M160 700 L920 700 L920 780 L160 780 Z'
       ].join(' ')
     }"
  )
  icon_drop <- htmlwidgets::JS(
    "{ width:1000, height:1000, ascent:1000, descent:0,
       path:
         'M500 937.5 ' +
         'C500 937.5 203.125 625 203.125 406.25 ' +
         'C203.125 242.1875 335.9375 109.375 500 109.375 ' +
         'C664.0625 109.375 796.875 242.1875 796.875 406.25 ' +
         'C796.875 625 500 937.5 500 937.5 Z'
     }"
  )
  overlay_js <- htmlwidgets::JS("
    function(gd){
      var isVisible = !!gd.layout.showlegend;
      if(!isVisible || gd._legendMode !== 'overlay'){
        Plotly.relayout(gd, {
          'showlegend': true,
          'legend.orientation': 'v',
          'legend.x': 0.98, 'legend.y': 0.92,
          'legend.xanchor': 'right', 'legend.yanchor': 'top',
          'legend.bgcolor': 'rgba(255,255,255,0.85)',
          'legend.bordercolor': 'rgba(0,0,0,0.2)',
          'legend.borderwidth': 1
        });
        gd._legendMode = 'overlay';
      } else {
        Plotly.relayout(gd, {'showlegend': false});
        gd._legendMode = 'hidden';
      }
    }
  ")
  export_js <- htmlwidgets::JS("
    function(gd){
      var isVisible = !!gd.layout.showlegend;
      if(isVisible && gd._legendMode === 'export'){
        Plotly.relayout(gd, {'showlegend': false});
        gd._legendMode = 'hidden';
        return;
      }
      Plotly.relayout(gd, {
        'showlegend': true,
        'legend.orientation': 'v',
        'legend.x': 1.06, 'legend.xanchor': 'left',
        'legend.y': 1.0,  'legend.yanchor': 'top',
        'legend.bgcolor': null, 'legend.bordercolor': null, 'legend.borderwidth': null
      });
      gd._legendMode = 'export';
    }
  ")
  palette_toggle_js <- htmlwidgets::JS("
    function(gd){
      try{
        var cur = (gd.layout.updatemenus && gd.layout.updatemenus[0] && gd.layout.updatemenus[0].visible) ? gd.layout.updatemenus[0].visible : false;
        var next = !(cur === true);
        var obj = {};
        obj['updatemenus[0].visible'] = next;
        Plotly.relayout(gd, obj);
      }catch(e){ console.warn('Unable to toggle palette dropdown:', e); }
    }
  ")

  plt <- plotly::config(
    plt,
    displaylogo = FALSE,
    modeBarButtonsToAdd = list(
      list(name = "Overlay legend", title = "Toggle overlay legend", click = overlay_js, icon = icon_overlay),
      list(name = "Legend right",   title = "Dock legend right",     click = export_js,  icon = icon_export),
      list(name = "Palette",        title = "Show/hide palette menu", click = palette_toggle_js, icon = icon_drop)
    )
  )

  # --- JS onRender: y-axis ticks follow the same financial logic as hover ---
  plt <- htmlwidgets::onRender(plt, "
    function(el, x){
      var gd = document.getElementById(el.id);
      if(!gd) return;

      function postprocess() {
        try{
          var nodes = el.querySelectorAll('.yaxislayer-above .ytick text');
          nodes.forEach(function(t){
            var s = t.textContent || '';
            if (s.indexOf('G') >= 0) t.textContent = s.replace('G', 'bn');
            else if (s.indexOf('M') >= 0) t.textContent = s.replace('M', 'm');
          });
        }catch(e){}
      }

      var applying = false;
      function applyTickFormat(){
        if(!gd || !gd._fullLayout || !gd._fullLayout.yaxis) return;
        var ya = gd._fullLayout.yaxis;
        var r = ya.range || [0, 1];
        var maxAbs = Math.max(Math.abs(r[0]), Math.abs(r[1]));
        var fmt;
        if (maxAbs < 1000)      fmt = ',.1f';
        else if (maxAbs < 1e6)  fmt = ',.0f';
        else                    fmt = '.3~s';
        if (ya.tickformat === fmt) { postprocess(); return; }
        if (applying) return;
        applying = true;
        Plotly.relayout(gd, {'yaxis.tickformat': fmt}).then(function(){
          applying = false;
          postprocess();
        }).catch(function(){ applying = false; });
      }

      gd.on('plotly_afterplot', applyTickFormat);
      gd.on('plotly_relayout',  applyTickFormat);
      applyTickFormat();
    }
  ")

  return(plt)
}
