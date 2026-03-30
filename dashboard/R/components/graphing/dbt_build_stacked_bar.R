
# ---- STACKED BAR (periodise + aggregate inside) ----
dbt_build_stacked_bar <- function(
  plt, df1, groups, col_vec, dataset1_group,
  bar_interval = c("daily","weekly","monthly","quarterly","annually","5year","decade"),
  bar_agg = c("sum","mean","last"),
  fmt_financial3, y_prefix, y_suffix,
  # --- internal defaults for labels/totals (kept inside the helper) ---
  bar_show_segment_labels = TRUE,   # only inside; tiny segments omitted
  bar_label_min_prop      = 0.08,   # >= 8% of stack height to draw label
  bar_show_totals         = TRUE,   # bold total above each stack
  bar_total_label_color   = "#000000",
  bar_total_label_size    = 12L
) {
  bar_interval <- match.arg(bar_interval)
  bar_agg <- match.arg(bar_agg)

  # ---------- Periodise + aggregate ----------
  dfb <- df1
  dfb$period <- .dbt_period_floor(dfb$time_period, bar_interval)

  if (bar_agg == "sum") {
    dfb <- dfb |>
      dplyr::group_by(period, group) |>
      dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  } else if (bar_agg == "mean") {
    dfb <- dfb |>
      dplyr::group_by(period, group) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  } else { # "last"
    dfb <- dfb |>
      dplyr::arrange(period) |>
      dplyr::group_by(period, group) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::ungroup()
  }

  # ---------- Complete the period grid ----------
  if (nrow(dfb) > 0) {
    by_str <- .dbt_by_string(bar_interval)
    pmin <- min(dfb$period, na.rm = TRUE)
    pmax <- max(dfb$period, na.rm = TRUE)
    seqp <- seq(from = pmin, to = pmax, by = by_str)
    full_grid <- tidyr::expand_grid(period = seqp, group = unique(dfb$group))
    dfb <- full_grid |>
      dplyr::left_join(dfb, by = c("period","group")) |>
      dplyr::mutate(value = tidyr::replace_na(value, 0)) |>
      dplyr::arrange(period, group)
  }

  # ---------- Per-period totals & shares ----------
  totals <- dfb |>
    dplyr::group_by(period) |>
    dplyr::summarise(
      total_pos  = sum(pmax(value, 0), na.rm = TRUE),
      total_neg  = sum(pmin(value, 0), na.rm = TRUE),
      total_sum  = sum(value, na.rm = TRUE),
      .groups = "drop"
    )

  dfb <- dfb |>
    dplyr::left_join(totals, by = "period")

  # Hover text for all segments
  dfb$.__hover_y__. <- vapply(dfb$value, fmt_financial3, "", prefix = y_prefix, suffix = y_suffix)

  # Segment labels shown *only* inside if segment ≥ threshold share of stack
  if (isTRUE(bar_show_segment_labels)) {
    share <- ifelse(
      dfb$value >= 0,
      ifelse(dfb$total_pos > 0, dfb$value / pmax(dfb$total_pos, .Machine$double.eps), 0),
      ifelse(dfb$total_neg < 0, abs(dfb$value) / pmax(abs(dfb$total_neg), .Machine$double.eps), 0)
    )
    dfb$.__seg_text__. <- ifelse(share >= bar_label_min_prop, dfb$.__hover_y__., NA_character_)
  } else {
    dfb$.__seg_text__. <- NA_character_
  }

  # ---------- Add stacked bar traces ----------
  indices_for_group <- setNames(vector("list", length(groups)), groups)
  trace_index <- -1L
  set_title_1 <- FALSE

  for (g in groups) {
    col_g <- unname(col_vec[[g]])
    dbg <- dplyr::filter(dfb, .data$group == g)
    if (nrow(dbg) > 0) {
      indices_for_group[[g]] <- c(indices_for_group[[g]], trace_index + 1L)
      plt <- plotly::add_bars(
        plt,
        data = dbg,
        x = ~period, y = ~value,
        # keep 'text' for hover; use 'texttemplate' for visible callouts
        text = ~.__hover_y__.,
        texttemplate = ~.__seg_text__.,
        textposition = "inside",
        insidetextanchor = "middle",
        name = as.character(g),
        marker = list(color = col_g),
        hovertemplate = paste0(as.character(g), ": %{text}<extra></extra>"),
        legendgroup = dataset1_group,
        legendgrouptitle = if (!set_title_1) list(text = dataset1_group) else NULL,
        showlegend = TRUE
      )
      trace_index <- trace_index + 1L
      set_title_1 <- TRUE
    }
  }

  plt <- plotly::layout(plt, barmode = "stack")

  # ---------- Bold totals above each stack ----------
  extra_annotations <- list()
  if (isTRUE(bar_show_totals) && nrow(totals) > 0) {
    for (i in seq_len(nrow(totals))) {
      # top of stack (prefer positive; if only negatives, use negative top)
      top_y <- if (totals$total_pos[i] != 0) totals$total_pos[i] else totals$total_neg[i]
      if (!is.finite(top_y)) top_y <- 0
      if (top_y == 0 && totals$total_sum[i] == 0) next

      total_lbl <- fmt_financial3(totals$total_sum[i], prefix = y_prefix, suffix = y_suffix)
      yanchor <- if (top_y >= 0) "bottom" else "top"
      yshift  <- if (top_y >= 0) 2 else -2

      extra_annotations[[length(extra_annotations) + 1L]] <- list(
        x = totals$period[i], xref = "x",
        y = top_y,            yref = "y",
        xanchor = "center",
        yanchor = yanchor,
        text = paste0("<b>", total_lbl, "</b>"),
        showarrow = FALSE,
        align = "center",
        yshift = yshift,
        font = list(color = bar_total_label_color, size = bar_total_label_size),
        borderpad = 0
      )
    }
  }

  list(
    plt = plt,
    indices_for_group = indices_for_group,
    legend_flags = list(set_title_1 = set_title_1, set_title_2 = FALSE),
    range_hint = list(
      kind = "stacked",
      df_total = totals |>
        dplyr::transmute(
          time_period = period,
          total_pos = total_pos,
          total_neg = total_neg
        )
    ),
    extra_annotations = extra_annotations
  )
}
