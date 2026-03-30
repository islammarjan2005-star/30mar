
# ---- Optional future builders (skeletons) ----

# Unstacked AREA (fill to zero, no df2 overlay)
dbt_build_area <- function(
  plt, df1, groups, col_vec, dataset1_group
) {
  indices_for_group <- setNames(vector("list", length(groups)), groups)
  trace_index <- -1L
  set_title_1 <- FALSE

  for (g in groups) {
    col_g <- unname(col_vec[[g]])
    d1g <- dplyr::filter(df1, .data$group == g)
    if (nrow(d1g) > 0) {
      indices_for_group[[g]] <- c(indices_for_group[[g]], trace_index + 1L)
      plt <- plotly::add_trace(
        plt, data = d1g,
        x = ~time_period, y = ~value, text = ~.__hover_y__.,
        type = "scatter", mode = "lines",
        fill = "tozeroy",
        line = list(width = 0, color = col_g),
        fillcolor = col_g,
        name = as.character(g),
        hovertemplate = paste0(as.character(g), ": %{text}<extra></extra>"),
        legendgroup = dataset1_group,
        legendgrouptitle = if (!set_title_1) list(text = dataset1_group) else NULL,
        showlegend = TRUE
      )
      trace_index <- trace_index + 1L
      set_title_1 <- TRUE
    }
  }

  list(
    plt = plt,
    indices_for_group = indices_for_group,
    legend_flags = list(set_title_1 = set_title_1, set_title_2 = FALSE),
    range_hint = NULL
  )
}

# HEATMAP (expects df already in long form: x=time_period, group=row, value -> z)
# This is a minimal skeleton; adapt to your matrix pivot if needed.
dbt_build_heatmap <- function(
  plt, df1, groups, col_vec, colorscale = "Viridis"
) {
  # A simple pivot to wide: rows = group, cols = time_period, values = value
  if (!all(c("time_period","group","value") %in% names(df1))) {
    stop("heatmap builder expects columns: time_period, group, value")
  }
  # Ensure time axis sorted
  df1 <- dplyr::arrange(df1, time_period, group)
  # Build z-matrix (group x time)
  w <- tidyr::pivot_wider(df1, names_from = time_period, values_from = value)
  rowlabs <- w$group
  z <- as.matrix(w[setdiff(names(w), "group")])
  # Add heatmap trace
  plt <- plotly::add_heatmap(
    plt,
    z = z,
    x = as.Date(colnames(z)),
    y = as.character(rownamelabs <- rowlabs),
    colorscale = colorscale,
    showscale = TRUE
  )
  # No per-group indices in a single heatmap trace, but keep shape for palette menu
  list(
    plt = plt,
    indices_for_group = list(`heatmap` = 0L),
    legend_flags = list(set_title_1 = FALSE, set_title_2 = FALSE),
    range_hint = NULL
  )
}