
# ---- STACKED AREA ----
dbt_build_stacked_area <- function(
  plt, df1, groups, col_vec, dataset1_group
) {
  indices_for_group <- setNames(vector("list", length(groups)), groups)
  trace_index <- -1L
  set_title_1 <- FALSE
  is_first <- TRUE

  for (g in groups) {
    col_g <- unname(col_vec[[g]])
    d1g <- dplyr::filter(df1, .data$group == g)
    if (nrow(d1g) > 0) {
      indices_for_group[[g]] <- c(indices_for_group[[g]], trace_index + 1L)
      fill_mode <- if (is_first) "tozeroy" else "tonexty"
      is_first <- FALSE
      plt <- plotly::add_trace(
        plt, data = d1g,
        x = ~time_period, y = ~value, text = ~.__hover_y__.,
        type = "scatter", mode = "lines",
        stackgroup = "one",
        fill = fill_mode,
        fillcolor = col_g,
        line = list(width = 0, color = col_g),
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
    range_hint = list(
      kind = "stacked",
      df_total = df1 |>
        dplyr::group_by(time_period) |>
        dplyr::summarise(
          total_pos = sum(pmax(value, 0), na.rm = TRUE),
          total_neg = sum(pmin(value, 0), na.rm = TRUE),
          .groups = "drop"
        )
    )
  )
}