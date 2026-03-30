# ---- LINE (with optional df2 overlay) ----
dbt_build_line <- function(
  plt, df1, df2,
  groups, col_vec,
  line_width, connect_gaps,
  dataset1_name, dataset2_name,
  dataset1_group, dataset2_group,
  line_dash1, line_dash2,
  fmt_financial3, y_prefix, y_suffix
) {
  indices_for_group <- setNames(vector("list", length(groups)), groups)
  trace_index <- -1L
  set_title_1 <- FALSE
  set_title_2 <- FALSE

  for (g in groups) {
    col_g <- unname(col_vec[[g]])
    d1g <- dplyr::filter(df1, .data$group == g)
    if (nrow(d1g) > 0) {
      indices_for_group[[g]] <- c(indices_for_group[[g]], trace_index + 1L)
      plt <- plotly::add_trace(
        plt, data = d1g,
        x = ~time_period, y = ~value, text = ~.__hover_y__.,
        type = "scatter", mode = "lines",
        connectgaps = connect_gaps,
        name = paste0(as.character(g), " — ", dataset1_name),
        line = list(width = line_width, color = col_g, dash = line_dash1),
        hovertemplate = paste0(as.character(g), ": %{text}<extra></extra>"),
        legendgroup = dataset1_group,
        legendgrouptitle = if (!set_title_1) list(text = dataset1_group) else NULL,
        showlegend = TRUE
      )
      trace_index <- trace_index + 1L
      set_title_1 <- TRUE

      if (!is.null(df2)) {
        d2g <- dplyr::filter(df2, .data$group == g)
        if (nrow(d2g) > 0) {
          d2g$.__hover_y__. <- vapply(d2g$value, fmt_financial3, "",
                                      prefix = y_prefix, suffix = y_suffix)
          indices_for_group[[g]] <- c(indices_for_group[[g]], trace_index + 1L)
          plt <- plotly::add_trace(
            plt, data = d2g,
            x = ~time_period, y = ~value, text = ~.__hover_y__.,
            type = "scatter", mode = "lines",
            connectgaps = connect_gaps,
            name = paste0(as.character(g), " — ", dataset2_name),
            line = list(width = line_width, color = col_g, dash = line_dash2),
            hovertemplate = paste0(as.character(g), ": %{text}<extra></extra>"),
            legendgroup = dataset2_group,
            legendgrouptitle = if (!set_title_2) list(text = dataset2_group) else NULL,
            showlegend = TRUE
          )
          trace_index <- trace_index + 1L
          set_title_2 <- TRUE
        }
      }
    }
  }

  list(
    plt = plt,
    indices_for_group = indices_for_group,
    legend_flags = list(set_title_1 = set_title_1, set_title_2 = set_title_2),
    range_hint = NULL
  )
}