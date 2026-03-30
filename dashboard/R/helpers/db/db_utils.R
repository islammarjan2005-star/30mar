
# ---- Render SQL safely when using a Pool-backed remote ----
sql_render_pool_safe <- function(tbl) {
  con <- pool::poolCheckout(APP_DB$pool)
  on.exit(pool::poolReturn(con), add = TRUE)
  dbplyr::sql_render(tbl, con = con)
}

# ---- Generic lazy filter helper usable across datasets ----
# - date_col: name of the date column (Date) to filter (or NULL to skip)
# - where_in:    named list of column -> vector; applies "col %in% values"
# - where_equals:named list of column -> scalar; applies "col == value"
# NOTE: All filters are applied lazily (translated to SQL). No collect() here.
apply_filters_general <- function(
  tbl,
  date_col     = "time_period",
  date_from    = NULL,
  date_to      = NULL,
  where_in     = NULL,
  where_equals = NULL
) {
  stopifnot(inherits(tbl, "tbl_sql") || inherits(tbl, "tbl_lazy") || inherits(tbl, "tbl_dbi"))

  # Date range
  if (!is.null(date_col) && nzchar(date_col)) {
    if (!is.null(date_from)) tbl <- tbl %>% dplyr::filter(.data[[date_col]] >= !!as.Date(date_from))
    if (!is.null(date_to))   tbl <- tbl %>% dplyr::filter(.data[[date_col]] <= !!as.Date(date_to))
  }

  # IN filters
  if (!is.null(where_in) && length(where_in)) {
    for (nm in names(where_in)) {
      vals <- where_in[[nm]]
      if (!is.null(vals) && length(vals)) {
        tbl <- tbl %>% dplyr::filter(.data[[nm]] %in% !!as.character(vals))
      }
    }
  }

  # Equality filters
  if (!is.null(where_equals) && length(where_equals)) {
    for (nm in names(where_equals)) {
      v <- where_equals[[nm]]
      if (!is.null(v) && length(v) == 1L && nzchar(as.character(v))) {
        tbl <- tbl %>% dplyr::filter(.data[[nm]] == !!v)
      }
    }
  }

  tbl
}

get_latest_value_eq <- function(
  schema,
  table,
  date_col,        # raw date/text column name in the source table
  filter_col,      # column to filter on (exact == match)
  filter_value,    # scalar exact value
  value_col,       # numeric column to return
  mode = c("latest", "diff"),                # calc mode
  parse_mode = c("none", "MMM-MMM YYYY", "MMM YYYY", "MMM YY"),  # date parser
  pool = APP_DB$pool,
  with_tooltip = FALSE,                      # when TRUE (and mode == "latest"), return list(value, delta, tooltip)
  date_fmt = "%Y-%m-%d"                      # formatting for date in tooltip
) {
  mode <- match.arg(mode)
  parse_mode <- match.arg(parse_mode)

  # 1) Base table
  base <- dplyr::tbl(pool, dbplyr::in_schema(schema, table))

  # 2) Parse/normalize date column if requested (DB-side)
  date_sql <- .date_sql_for(parse_mode, date_col)
  if (!is.null(date_sql)) {
    base <- base %>% dplyr::mutate(!!date_col := !!date_sql)
  }

  # (Optional) keep only rows with valid dates to be strict:
  # base <- base %>% dplyr::filter(!is.na(.data[[date_col]]))

  # 3) Exact filter (DB-side)
  filtered <- base %>% dplyr::filter(.data[[filter_col]] == !!filter_value)

  # 4) Latest / Diff
  if (mode == "latest") {
    if (!with_tooltip) {
      # Backward compatible path: fetch only the latest value
      latest_val <- filtered %>%
        dplyr::slice_max(order_by = .data[[date_col]], n = 1, with_ties = FALSE) %>%
        dplyr::pull(var = value_col)

      return(if (length(latest_val) >= 1) latest_val[[1]] else NA_real_)
    }

    # Bundled path: fetch top 2 rows once, and build (value, delta, tooltip)
    top2 <- filtered %>%
      dplyr::slice_max(order_by = .data[[date_col]], n = 2, with_ties = FALSE) %>%
      dplyr::select(!!date_col, !!value_col) %>%
      dplyr::collect()

    if (nrow(top2) == 0) {
      return(list(
        value   = NA_real_,
        delta   = NA_real_,
        tooltip = sprintf(
          "Reading taken on %s. Based on variable %s from %s.%s",
          "unknown date", value_col, schema, table
        )
      ))
    }

    # Row 1 is latest because of slice_max(with_ties = FALSE)
    latest_val <- top2[[value_col]][[1]]
    latest_dt  <- top2[[date_col]][[1]]
    prev_val   <- if (nrow(top2) >= 2) top2[[value_col]][[2]] else NA_real_
    delta_val  <- if (!is.na(prev_val)) latest_val - prev_val else NA_real_

    # Format date robustly for tooltip
    date <- tryCatch({
      if (inherits(latest_dt, "POSIXt")) {
        format(latest_dt, date_fmt, tz = attr(latest_dt, "tzone") %||% "UTC")
      } else if (inherits(latest_dt, "Date")) {
        format(latest_dt, date_fmt)
      } else {
        # last resort: attempt to coerce; if fails, use as is
        dt <- suppressWarnings(as.POSIXct(latest_dt, tz = "UTC"))
        if (!is.na(dt)) format(dt, date_fmt, tz = "UTC") else as.character(latest_dt)
      }
    }, error = function(e) as.character(latest_dt))

    tooltip <- sprintf(
      "Based on variable %s from %s.%s. Date: %s",
      filter_value, schema, table, date
    )

    return(list(value = latest_val, delta = delta_val, tooltip = tooltip))
  }

  # mode == "diff"
  vals <- filtered %>%
    dplyr::slice_max(order_by = .data[[date_col]], n = 2, with_ties = FALSE) %>%
    dplyr::pull(var = value_col)

  if (length(vals) < 2) return(NA_real_)
  vals[[1]] - vals[[2]]
}
