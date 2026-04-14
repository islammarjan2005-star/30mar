

# -----------------------------------------------------------------------------
# SQL fragments for date parsing
# -----------------------------------------------------------------------------

# Quarter range like "Oct–Dec 2024" (em/en dashes normalized to "-"):
#   - split on "-", take the *second* part ("Dec 2024")
#   - normalize whitespace/case
#   - to_date('Mon YYYY')
.sql_parse_quarter_range <- function(col_name) {
  dbplyr::sql(sprintf("
    to_date(
      initcap(substr(
        btrim(split_part(
          regexp_replace(
            btrim(regexp_replace(%s::text, '\\\\s+', ' ', 'g')),
            '[\\u2013\\u2014]', '-', 'g'
          ),
          '-', 2
        )),
        1, 8
      )),
      'Mon YYYY'
    )::date
  ", col_name))
}

# Monthly "MMM YYYY" using your CAST approach (as in get_awe_table)
.sql_parse_month_yyyy_cast <- function(col_name) {
  dbplyr::sql(sprintf("CAST(%s AS DATE)", col_name))
}

# Monthly "MMM YY" using to_date with a 2-digit year mask
.sql_parse_month_yy <- function(col_name) {
  dbplyr::sql(sprintf("
    to_date(
      initcap(btrim(%s::text)),
      'Mon YY'
    )::date
  ", col_name))
}

# Router by descriptive parse_mode
.date_sql_for <- function(parse_mode, col_name) {
  parse_mode <- match.arg(parse_mode, c("none", "MMM-MMM YYYY", "MMM YYYY", "MMM YY"))
  switch(
    parse_mode,
    "none"         = NULL,
    "MMM-MMM YYYY" = .sql_parse_quarter_range(col_name),
    "MMM YYYY"     = .sql_parse_month_yyyy_cast(col_name),
    "MMM YY"       = .sql_parse_month_yy(col_name)
  )
}