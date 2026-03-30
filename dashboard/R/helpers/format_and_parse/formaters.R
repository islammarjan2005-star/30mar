
# ========== INTERNAL UTILITIES ==========

#' @keywords internal
#' @noRd
.sign_prefix <- function(x, tol = getOption("govuk.delta.tol", 0)) {
  ifelse(x > tol, "+", ifelse(x < -tol, "-", ""))
}
 
# ========== NUMBER WITH SPECIFIED ROUNDING ==========

#' Number with thousands separators (unsigned)
#'
#' Rounds to the specified number of digits. Negative `digits` round to
#' tens/hundreds/thousands, etc. Decimals (when `digits > 0`) have trailing
#' zeros stripped to keep output tidy.
#'
#' @param x Numeric vector.
#' @param digits Integer. Number of decimal places to round to.
#'   * `0` (default): nearest integer.
#'   * `> 0`: keep up to that many decimals (strip trailing zeros).
#'   * `< 0`: round to 10s (-1), 100s (-2), 1000s (-3), etc. (no decimals shown).
#' @return Character vector like "1,234", "1,234.5", "12,340".
#' @examples
#' govuk_format_number(12345)            # "12,345"
#' govuk_format_number(1234.56, digits=1) # "1,234.6"
#' govuk_format_number(12345, digits=-1)  # "12,350"
#' govuk_format_number(12345, digits=-2)  # "12,300"
#' @export
govuk_format_number <- function(x, digits = 0) {
  out <- rep(NA_character_, length(x))
  bad <- is.na(x) | !is.finite(x)
  if (any(!bad)) {
    # Round at the requested precision (supports negative digits)
    xx <- round(x[!bad], digits = digits)

    # For display: if digits <= 0 we never show decimals
    display_digits <- if (digits > 0) digits else 0

    base <- formatC(xx, big.mark = ",", format = "f", digits = display_digits)

    # Strip trailing ".0"/".00" etc. when digits > 0 (tidier display)
    if (display_digits > 0) {
      base <- sub("\\.?0+$", "", base)
    }
    out[!bad] <- base
  }
  out
}

#' Number with thousands separators — DELTA (SIGNED)
#'
#' Adds a leading "+" for positive values, "-" for negatives, and no sign for
#' exact zeros (within tolerance). Rounding behaves like `govuk_format_number`:
#' negative `digits` round to 10s/100s/1000s etc., and decimals (when shown)
#' have trailing zeros stripped.
#'
#' @param x Numeric vector.
#' @param digits Integer. Number of decimal places to round to.
#'   * `0` (default): nearest integer.
#'   * `> 0`: keep up to that many decimals (strip trailing zeros).
#'   * `< 0`: round to 10s (-1), 100s (-2), 1000s (-3), etc. (no decimals shown).
#' @param tol Numeric tolerance for treating values as zero
#'   (default from option 'govuk.delta.tol', else 0).
#' @return Character vector like "-1,234", "0", "+12,340".
#' @examples
#' govuk_format_number_signed(c(-1234.5, 0, 1234.5))
#' # "-1,234.5" "0" "+1,234.5"
#' govuk_format_number_signed(12345, digits=-1) # "+12,350"
#' @export
govuk_format_number_signed <- function(x, digits = 0, tol = getOption("govuk.delta.tol", 0)) {
  out <- rep(NA_character_, length(x))
  bad <- is.na(x) | !is.finite(x)
  if (any(!bad)) {
    # Round first (supports negative digits)
    xx <- round(x[!bad], digits = digits)

    # Determine sign prefix with tolerance
    pref <- .sign_prefix(xx, tol = tol)

    # For display: if digits <= 0 we never show decimals
    display_digits <- if (digits > 0) digits else 0

    base <- formatC(abs(xx), big.mark = ",", format = "f", digits = display_digits)

    # Strip trailing ".0"/".00" etc. when digits > 0
    if (display_digits > 0) {
      base <- sub("\\.?0+$", "", base)
    }

    is_zero <- abs(xx) <= tol

    res <- paste0(pref, base)
    # For exact (or near) zero, drop the sign explicitly
    res[is_zero] <- base[is_zero]

    out[!bad] <- res
  }
  out
}

# ========== PERCENT: HEADLINE vs DELTA ==========

# HEADLINE (natural sign: no '+' on positives)

#' Percent (values already on 0–100), 1 decimal place – HEADLINES
#' e.g. 12.3 -> "12.3%"
#'
#' @param x Numeric vector (percent values 0–100).
#' @return Character vector like "12.3%".
#' @examples
#' govuk_format_percent1(12.3)  # "12.3%"
#' @export
govuk_format_percent1 <- function(x) {
  out <- rep(NA_character_, length(x))
  bad <- is.na(x) | !is.finite(x)
  if (any(!bad)) {
    xx <- x[!bad]
    out[!bad] <- sprintf("%.1f%%", xx)
  }
  out
}

#' Percent (proportions 0–1) -> 0–100, 1 dp – HEADLINES
#' e.g. 0.123 -> "12.3%"
#'
#' @param x Numeric vector as proportions (0–1).
#' @return Character vector like "12.3%".
#' @examples
#' govuk_format_percent1_prop(0.123)  # "12.3%"
#' @export
govuk_format_percent1_prop <- function(x) {
  out <- rep(NA_character_, length(x))
  bad <- is.na(x) | !is.finite(x)
  if (any(!bad)) {
    xx <- x[!bad] * 100
    out[!bad] <- sprintf("%.1f%%", xx)
  }
  out
}

# DELTAS (unsigned: absolute, for when colour tag conveys direction)

#' Percent (0–100), 1 dp – DELTAS (NO SIGN)
#' Uses absolute value so your sign/colour logic controls the tag.
#'
#' @param x Numeric vector (percent values 0–100).
#' @return Character vector like "0.3%".
#' @examples
#' govuk_format_percent1_abs(-0.3)  # "0.3%"
#' @export
govuk_format_percent1_abs <- function(x) {
  out <- rep(NA_character_, length(x))
  bad <- is.na(x) | !is.finite(x)
  if (any(!bad)) {
    xx <- abs(x[!bad])
    out[!bad] <- sprintf("%.1f%%", xx)
  }
  out
}

#' Percent (proportions 0–1), 1 dp – DELTAS (NO SIGN)
#'
#' @param x Numeric vector as proportions (0–1).
#' @return Character vector like "0.4%".
#' @examples
#' govuk_format_percent1_prop_abs(-0.004)  # "0.4%"
#' @export
govuk_format_percent1_prop_abs <- function(x) {
  out <- rep(NA_character_, length(x))
  bad <- is.na(x) | !is.finite(x)
  if (any(!bad)) {
    xx <- abs(x[!bad]) * 100
    out[!bad] <- sprintf("%.1f%%", xx)
  }
  out
}

# DELTAS (signed: explicit + / -; no sign for zero)

#' Percent (0–100), 1 dp – DELTAS (SIGNED)
#' e.g. c(-0.3, 0, 0.4) -> "-0.3%", "0.0%", "+0.4%"
#'
#' @param x Numeric vector (percent values 0–100).
#' @param tol Numeric tolerance for treating values as zero (default from option 'govuk.delta.tol', else 0).
#' @return Character vector like "-0.3%", "0.0%", "+0.4%".
#' @examples
#' govuk_format_percent1_signed(c(-0.3, 0, 0.4))
#' @export
govuk_format_percent1_signed <- function(x, tol = getOption("govuk.delta.tol", 0)) {
  out <- rep(NA_character_, length(x))
  bad <- is.na(x) | !is.finite(x)
  if (any(!bad)) {
    xx <- x[!bad]
    pref <- .sign_prefix(xx, tol = tol)
    body <- sprintf("%.1f%%", abs(xx))
    is_zero <- abs(xx) <= tol
    res <- paste0(pref, body)
    res[is_zero] <- sprintf("%.1f%%", 0)
    out[!bad] <- res
  }
  out
}

#' Percent (proportions 0–1), 1 dp – DELTAS (SIGNED)
#' e.g. c(-0.004, 0, 0.004) -> "-0.4%", "0.0%", "+0.4%"
#'
#' @param x Numeric vector as proportions (0–1).
#' @param tol Numeric tolerance for treating values as zero (default from option 'govuk.delta.tol', else 0).
#' @return Character vector like "-0.4%", "0.0%", "+0.4%".
#' @examples
#' govuk_format_percent1_prop_signed(c(-0.004, 0, 0.004))
#' @export
govuk_format_percent1_prop_signed <- function(x, tol = getOption("govuk.delta.tol", 0)) {
  out <- rep(NA_character_, length(x))
  bad <- is.na(x) | !is.finite(x)
  if (any(!bad)) {
    xx <- x[!bad] * 100
    pref <- .sign_prefix(xx, tol = tol)
    body <- sprintf("%.1f%%", abs(xx))
    is_zero <- abs(xx) <= tol
    res <- paste0(pref, body)
    res[is_zero] <- sprintf("%.1f%%", 0)
    out[!bad] <- res
  }
  out
}

# ========== MILLIONS (rounded to nearest 10,000) ==========

# HEADLINE (natural sign)

#' Millions (rounded to nearest 10,000; 0.01m steps) – HEADLINES
#' Example: 14,132,000 -> "14.13m"
#'
#' @param x Numeric vector (absolute numbers).
#' @return Character vector like "14.13m".
#' @examples
#' govuk_format_millions_10k(14132000)  # "14.13m"
#' @export
govuk_format_millions_10k <- function(x) {
  out <- rep(NA_character_, length(x))
  bad <- is.na(x) | !is.finite(x)
  if (any(!bad)) {
    xx <- x[!bad]
    rounded <- round(xx / 10000) * 10000
    in_m <- rounded / 1e6
    out[!bad] <- paste0(
      formatC(in_m, format = "f", digits = 2, big.mark = ","),
      "m"
    )
  }
  out
}

# DELTAS (unsigned)

#' Millions rounded to nearest 10,000 – DELTAS (NO SIGN)
#'
#' @param x Numeric vector.
#' @return Character vector like "0.33m".
#' @examples
#' govuk_format_millions_10k_abs(-325000)  # "0.33m"
#' @export
govuk_format_millions_10k_abs <- function(x) {
  out <- rep(NA_character_, length(x))
  bad <- is.na(x) | !is.finite(x)
  if (any(!bad)) {
    xx <- abs(x[!bad])
    rounded <- round(xx / 10000) * 10000
    in_m <- rounded / 1e6
    out[!bad] <- paste0(
      formatC(in_m, format = "f", digits = 2, big.mark = ","),
      "m"
    )
  }
  out
}

# DELTAS (signed)

#' Millions rounded to nearest 10,000 – DELTAS (SIGNED)
#' Example: c(-325000, 0, 325000) -> "-0.33m", "0.00m", "+0.33m"
#'
#' @param x Numeric vector.
#' @param tol Numeric tolerance for treating values as zero (default from option 'govuk.delta.tol', else 0).
#' @return Character vector like "-0.33m", "0.00m", "+0.33m".
#' @examples
#' govuk_format_millions_10k_signed(c(-325000, 0, 325000))
#' @export
govuk_format_millions_10k_signed <- function(x, tol = getOption("govuk.delta.tol", 0)) {
  out <- rep(NA_character_, length(x))
  bad <- is.na(x) | !is.finite(x)
  if (any(!bad)) {
    xx <- x[!bad]
    rounded <- round(xx / 10000) * 10000
    in_m <- rounded / 1e6
    pref <- .sign_prefix(in_m, tol = tol)
    body <- formatC(abs(in_m), format = "f", digits = 2, big.mark = ",")
    is_zero <- abs(in_m) <= tol
    res <- paste0(pref, body, "m")
    res[is_zero] <- paste0(formatC(0, format = "f", digits = 2), "m")
    out[!bad] <- res
  }
  out
}

# ========== GBP (≤ 2 dp, strip trailing zeros, with £) ==========

# HEADLINE (natural sign)

#' GBP (≤ 2 dp, strip trailing zeros), with comma separators – HEADLINES
#' Keeps the natural minus for negative values (e.g., -£50.2).
#'
#' @param x Numeric vector.
#' @return Character vector like "£1,234.56" or "£1,234".
#' @examples
#' govuk_format_gbp(c(1234, 1234.5, 1234.00, -50.2))
#' # "£1,234", "£1,234.5", "£1,234", "-£50.2"
#' @export
govuk_format_gbp <- function(x) {
  out <- rep(NA_character_, length(x))
  bad <- is.na(x) | !is.finite(x)
  if (any(!bad)) {
    xx <- round(x[!bad], 2)
    sign <- ifelse(xx < 0, "-", "")
    absx <- abs(xx)
    base <- formatC(absx, big.mark = ",", format = "f", digits = 2)
    base <- sub("\\.?0+$", "", base)  # strip .0/.00
    out[!bad] <- paste0(sign, "£", base)
  }
  out
}

# DELTAS (unsigned)

#' GBP (≤ 2 dp, strip trailing zeros) – DELTAS (NO SIGN)
#' Formats absolute values; leave sign/colour to tag logic.
#'
#' @param x Numeric vector.
#' @return Character vector like "£5.4" or "£1,234".
#' @examples
#' govuk_format_gbp_abs(-5.4)   # "£5.4"
#' @export
govuk_format_gbp_abs <- function(x) {
  out <- rep(NA_character_, length(x))
  bad <- is.na(x) | !is.finite(x)
  if (any(!bad)) {
    xx <- round(abs(x[!bad]), 2)
    base <- formatC(xx, big.mark = ",", format = "f", digits = 2)
    base <- sub("\\.?0+$", "", base)
    out[!bad] <- paste0("£", base)
  }
  out
}

# DELTAS (signed)

#' GBP (≤ 2 dp, strip trailing zeros) – DELTAS (SIGNED)
#' Examples: c(-5.4, 0, 1234.00) -> "-£5.4", "£0", "+£1,234"
#'
#' @param x Numeric vector.
#' @param tol Numeric tolerance for treating values as zero (default from option 'govuk.delta.tol', else 0).
#' @return Character vector like "-£5.4", "£0", "+£1,234".
#' @examples
#' govuk_format_gbp_signed(c(-5.4, 0, 1234.00))
#' @export
govuk_format_gbp_signed <- function(x, tol = getOption("govuk.delta.tol", 0)) {
  out <- rep(NA_character_, length(x))
  bad <- is.na(x) | !is.finite(x)
  if (any(!bad)) {
    xx <- round(x[!bad], 2)
    pref <- .sign_prefix(xx, tol = tol)
    base <- formatC(abs(xx), big.mark = ",", format = "f", digits = 2)
    base <- sub("\\.?0+$", "", base)  # strip .0/.00
    is_zero <- abs(xx) <= tol
    res <- paste0(pref, "£", base)
    res[is_zero] <- paste0("£", base[is_zero])  # no sign for zero
    out[!bad] <- res
  }
  out
}

# ========== Percentage points (pp) ==========

# DELTAS (unsigned)

#' Percentage points, 1 dp – DELTAS (NO SIGN)
#' Displays absolute value with "pp" suffix, e.g., 0.3 -> "0.3pp".
#'
#' @param x Numeric vector (percentage points).
#' @return Character vector like "0.3pp".
#' @examples
#' govuk_format_pp1_abs(-0.3)  # "0.3pp"
#' @export
govuk_format_pp1_abs <- function(x) {
  out <- rep(NA_character_, length(x))
  bad <- is.na(x) | !is.finite(x)
  if (any(!bad)) {
    xx <- abs(x[!bad])
    out[!bad] <- sprintf("%.1fpp", xx)
  }
  out
}

# DELTAS (signed)

#' Percentage points, 1 dp – DELTAS (SIGNED)
#' e.g. c(-0.3, 0, 0.3) -> "-0.3pp", "0.0pp", "+0.3pp"
#'
#' @param x Numeric vector (percentage points).
#' @param tol Numeric tolerance for treating values as zero (default from option 'govuk.delta.tol', else 0).
#' @return Character vector like "-0.3pp", "0.0pp", "+0.3pp".
#' @examples
#' govuk_format_pp1_signed(c(-0.3, 0, 0.3))
#' @export
govuk_format_pp1_signed <- function(x, tol = getOption("govuk.delta.tol", 0)) {
  out <- rep(NA_character_, length(x))
  bad <- is.na(x) | !is.finite(x)
  if (any(!bad)) {
    xx <- x[!bad]
    pref <- .sign_prefix(xx, tol = tol)
    body <- sprintf("%.1fpp", abs(xx))
    is_zero <- abs(xx) <= tol
    res <- paste0(pref, body)
    res[is_zero] <- sprintf("%.1fpp", 0)
    out[!bad] <- res
  }
  out
}

# ========== TAG COLOUR (unchanged, expects numeric or signed string) ==========

#' Internal: choose GOV.UK tag colour from a delta
#'
#' @param delta Numeric or character; leading "+" / "-" parsed if character.
#' @param good_if_increase Logical; if TRUE, positive is "green", else "red".
#' @return One of "green", "red", or "blue".
#' @keywords internal
#' @noRd
.govuk_tag_colour <- function(delta, good_if_increase = TRUE) {
  if (is.null(delta)) return("blue")  # neutral when no delta

  # Determine sign for numeric or "+/-" strings
  sign <- if (is.numeric(delta)) {
    if (delta > 0) 1L else if (delta < 0) -1L else 0L
  } else if (is.character(delta)) {
    if (grepl("^\\s*\\+", delta)) 1L else if (grepl("^\\s*-", delta)) -1L else 0L
  } else 0L

  if (sign == 0) {
    "blue"
  } else if (good_if_increase) {
    if (sign > 0) "green" else "red"
  } else {
    if (sign > 0) "red" else "green"
  }
}

#' Split a title into two lines without splitting words
#'
#' @param title Character scalar.
#' @param threshold Integer; number of characters before attempting line break.
#' @return HTML-ready string containing "<br>" if split is applied.
#' @keywords internal
split_title_at <- function(title, threshold = 25) {
  # Titles under threshold: no split
  if (nchar(title) <= threshold) return(title)

  # Look for the nearest space BEFORE the threshold
  before <- substr(title, 1, threshold)
  space_pos <- max(gregexpr(" ", before)[[1]])

  if (space_pos > 0) {
    # Clean split at last full word before threshold
    line1 <- substr(title, 1, space_pos - 1)
    line2 <- substr(title, space_pos + 1, nchar(title))
    return(paste0(line1, "<br>", line2))
  }

  # Fallback: no space before threshold, look AFTER threshold
  after <- substr(title, threshold + 1, nchar(title))
  next_space <- gregexpr(" ", after)[[1]][1]

  if (next_space > 0) {
    break_pos <- threshold + next_space
    line1 <- substr(title, 1, break_pos - 1)
    line2 <- substr(title, break_pos + 1, nchar(title))
    return(paste0(line1, "<br>", line2))
  }

  # Final fallback: no spaces at all → force break at threshold
  line1 <- substr(title, 1, threshold)
  line2 <- substr(title, threshold + 1, nchar(title))
  paste0(line1, "<br>", line2)
}