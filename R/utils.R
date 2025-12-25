#' Moving Average
#'
#' Calculate a k-period moving average
#'
#' @param x Numeric vector
#' @param k Window size for moving average (default: 3)
#' @return Numeric vector with moving average values
#' @export
#' @examples
#' movavg_k(c(1, 2, 3, 4, 5), k = 3)
movavg_k <- function(x, k = 3) {
  x <- as.numeric(x)
  if (length(x) < k) return(rep(NA_real_, length(x)))
  as.numeric(stats::filter(x, rep(1 / k, k), sides = 1))
}

#' Format Numbers with Thousands Separator
#'
#' Format numeric values with comma as thousands separator
#'
#' @param x Numeric value or vector
#' @return Character string with formatted number
#' @export
#' @examples
#' fmt_miles(1000)
#' fmt_miles(1234567)
fmt_miles <- function(x) {
  formatC(x, format = "f", digits = 0, big.mark = ",")
}

#' Normalize Text Keys
#'
#' Normalize text by removing accents, converting to uppercase, and standardizing spaces
#'
#' @param x Character vector
#' @return Normalized character vector
#' @export
#' @examples
#' norm_key("San José")
norm_key <- function(x) {
  x <- stringi::stri_trans_general(as.character(x), "Latin-ASCII")
  x <- toupper(trimws(x))
  gsub("\\s+", " ", x)
}
