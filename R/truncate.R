#' Truncate a character string by bytesize.
#' @param string A character vector.
#' @param size Maximum bytesize of string.
#' @param ellipsis Content of ellipsis that indicates content has been removed.
#' @export
#' @examples
#' x <- "This string is moderately long"
#' byte_trunc(x, 20)
#'
#' y <- "こんにちは"
#' byte_trunc(y, 10)
byte_trunc <- function(string, size, ellipsis = "...") {
  string <- enc2utf8(string)
  ellipsis <- enc2utf8(ellipsis)

  too_long <- !is.na(string) & nchar(string, type = "bytes") > size
  size <- size - nchar(ellipsis, type = "bytes")
  if (size < 0L) {
    stop("`size` is shorter than `ellipsis`", .call = FALSE)
  }

  string[too_long] <- paste0(byte_trunc_(string[too_long], size), ellipsis)
  Encoding(string) <- "UTF-8"

  string
}
