# Check for valid color names
#'
#' Check whether a character string is a valid color specification.
#'
#' @param x 'character' vector.
#'   color specification
#' @param null_ok 'logical' flag.
#'   If set to `TRUE`, `x` may also be `NULL`.
#'
#' @return A 'logical' vector of the same length as argument `x`.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords internal
#'
#' @export
#'
#' @examples
#' is_color(c("red", "zzz", "#FFFFFF", "#7FAF1B111"))

is_color <- function(x, null_ok = FALSE) {
  if (is.null(x) && null_ok) {
    return(TRUE)
  }
  vapply(x, function(i) {
    tryCatch({
      is.matrix(grDevices::col2rgb(i))
    }, error = function(e) FALSE)
  }, TRUE)
}
