#' Construct an inlpal-class object
#'
#' A constructor for the 'inlpal' class.
#'
#' @param x 'character' vector.
#'   Palette colors represented in hexadecimal format.
#' @param call 'call'-class object.
#'   Unevaluated function call (expression) that can be used to reproduce the color palette.
#' @param nan 'character' string.
#'   Color meant for missing data, in hexadecimal format,
#'   where `NA` indicates no color is specified.
#'
#' @return An object of class 'inlpal' that inherits behavior from the 'character' class.
#'   The returned object is comprised of a 'character'
#'   vector of `n` colors in the RGB color system.
#'   Colors are specified with a string of the form `"#RRGGBB"` or `"#RRGGBBAA"`,
#'   where `RR`, `GG`, `BB`, and `AA` are the
#'   red, green, blue, and alpha hexadecimal values (00 to FF), respectively.
#'   Attributes of the returned object include:
#'   `"names"` is an the informal names assigned to colors in the palette,
#'   where `NULL` indicates no color names are specified;
#'   `"nan"` is a character string giving the color meant for missing data,
#'   in hexadecimal format, where `NA` indicates no color is specified; and
#'   `"call"` is an object of class '[call]' giving the unevaluated function
#'   call (expression) that can be used to reproduce the color palette.
#'   Use the [`eval`] function to evaluate the `"call"` argument.
#'   A simple [`plot`][plot.inlpal] method is provided for the 'inlpal' class.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords internal
#'
#' @export

make_inlpal_class <- function(x, call, nan = NA_character_) {

  # set pattern for color strings
  pattern <- "^#(\\d|[a-f]){6,8}$"

  # check arguments
  checkmate::assertCharacter(x,
    pattern = pattern,
    ignore.case = TRUE,
    any.missing = FALSE,
    min.len = 1
  )
  checkmate::assertString(nan,
    na.ok = TRUE,
    pattern = pattern,
    ignore.case = TRUE
  )
  stopifnot(is.call(call))

  fun_nm <- rlang::call_name(call)
  checkmate::assert_subset(fun_nm, choices = c("get_colors", "set_hinge"))

  args <- formals(fun_nm) |> names()
  is <- args %in% c(names(as.list(call)), "...")
  stopifnot(all(is))

  structure(x,
    nan = nan,
    call = call,
    class = append("inlpal", class(x))
  )
}
