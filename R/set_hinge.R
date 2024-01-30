#' Set hinge location in color palette
#'
#' The *hinge* indicates a dramatic color change in a palette
#' that is typically located at the midpoint of the data range.
#' An asymmetrical data range can result in an undesired hinge location,
#' a location that does not necessarily coincide with the break-point in the user's data.
#' This function can be used to specify a hinge location that is appropriate for your data.
#'
#' @param x 'numeric' object that can be passed to the [`range`]
#'   function with `NA`'s removed.
#'   The user's data range.
#' @param hinge 'numeric' number.
#'   Hinge value (such as, at sea-level) in data units.
#' @param scheme 'character' vector of length 1 or 2, value is recycled as necessary.
#'   Name of color scheme(s).
#'   The color palette is derived from one or two color schemes.
#'   The scheme(s) must be suitable for continuous data types and allow for color interpolation.
#'   See [`get_colors`] function for a list of possible scheme names.
#'   Argument choices may be abbreviated as long as there is no ambiguity.
#' @param alpha 'numeric' vector of length 1 or 2, value is recycled as necessary.
#'   Alpha transparency applied separately on either side of the hinge.
#'   Values range from 0 (fully transparent) to 1 (fully opaque).
#'   Specify as `NULL` to exclude the alpha channel value from colors.
#' @param reverse 'logical' vector of length 1 or 2, value is recycled as necessary.
#'   Whether to reverse the order of colors in the scheme(s).
#'   Values applied separately on either side of the hinge.
#' @param buffer 'numeric' vector of length 1 or 2, value is recycled as necessary.
#'   Color buffer around the hinge measured as a fraction of the color range.
#'   Values applied separately on either side of the hinge.
#' @param stops 'numeric' vector of length 2.
#'   Color stops defined by interval endpoints (between 0 and 1)
#'   and used to select a subset of the color palette(s).
#' @param allow_bias 'logical' flag.
#'   Whether to allow bias in the color spacing.
#' @param nan 'character' string.
#'   Color meant for missing data, in hexadecimal format,
#'   where `NA` indicates no color is specified.
#'
#' @return A 'function' that takes an 'integer' argument (the required number of colors)
#'   and returns a vector of colors of class '[inlpal][make_inlpal_class]'.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso [`plot`][plot.inlpal] method for drawing color palettes.
#'
#' @keywords color
#'
#' @export
#'
#' @examples
#' f <- set_hinge(c(-3, 7), hinge = 0)
#' f(n = 19) |> plot()
#'
#' x <- datasets::volcano
#' f <- set_hinge(x, hinge = 140, scheme = c("abyss", "dem1"))
#' filled.contour(x, color.palette = f, nlevels = 50, plot.axes = FALSE)
#'
#' # Data range (x)
#' hinge <- 0
#' n <- 20
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
#' set_hinge(c(-10, 0), hinge)(n) |> plot()
#' set_hinge(c(-7, 3), hinge)(n) |> plot()
#' set_hinge(c(-5, 5), hinge)(n) |> plot()
#' set_hinge(c(-3, 7), hinge)(n) |> plot()
#' set_hinge(c(0, 10), hinge)(n) |> plot()
#' par(op)
#'
#' # Hinge value (hinge)
#' x <- c(-5, 5)
#' n <- 255
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
#' set_hinge(x, hinge = -6)(n) |> plot()
#' set_hinge(x, hinge = -2)(n) |> plot()
#' set_hinge(x, hinge = 0)(n) |> plot()
#' set_hinge(x, hinge = 2)(n) |> plot()
#' set_hinge(x, hinge = 6)(n) |> plot()
#' par(op)
#'
#' # Color scheme (scheme)
#' x <- c(-10, 10)
#' hinge <- -3
#' n <- 255
#' op <- par(mfrow = c(3, 1), oma = c(0, 0, 0, 0))
#' set_hinge(x, hinge, scheme = "romaO")(n) |> plot()
#' set_hinge(x, hinge, scheme = "BuRd")(n) |> plot()
#' set_hinge(x, hinge, scheme = c("ocean", "copper"))(n) |> plot()
#' par(op)
#'
#' # Alpha transparency (alpha)
#' x <- c(-5, 5)
#' hinge <- 0
#' scheme <- c("drywet", "hawaii")
#' n <- 255
#' op <- par(mfrow = c(4, 1), oma = c(0, 0, 0, 0))
#' set_hinge(x, hinge, scheme, alpha = 1.0)(n) |> plot()
#' set_hinge(x, hinge, scheme, alpha = 0.5)(n) |> plot()
#' set_hinge(x, hinge, scheme, alpha = c(1.0, 0.5))(n) |> plot()
#' set_hinge(x, hinge, scheme, alpha = c(0.5, 1.0))(n) |> plot()
#' par(op)
#'
#' # Reverse colors (reverse)
#' x <- c(-10, 10)
#' hinge <- -3
#' n <- 255
#' op <- par(mfrow = c(6, 1), oma = c(0, 0, 0, 0))
#' set_hinge(x, hinge, "romaO", reverse = FALSE)(n) |> plot()
#' set_hinge(x, hinge, "romaO", reverse = TRUE)(n) |> plot()
#' set_hinge(x, hinge, c("davos", "hawaii"), reverse = FALSE)(n) |> plot()
#' set_hinge(x, hinge, c("davos", "hawaii"), reverse = TRUE)(n) |> plot()
#' set_hinge(x, hinge, c("davos", "hawaii"), reverse = c(TRUE, FALSE))(n) |> plot()
#' set_hinge(x, hinge, c("davos", "hawaii"), reverse = c(FALSE, TRUE))(n) |> plot()
#' par(op)
#'
#' # Buffer around hinge (buffer)
#' x <- c(-5, 5)
#' hinge <- -2
#' n <- 20
#' op <- par(mfrow = c(6, 1), oma = c(0, 0, 0, 0))
#' set_hinge(x, hinge, buffer = 0.0)(n) |> plot()
#' set_hinge(x, hinge, buffer = 0.2)(n) |> plot()
#' set_hinge(x, hinge, buffer = c(0.4, 0.2))(n) |> plot()
#' set_hinge(x, hinge, c("gray", "plasma"), buffer = 0.0)(n) |> plot()
#' set_hinge(x, hinge, c("gray", "plasma"), buffer = 0.2)(n) |> plot()
#' set_hinge(x, hinge, c("gray", "plasma"), buffer = c(0.2, 0.4))(n) |> plot()
#' par(op)
#'
#' # Color stops (stops)
#' x <- c(-5, 5)
#' hinge <- 1
#' n <- 20
#' op <- par(mfrow = c(6, 1), oma = c(0, 0, 0, 0))
#' set_hinge(x, hinge, stops = c(0.0, 1.0))(n) |> plot()
#' set_hinge(x, hinge, stops = c(0.2, 0.8))(n) |> plot()
#' set_hinge(x, hinge, stops = c(0.4, 0.6))(n) |> plot()
#' set_hinge(x, hinge, c("gray", "plasma"), stops = c(0.0, 1.0))(n) |> plot()
#' set_hinge(x, hinge, c("gray", "plasma"), stops = c(0.2, 0.8))(n) |> plot()
#' set_hinge(x, hinge, c("gray", "plasma"), stops = c(0.4, 0.6))(n) |> plot()
#' par(op)
#'
#' # Allow bias (allow_bias)
#' x <- c(-3, 7)
#' n <- 20
#' op <- par(mfrow = c(4, 1), oma = c(0, 0, 0, 0))
#' set_hinge(x, hinge = 0, allow_bias = TRUE)(n) |> plot()
#' set_hinge(x, hinge = 0, allow_bias = FALSE)(n) |> plot()
#' set_hinge(x, hinge = 4, allow_bias = TRUE)(n) |> plot()
#' set_hinge(x, hinge = 4, allow_bias = FALSE)(n) |> plot()
#' par(op)

set_hinge <- function(x,
                      hinge,
                      scheme = "sunset",
                      alpha = NULL,
                      reverse = FALSE,
                      buffer = 0,
                      stops = c(0, 1),
                      allow_bias = TRUE,
                      nan = NA_character_) {

  x <- range(x, na.rm = TRUE)
  checkmate::assertNumeric(x,
    finite = TRUE,
    any.missing = FALSE,
    len = 2,
    unique = TRUE,
    sorted = TRUE
  )
  checkmate::assertNumber(hinge, finite = TRUE)
  checkmate::assertCharacter(scheme,
    min.chars = 1,
    any.missing = FALSE,
    min.len = 1,
    max.len = 2
  )
  nm <- names(schemes)[vapply(schemes, function(x) x$nmax == Inf, FALSE)]
  scheme <- match.arg(scheme, nm, several.ok = TRUE)
  checkmate::assertNumeric(alpha,
    lower = 0,
    upper = 1,
    finite = TRUE,
    any.missing = FALSE,
    min.len = 1,
    max.len = 2,
    null.ok = TRUE
  )
  checkmate::assertLogical(reverse,
    any.missing = FALSE,
    min.len = 1,
    max.len = 2
  )
  checkmate::assertFlag(allow_bias)
  checkmate::assertNumeric(buffer,
    lower = 0,
    upper = 1,
    finite = TRUE,
    any.missing = FALSE,
    min.len = 1,
    max.len = 2
  )
  checkmate::assertNumeric(stops,
    lower = 0,
    upper = 1,
    finite = TRUE,
    any.missing = FALSE,
    len = 2,
    unique = TRUE,
    sorted = TRUE
  )
  checkmate::assertString(nan,
    na.ok = TRUE,
    pattern = "^#(\\d|[a-f]){6,8}$",
    ignore.case = TRUE
  )

  scheme <- rep(scheme, length.out = 2)
  if (!is.null(alpha)) alpha <- rep(alpha, length.out = 2)
  reverse <- rep(reverse, length.out = 2)
  buffer <- rep(buffer, length.out = 2)

  if (x[1] < hinge & x[2] > hinge) {
    ratio <- diff(c(x[1], hinge)) / diff(x)
  } else if (x[1] < hinge) {
    ratio <- 1
  } else {
    ratio <- 0
  }

  ran <- ifelse(identical(scheme[1], scheme[2]), 0.5, 1)
  buf <- buffer * ran
  stp <- c(stops[1], 1 - stops[2]) * ran

  if (allow_bias || ratio %in% c(0, 0.5, 1)) {
    adj <- c(0, 0)
  } else {
    d1 <- diff(c(x[1], hinge))
    d2 <- diff(c(hinge, x[2]))
    if (d1 < d2) {
      adj <- c((ran - buf[1] - stp[1]) * (1 - d1 / d2), 0)
    } else {
      adj <- c(0, (ran - buf[2] - stp[2]) * (1 - d2 / d1))
    }
  }

  s1 <- c(stp[1] + adj[1], ran - buf[1])
  s2 <- c(1 - ran + buf[2], 1 - stp[2] - adj[2])

  if (s1[1] >= s1[2] || s2[1] >= s2[2]) {
    stop("Problem with color stops and (or) buffer values.")
  }

  # avoid duplicate colors at interface between schemes
  dup_fix <- ratio > 0 &&
    ratio < 1 &&
    identical(scheme[1], scheme[2]) &&
    identical(alpha[1], alpha[2]) &&
    identical(s1[2], s2[1]) &&
    identical(reverse[1], reverse[2])

  function(...) {
    n1 <- round(... * ratio)
    n2 <- ... - n1
    if (dup_fix) n1 <- n1 + 1L
    p1 <- get_colors(n1, scheme[1], alpha[1], stops = s1, reverse = reverse[1])
    p2 <- get_colors(n2, scheme[2], alpha[2], stops = s2, reverse = reverse[2])
    if (dup_fix) p1 <- utils::head(p1, -1)
    pal <- c(p1, p2)
    cl <- as.call(list(quote(set_hinge),
      "x" = x,
      "hinge" = hinge,
      "scheme" = unique(scheme),
      "alpha" = unique(alpha),
      "reverse" = unique(reverse),
      "buffer" = unique(buffer),
      "stops" = stops,
      "allow_bias" = allow_bias,
      "nan" = nan
    ))
    make_inlpal_class(pal, call = cl, nan = nan)
  }
}
