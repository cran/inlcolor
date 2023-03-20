#' Get color palette
#'
#' Create a vector of `n` colors from qualitative, diverging, and sequential color schemes.
#'
#' @param n 'integer' count.
#'   Number of colors to be in the palette.
#'   The maximum number of colors in a generated palette is dependent on the specified color scheme,
#'   see 'Details' section for maximum values.
#' @param scheme 'character' string.
#'   Name of color scheme, see 'Details' section for scheme descriptions.
#'   Argument choices may be abbreviated as long as there is no ambiguity.
#' @param alpha 'numeric' number.
#'   Alpha transparency, values range from 0 (fully transparent) to 1 (fully opaque).
#'   Specify as `NULL` to exclude the alpha channel value from colors.
#' @param stops 'numeric' vector of length 2.
#'   Color stops defined by interval endpoints (between 0 and 1)
#'   and used to select a subset of the color palette.
#'   Only suitable for schemes that allow for color interpolations.
#' @param bias 'numeric' number.
#'   Interpolation bias where larger values result in more widely spaced colors at the high end.
#' @param reverse 'logical' flag.
#'   Whether to reverse the order of colors in the scheme.
#' @param blind 'character' string.
#'   Type of color blindness to simulate: specify `"deutan"` for green-blind vision,
#'   `"protan"` for red-blind vision, `"tritan"` for green-blue-blind vision, or
#'   `"monochrome"` for total-color blindness.
#'   A partial-color blindness simulation requires that the **dichromat** package is available,
#'   see [`dichromat::dichromat`] function for additional information.
#'   Argument choices may be abbreviated as long as there is no ambiguity.
#' @param gray 'logical' flag.
#'   Whether to subset/reorder the `"bright"`, `"high-contrast"`, `"vibrant"`,
#'   and `"muted"` schemes to work well after conversion to gray scale.
#' @param ...
#'   Not used
#'
#' @details The suggested data type for color schemes and the
#'   characteristics of generated palettes are given in the tables below.
#'   \[**Type**: is the type of data being represented,
#'   either qualitative, diverging, or sequential.
#'   **Max n**: is the maximum number of colors in a generated palette.
#'   And the maximum `n` value when scheme colors are designed for
#'   gray-scale conversion is enclosed in parentheses.
#'   A value of infinity indicates that the scheme allows for color interpolations.
#'   **N**: is the not-a-number color.
#'   **B**: is the background color.
#'   **F**: is the foreground color.
#'   **Abbreviations**: --, not available]
#'
#'   \if{html}{\figure{table01.svg}}
#'   \if{latex}{\figure{table01.pdf}{options: width=5.36in}}
#'
#'   \if{html}{\figure{table02.svg}}
#'   \if{latex}{\figure{table02.pdf}{options: width=5.36in}}
#'
#'   \if{html}{\figure{table03.svg}}
#'   \if{latex}{\figure{table03.pdf}{options: width=5.36in}}
#'
#'   \if{html}{\figure{table04.svg}}
#'   \if{latex}{\figure{table04.pdf}{options: width=5.36in}}
#'
#'   \if{html}{\figure{table05.svg}}
#'   \if{latex}{\figure{table05.pdf}{options: width=5.36in}}
#'
#'   Schemes `"pale"`, `"dark"`, and `"ground cover"` are
#'   intended to be accessed in their entirety and subset using vector element names.
#'
#' @return When argument `n` is specified, the function
#'   returns an object of class '[inlpal][make_inlpal_class]'.
#'   When `n` is unspecified a variant of the `get_colors` function is
#'   returned that has default argument values set equal to the values specified by the user.
#'
#' @note Sequential color schemes `"YlOrBr"` and `"iridescent"`
#'   work well for conversion to gray scale.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @references
#'   Dewez, Thomas, 2004, Variations on a DEM palette, accessed October 15, 2018 at
#'   <http://soliton.vm.bytemark.co.uk/pub/cpt-city/td/index.html>
#'
#'   Mikhailov, Anton, 2019, Turbo, an improved rainbow colormap for visualization:
#'   Google AI Blog, accessed August 21, 2019 at
#'   <https://ai.googleblog.com/2019/08/turbo-improved-rainbow-colormap-for.html>.
#'
#'   Tol, Paul, 2018, Colour Schemes:
#'   SRON Technical Note, doc. no. SRON/EPS/TN/09-002, issue 3.1, 20 p.,
#'   accessed September 24, 2018 at <https://personal.sron.nl/~pault/data/colourschemes.pdf>.
#'
#'   Wessel, P., Smith, W.H.F., Scharroo, R., Luis, J.F., and Wobbe, R., 2013,
#'   Generic Mapping Tools: Improved version released, AGU, v. 94, no. 45, p. 409--410
#'   \doi{10.1002/2013EO450001}.
#'
#' @seealso [`plot`][plot.inlpal] method for drawing color palettes.
#' @seealso [`set_hinge`] function to set the hinge location in
#'   a color palette derived from one or two color schemes.
#' @seealso [`grDevices::col2rgb`] function to express palette
#'   colors represented in the hexadecimal format as RGB triplets (R, G, B).
#'
#' @keywords color
#'
#' @export
#'
#' @examples
#' pal <- get_colors(n = 10)
#' print(pal)
#' plot(pal)
#'
#' get_pal <- get_colors(scheme = "turbo")
#' formals(get_pal)
#' filled.contour(datasets::volcano,
#'   color.palette = get_pal,
#'   plot.axes = FALSE
#' )
#'
#' # Diverging color schemes (scheme)
#' op <- par(mfrow = c(6, 1), oma = c(0, 0, 0, 0))
#' get_colors(9, scheme = "BuRd") |> plot()
#' get_colors(255, scheme = "BuRd") |> plot()
#' get_colors(9, scheme = "PRGn") |> plot()
#' get_colors(255, scheme = "PRGn") |> plot()
#' get_colors(11, scheme = "sunset") |> plot()
#' get_colors(255, scheme = "sunset") |> plot()
#' par(op)
#'
#' # Qualitative color schemes (scheme)
#' op <- par(mfrow = c(7, 1), oma = c(0, 0, 0, 0))
#' get_colors(7, scheme = "bright") |> plot()
#' get_colors(6, scheme = "dark") |> plot()
#' get_colors(5, scheme = "high-contrast") |> plot()
#' get_colors(9, scheme = "light") |> plot()
#' get_colors(9, scheme = "muted") |> plot()
#' get_colors(6, scheme = "pale") |> plot()
#' get_colors(7, scheme = "vibrant") |> plot()
#' par(op)
#'
#' # Sequential color schemes (scheme)
#' op <- par(mfrow = c(7, 1), oma = c(0, 0, 0, 0))
#' get_colors(23, scheme = "discrete rainbow") |> plot()
#' get_colors(34, scheme = "smooth rainbow") |> plot()
#' get_colors(255, scheme = "smooth rainbow") |> plot()
#' get_colors(9, scheme = "YlOrBr") |> plot()
#' get_colors(255, scheme = "YlOrBr") |> plot()
#' get_colors(23, scheme = "iridescent") |> plot()
#' get_colors(255, scheme = "iridescent") |> plot()
#' par(op)
#'
#' # Alpha transparency (alpha)
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
#' get_colors(34, alpha = 1.0) |> plot()
#' get_colors(34, alpha = 0.8) |> plot()
#' get_colors(34, alpha = 0.6) |> plot()
#' get_colors(34, alpha = 0.4) |> plot()
#' get_colors(34, alpha = 0.2) |> plot()
#' par(op)
#'
#' # Color stops (stops)
#' op <- par(mfrow = c(4, 1), oma = c(0, 0, 0, 0))
#' get_colors(255, stops = c(0.0, 1.0)) |> plot()
#' get_colors(255, stops = c(0.0, 0.5)) |> plot()
#' get_colors(255, stops = c(0.5, 1.0)) |> plot()
#' get_colors(255, stops = c(0.3, 0.9)) |> plot()
#' par(op)
#'
#' # Interpolation bias (bias)
#' op <- par(mfrow = c(7, 1), oma = c(0, 0, 0, 0))
#' get_colors(255, bias = 0.4) |> plot()
#' get_colors(255, bias = 0.6) |> plot()
#' get_colors(255, bias = 0.8) |> plot()
#' get_colors(255, bias = 1.0) |> plot()
#' get_colors(255, bias = 1.2) |> plot()
#' get_colors(255, bias = 1.4) |> plot()
#' get_colors(255, bias = 1.6) |> plot()
#' par(op)
#'
#' # Reverse colors (reverse)
#' op <- par(
#'   mfrow = c(2, 1),
#'   oma = c(0, 0, 0, 0),
#'   cex = 0.7
#' )
#' get_colors(10, reverse = FALSE) |> plot()
#' get_colors(10, reverse = TRUE) |> plot()
#' par(op)
#'
#' # Color blindness (blind)
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
#' get_colors(34, blind = NULL) |> plot()
#' get_colors(34, blind = "deutan") |> plot()
#' get_colors(34, blind = "protan") |> plot()
#' get_colors(34, blind = "tritan") |> plot()
#' get_colors(34, blind = "monochrome") |> plot()
#' par(op)
#'
#' # Gray-scale preparation (gray)
#' op <- par(mfrow = c(8, 1), oma = c(0, 0, 0, 0))
#' get_colors(3, "bright", gray = TRUE) |> plot()
#' get_colors(3, "bright", gray = TRUE, blind = "monochrome") |> plot()
#' get_colors(5, "high-contrast", gray = TRUE) |> plot()
#' get_colors(5, "high-contrast", gray = TRUE, blind = "monochrome") |> plot()
#' get_colors(4, "vibrant", gray = TRUE) |> plot()
#' get_colors(4, "vibrant", gray = TRUE, blind = "monochrome") |> plot()
#' get_colors(5, "muted", gray = TRUE) |> plot()
#' get_colors(5, "muted", gray = TRUE, blind = "monochrome") |> plot()
#' par(op)

get_colors <- function(n,
                       scheme = "smooth rainbow",
                       alpha = NULL,
                       stops = c(0, 1),
                       bias = 1,
                       reverse = FALSE,
                       blind = NULL,
                       gray = FALSE,
                       ...) {

  if (!missing(n)) {
    checkmate::assertCount(n)
    if (n == 0) return(NULL)
  }
  checkmate::assertFlag(gray)

  scheme <- match.arg(scheme, names(schemes))
  s <- schemes[[scheme]]
  nmax <- if (gray) length(s$gray) else s$nmax

  if (!missing(n) && n > nmax) {
    stop(
      "n = ", n, " exceeds the maximum number of colors in palette: ",
      nmax, " for '", scheme, "' scheme."
    )
  }
  if (gray && nmax == 0) {
    stop("gray component not available for '", scheme, "' scheme.")
  }

  checkmate::assertNumber(alpha, lower = 0, upper = 1, finite = TRUE, null.ok = TRUE)

  checkmate::assertNumeric(stops,
    lower = 0,
    upper = 1,
    finite = TRUE,
    any.missing = FALSE,
    len = 2,
    unique = TRUE,
    sorted = TRUE
  )
  checkmate::qassert(bias, "N1(0,)")
  checkmate::assertFlag(reverse)
  checkmate::assertString(blind, min.chars = 1, null.ok = TRUE)

  if (is.character(blind)) {
    blind <- match.arg(blind, c("deutan", "protan", "tritan", "monochrome"))
    if (blind != "monochrome" && !requireNamespace("dichromat", quietly = TRUE)) {
      stop("simulating partial color blindness requires the dichromat package")
    }
  }

  if (nmax < Inf && !identical(stops, c(0, 1))) {
    warning("'stops' only applies to interpolated color schemes")
  }

  if (missing(n)) {
    get_pal <- get_colors
    formals(get_pal) <- eval(substitute(
      alist(
        "n" = ,
        "scheme" = a1,
        "alpha" = a2,
        "stops" = a3,
        "bias" = a4,
        "reverse" = a5,
        "blind" = a6,
        "gray" = a7
      ),
      list(
        "a1" = scheme,
        "a2" = alpha,
        "a3" = stops,
        "a4" = bias,
        "a5" = reverse,
        "a6" = blind,
        "a7" = gray
      )
    ))
    return(get_pal)
  }

  color <- s$data$color
  names(color) <- s$data$name
  if (gray) color <- color[s$gray]

  if (scheme == "turbo") {

    # code adapted from turbo colormap look-up table;
    # changes include: add 'bias' variable, convert from Python to R,
    # and store parsed 'turbo_colormap_data' in R/sysdata.rda,
    # copyright 2019 Google LLC, Apache-2.0 license,
    # authored by Anton Mikhailov and accessed August 21, 2019
    # at https://gist.github.com/mikhailov-work/ee72ba4191942acecc03fe6da94fc73f
    x <- seq.int(stops[1], stops[2], length.out = n)^bias
    x <- vapply(x, function(y) max(0, min(1, y)), 0)
    a <- floor(x * 255)
    b <- vapply(a, function(y) min(255, y + 1), 0)
    f <- x * 255 - a
    a <- a + 1
    b <- b + 1
    pal <- grDevices::rgb(
      turbo_colormap_data[a, 1] + (turbo_colormap_data[b, 1] - turbo_colormap_data[a, 1]) * f,
      turbo_colormap_data[a, 2] + (turbo_colormap_data[b, 2] - turbo_colormap_data[a, 2]) * f,
      turbo_colormap_data[a, 3] + (turbo_colormap_data[b, 3] - turbo_colormap_data[a, 3]) * f
    )

    if (reverse) pal <- rev(pal)
  } else if (scheme == "discrete rainbow") {
    pal <- color[discrete_rainbow_indexes[[n]]]
    if (reverse) pal <- rev(pal)
  } else if (scheme == "bpy") {

    # code adapted from sp::bpy.colors function,
    # authored by Edzer Pebesma and accessed June 4, 2019
    # at https://CRAN.R-project.org/package=sp
    x <- seq.int(stops[1], stops[2], length.out = n)^bias
    r <- ifelse(x < 0.25, 0, ifelse(x < 0.57, x / 0.32 - 0.78125, 1))
    g <- ifelse(x < 0.42, 0, ifelse(x < 0.92, 2 * x - 0.84, 1))
    b <- ifelse(x < 0.25, 4 * x,
      ifelse(x < 0.42, 1, ifelse(x < 0.92, -2 * x + 1.84, x / 0.08 - 11.5))
    )
    pal <- grDevices::rgb(r, g, b)

    if (reverse) pal <- rev(pal)
  } else if (nmax < Inf) {
    if (reverse) color <- rev(color)
    pal <- color[seq_len(n)]
  } else {
    value <- if (is.null(s$data$value)) seq_along(s$data$color) else s$data$value
    value <- scales::rescale(value)
    if (reverse) {
      color <- rev(color)
      value <- rev(1 - value)
    }
    x <- seq.int(stops[1], stops[2], length.out = 255)
    color <- scales::gradient_n_pal(color, values = value)(x)
    pal <- grDevices::colorRampPalette(color, bias = bias, space = "Lab")(n)
  }

  nan <- ifelse(is.null(s$nan), NA_character_, s$nan)

  if (!is.null(blind) || !is.null(alpha)) {
    pal_names <- names(pal)
    if (!is.null(blind)) {
      if (blind == "monochrome") {
        pal <- col2gray(pal)
        if (!is.null(nan)) nan <- col2gray(nan)
      } else {
        pal <- dichromat::dichromat(pal, type = blind)
        if (!is.null(nan)) nan <- dichromat::dichromat(nan, type = blind)
      }
    }
    if (!is.null(alpha)) {
      pal <- grDevices::adjustcolor(pal, alpha.f = alpha)
      if (!is.null(nan)) nan <- grDevices::adjustcolor(nan, alpha.f = alpha)
    }
    names(pal) <- pal_names
  }

  cl <- as.call(list(quote(get_colors),
    "n" = n,
    "scheme" = scheme,
    "alpha" = alpha,
    "stops" = stops,
    "bias" = bias,
    "reverse" = reverse,
    "blind" = blind,
    "gray" = gray
  ))

  make_inlpal_class(pal, call = cl, nan = nan)
}
