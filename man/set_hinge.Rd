% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_hinge.R
\name{set_hinge}
\alias{set_hinge}
\title{Set hinge location in color palette}
\usage{
set_hinge(
  x,
  hinge,
  scheme = "sunset",
  alpha = NULL,
  reverse = FALSE,
  buffer = 0,
  stops = c(0, 1),
  allow_bias = TRUE,
  nan = NA_character_
)
}
\arguments{
\item{x}{'numeric' object that can be passed to the \code{\link{range}}
function with \code{NA}'s removed.
The user's data range.}

\item{hinge}{'numeric' number.
Hinge value (such as, at sea-level) in data units.}

\item{scheme}{'character' vector of length 1 or 2, value is recycled as necessary.
Name of color scheme(s).
The color palette is derived from one or two color schemes.
The scheme(s) must be suitable for continuous data types and allow for color interpolation.
See \code{\link{get_colors}} function for a list of possible scheme names.
Argument choices may be abbreviated as long as there is no ambiguity.}

\item{alpha}{'numeric' vector of length 1 or 2, value is recycled as necessary.
Alpha transparency applied separately on either side of the hinge.
Values range from 0 (fully transparent) to 1 (fully opaque).
Specify as \code{NULL} to exclude the alpha channel value from colors.}

\item{reverse}{'logical' vector of length 1 or 2, value is recycled as necessary.
Whether to reverse the order of colors in the scheme(s).
Values applied separately on either side of the hinge.}

\item{buffer}{'numeric' vector of length 1 or 2, value is recycled as necessary.
Color buffer around the hinge measured as a fraction of the color range.
Values applied separately on either side of the hinge.}

\item{stops}{'numeric' vector of length 2.
Color stops defined by interval endpoints (between 0 and 1)
and used to select a subset of the color palette(s).}

\item{allow_bias}{'logical' flag.
Whether to allow bias in the color spacing.}

\item{nan}{'character' string.
Color meant for missing data, in hexadecimal format,
where \code{NA} indicates no color is specified.}
}
\value{
A 'function' that takes an 'integer' argument (the required number of colors)
and returns a vector of colors of class '\link[=make_inlpal_class]{inlpal}'.
}
\description{
The \emph{hinge} indicates a dramatic color change in a palette
that is typically located at the midpoint of the data range.
An asymmetrical data range can result in an undesired hinge location,
a location that does not necessarily coincide with the break-point in the user's data.
This function can be used to specify a hinge location that is appropriate for your data.
}
\examples{
f <- set_hinge(c(-3, 7), hinge = 0)
f(n = 19) |> plot()

x <- datasets::volcano
f <- set_hinge(x, hinge = 140, scheme = c("abyss", "dem1"))
filled.contour(x, color.palette = f, nlevels = 50, plot.axes = FALSE)

# Data range (x)
hinge <- 0
n <- 20
op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
set_hinge(c(-10, 0), hinge)(n) |> plot()
set_hinge(c(-7, 3), hinge)(n) |> plot()
set_hinge(c(-5, 5), hinge)(n) |> plot()
set_hinge(c(-3, 7), hinge)(n) |> plot()
set_hinge(c(0, 10), hinge)(n) |> plot()
par(op)

# Hinge value (hinge)
x <- c(-5, 5)
n <- 255
op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
set_hinge(x, hinge = -6)(n) |> plot()
set_hinge(x, hinge = -2)(n) |> plot()
set_hinge(x, hinge = 0)(n) |> plot()
set_hinge(x, hinge = 2)(n) |> plot()
set_hinge(x, hinge = 6)(n) |> plot()
par(op)

# Color scheme (scheme)
x <- c(-10, 10)
hinge <- -3
n <- 255
op <- par(mfrow = c(3, 1), oma = c(0, 0, 0, 0))
set_hinge(x, hinge, scheme = "romaO")(n) |> plot()
set_hinge(x, hinge, scheme = "BuRd")(n) |> plot()
set_hinge(x, hinge, scheme = c("ocean", "copper"))(n) |> plot()
par(op)

# Alpha transparency (alpha)
x <- c(-5, 5)
hinge <- 0
scheme <- c("drywet", "hawaii")
n <- 255
op <- par(mfrow = c(4, 1), oma = c(0, 0, 0, 0))
set_hinge(x, hinge, scheme, alpha = 1.0)(n) |> plot()
set_hinge(x, hinge, scheme, alpha = 0.5)(n) |> plot()
set_hinge(x, hinge, scheme, alpha = c(1.0, 0.5))(n) |> plot()
set_hinge(x, hinge, scheme, alpha = c(0.5, 1.0))(n) |> plot()
par(op)

# Reverse colors (reverse)
x <- c(-10, 10)
hinge <- -3
n <- 255
op <- par(mfrow = c(6, 1), oma = c(0, 0, 0, 0))
set_hinge(x, hinge, "romaO", reverse = FALSE)(n) |> plot()
set_hinge(x, hinge, "romaO", reverse = TRUE)(n) |> plot()
set_hinge(x, hinge, c("davos", "hawaii"), reverse = FALSE)(n) |> plot()
set_hinge(x, hinge, c("davos", "hawaii"), reverse = TRUE)(n) |> plot()
set_hinge(x, hinge, c("davos", "hawaii"), reverse = c(TRUE, FALSE))(n) |> plot()
set_hinge(x, hinge, c("davos", "hawaii"), reverse = c(FALSE, TRUE))(n) |> plot()
par(op)

# Buffer around hinge (buffer)
x <- c(-5, 5)
hinge <- -2
n <- 20
op <- par(mfrow = c(6, 1), oma = c(0, 0, 0, 0))
set_hinge(x, hinge, buffer = 0.0)(n) |> plot()
set_hinge(x, hinge, buffer = 0.2)(n) |> plot()
set_hinge(x, hinge, buffer = c(0.4, 0.2))(n) |> plot()
set_hinge(x, hinge, c("gray", "plasma"), buffer = 0.0)(n) |> plot()
set_hinge(x, hinge, c("gray", "plasma"), buffer = 0.2)(n) |> plot()
set_hinge(x, hinge, c("gray", "plasma"), buffer = c(0.2, 0.4))(n) |> plot()
par(op)

# Color stops (stops)
x <- c(-5, 5)
hinge <- 1
n <- 20
op <- par(mfrow = c(6, 1), oma = c(0, 0, 0, 0))
set_hinge(x, hinge, stops = c(0.0, 1.0))(n) |> plot()
set_hinge(x, hinge, stops = c(0.2, 0.8))(n) |> plot()
set_hinge(x, hinge, stops = c(0.4, 0.6))(n) |> plot()
set_hinge(x, hinge, c("gray", "plasma"), stops = c(0.0, 1.0))(n) |> plot()
set_hinge(x, hinge, c("gray", "plasma"), stops = c(0.2, 0.8))(n) |> plot()
set_hinge(x, hinge, c("gray", "plasma"), stops = c(0.4, 0.6))(n) |> plot()
par(op)

# Allow bias (allow_bias)
x <- c(-3, 7)
n <- 20
op <- par(mfrow = c(4, 1), oma = c(0, 0, 0, 0))
set_hinge(x, hinge = 0, allow_bias = TRUE)(n) |> plot()
set_hinge(x, hinge = 0, allow_bias = FALSE)(n) |> plot()
set_hinge(x, hinge = 4, allow_bias = TRUE)(n) |> plot()
set_hinge(x, hinge = 4, allow_bias = FALSE)(n) |> plot()
par(op)
}
\seealso{
\code{\link[=plot.inlpal]{plot}} method for drawing color palettes.
}
\author{
J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
}
\keyword{color}
