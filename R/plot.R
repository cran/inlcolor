
#' Plot method for color palettes
#'
#' Plot a sequence of shaded rectangles showing colors in the palette.
#'
#' @param x 'inlpal' object that inherits behavior from the 'character' class.
#'   Palette colors represented in hexadecimal format.
#' @param ...
#'   Not used
#' @param label 'logical' flag.
#'   Whether to include the plot title.
#'
#' @return Invisibly returns `NULL`, called for side effect,
#'   plotting a color palette in an \R graphics device.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @exportS3Method plot inlpal
#'
#' @examples
#' get_colors(10) |> plot()
#'
#' set_hinge(c(-10, 10), hinge = 0)(20) |> plot()

plot.inlpal <- function(x, ..., label = TRUE) {

  checkmate::assert_class(x, "inlpal")
  checkmate::assertFlag(label)

  n <- length(x)

  if (label) {
    cl <- attr(x, "call")
    args <- as.list(cl)
    fun_nm <- rlang::call_name(cl)
    defaults <- lapply(formals(fun_nm), function(x) {
      if (inherits(x, "call")) eval(x) else x
    })
    txt <- switch(fun_nm,

      "get_colors" = c(
        n = sprintf("n = %d", n),
        scheme = sprintf("scheme = %s", args$scheme),
        alpha = sprintf("alpha = %s", args$alpha),
        stops = sprintf("stops = c(%s)", paste(args$stops, collapse = ", ")),
        bias = sprintf("bias = %s", args$bias),
        reverse = sprintf("reverse = %s", args$reverse),
        blind = sprintf("blind = %s", args$blind),
        gray = sprintf("gray = %s", args$gray)
      ),

      "set_hinge" = c(
        x = sprintf("x = c(%s)", paste(args$x, collapse = ", ")),
        hinge = sprintf("hinge = %s", args$hinge),
        scheme = sprintf("schema = c(%s)", paste(args$scheme, collapse = ", ")),
        alpha = sprintf("alpha = c(%s)", paste(args$alpha, collapse = ", ")),
        reverse = sprintf("reverse = c(%s)", paste(args$reverse, collapse = ", ")),
        buffer = sprintf("buffer = c(%s)", paste(args$buffer, collapse = ", ")),
        stops = sprintf("stops = c(%s)", paste(args$stops, collapse = ", ")),
        allow_bias = sprintf("allow_bias = %s", args$allow_bias),
        nan = sprintf("nan = %s", args$nan)
      )

    )
    is <- vapply(names(txt), function(nm) {
      !identical(args[[nm]], defaults[[nm]])
    }, logical(1))
    main <- txt[is] |> paste(collapse = ", ")
  } else {
    main <- NULL
  }

  # code adapted from example in
  # colorspace::rainbow_hcl function documentation,
  # authored by Achim Zeileis and accessed August 8, 2018
  # at https://CRAN.R-project.org/package=colorspace
  mar <- if (label) c(3, 2, 2, 2) else c(0, 0, 0, 0)
  op <- graphics::par(mar = mar)
  on.exit(graphics::par(op))
  graphics::plot.default(NA,
    type = "n",
    xlim = c(0, 1),
    ylim = c(0, 1),
    main = main,
    xaxs = "i",
    yaxs = "i",
    bty = "n",
    xaxt = "n",
    yaxt = "n",
    xlab = "",
    ylab = "",
    col.main = "#333333",
    ...
  )
  xl <- 0:(n - 1) / n
  xr <- 1:n / n
  if (any(grepl("^#(\\d|[a-f]){8}$", x, ignore.case = TRUE))) {
    graphics::rect(0, 0, 1, 1, col = "#FFFFFF", border = NA)
  } else if (n > 1) {
    xr <- c(
      utils::head(xr, -1) + 1 / (2 * n),
      utils::tail(xr, 1)
    )
  }
  graphics::rect(xl, 0, xr, 1, col = x, border = NA)
  if (label && n < 35) {
    at <- 0:(n - 1) / n + 1 / (2 * n)
    lab <- gsub(" ", "\n", names(x))
    if (length(lab) == 0) {
      lab <- seq_along(x)
    }
    graphics::axis(1,
      at = at,
      labels = lab,
      tick = FALSE,
      line = -0.5,
      padj = 1,
      mgp = c(3, 0, 0),
      col.lab = "#333333"
    )
    v <- (0:(n - 1) / n)[-1]
    graphics::abline(v = v, col = "#D3D3D3", lwd = 0.25)
  }
  graphics::box(lwd = 0.25, col = "#D3D3D3")

  invisible()
}
