# setup tinytest for checkmate functionality
library("tinytest")
library("checkmate")
using("checkmate")

# test retrieval of color palettes
n <- 10L
x <- get_colors(n = n)
expect_character(x, len = n, any.missing = FALSE, unique = TRUE)

# test setting the hinge location in color palette
n <- 19L
fun <- set_hinge(x = c(-3, 7), hinge = 0)
x <- fun(n)
expect_character(x, len = n, any.missing = FALSE, unique = TRUE)
