# Function to read data from a single color palette (CPT) formatted file

read_cpt <- function(file, cite = NULL, type = "Sequential") {

  checkmate::assertString(file)
  checkmate::assertString(cite, null.ok = TRUE)
  type <- match.arg(type, c("Sequential", "Diverging", "Qualitative"))

  line <- readLines(file)
  line <- line[-grep("^(#$|#-+)", line)]

  nm <- c("COLOR_MODEL", "RANGE", "HINGE", "CYCLIC")
  option <- lapply(nm, function(opt) {
    idx <- sprintf("^#[ \t]%s", opt) |> grep(line)
    if (length(idx) == 0) {
      return(NULL)
    }
    x <- strsplit(line[idx], "[ \t]")[[1]] |> utils::tail(1)
    line <<- line[-idx]
    if (opt == "COLOR_MODEL") x <- toupper(x)
    if (opt == "RANGE") x <- as.numeric(strsplit(x, "/")[[1]])
    if (opt == "HINGE") x <- as.numeric(x)
    if (opt == "CYCLIC") x <- TRUE
    x
  })
  names(option) <- nm

  if (option$COLOR_MODEL != "RGB") {
    return(NULL)
  }

  color <- lapply(c("N" = "N", "B" = "B", "F" = "F"), function(key) {
    idx <- sprintf("^%s[ |\t]", key) |> grep(line)
    if (length(idx) == 0) {
      return(NULL)
    }
    x <- strsplit(line[idx], "[ \t]")[[1]] |> utils::tail(1)
    line <<- line[-idx]
    cpt2hex(x)
  })

  idx <- grep("^#[ \t]", line)
  note <- substring(line[idx], 3) |> strwrap(width = .Machine$integer.max)
  line <- line[-idx]

  m <- do.call("rbind", lapply(line, function(x) {
    x <- strsplit(x, "[ \t]")[[1]]
    x <- x[x != ""]
    if (length(x) == 4) {
      elem <- c(x[1], cpt2hex(x[2]), x[3], cpt2hex(x[4]))
    } else if (length(x) == 8) {
      elem <- c(x[1], cpt2hex(x[2:4]), x[5], cpt2hex(x[6:8]))
    } else {
      return(NULL)
    }
    elem
  }))

  if (is.null(m)) {
    return(NULL)
  }

  for (i in seq_len(nrow(m) - 1)) {
    if (!identical(m[i, 4], m[i + 1, 2])) {
      return(NULL)
    }
  }

  d <- rbind(m[, 1:2], m[nrow(m), 3:4]) |> as.data.frame(stringsAsFactors = FALSE)
  names(d) <- c("value", "color")
  d$value <- as.numeric(d$value)
  if (is.numeric(option$RANGE)) {
    if (is.numeric(option$HINGE)) {
      x <- c(-1, 0, 1)
      y <- c(option$RANGE[1], option$HINGE, option$RANGE[2])
      d$value <- stats::approx(x, y, xout = d$value)
    } else {
      d$value <- d$value * diff(option$RANGE) + option$RANGE[1]
    }
  }

  l <- list(
    "data" = d,
    "type" = type,
    "cite" = cite,
    "nmax" = Inf,
    "nan" = color$N,
    "back" = color$B,
    "fore" = color$F,
    "note" = note
  )
  l[vapply(l, is.null, FALSE)] <- NULL
  l
}


# Function to convert a color palette (CPT) to hexadecimal color

cpt2hex <- function(x) {
  checkmate::assertVector(x,
    strict = TRUE,
    any.missing = FALSE,
    min.len = 1,
    max.len = 3
  )
  if (length(x) == 1) {
    if (grepl("/", x)) {
      x <- as.integer(strsplit(x, "/")[[1]])
    } else {
      x <- t(grDevices::col2rgb(x))[1, ]
    }
  }
  grDevices::rgb(x[1], x[2], x[3], maxColorValue = 255)
}


# Function to read color palette (CPT) files in the Generic Mapping Tools (GMT) repo

get_cpt_gmt <- function(destdir) {

  # check access to httr package
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Read access to CPT files require the 'httr' package.", call. = FALSE)
  }

  # Generic Mapping Tools (GMT)
  cite <- "Wessel and others (2013)"

  # code adapted from stackoverflow answer by lukeA, accessed October 27, 2018
  # at https://stackoverflow.com/questions/25485216
  host <- "api.github.com"
  owner <- "GenericMappingTools"
  repo <- "gmt"
  fmt <- "https://%s/repos/%s/%s/git/trees/master?recursive=1"
  path <- sprintf(fmt, host, owner, repo)
  info <- httr::GET(sprintf(fmt, host, owner, repo))
  httr::stop_for_status(info)
  tree <- unlist(lapply(httr::content(info)$tree, "[", "path"), use.names = FALSE)
  path <- grep("share/cpt/.*cpt$", tree, value = TRUE)
  path <- path[!duplicated(basename(path))]
  host <- "raw.githubusercontent.com"
  file <- sprintf("https://%s/%s/%s/master/%s", host, owner, repo, path)

  dir.create(destdir, showWarnings = FALSE)
  destfile <- file.path(destdir, basename(file), fsep = "/")
  for (i in seq_along(file)) {
    utils::download.file(file[i], destfile[i], quiet = TRUE)
  }

  nm <- tools::file_path_sans_ext(basename(file))
  type <- rep("Sequential", length(nm))
  div <- c(
    "polar",
    "red2green",
    "romaO",
    "split"
  )
  type[nm %in% div] <- "Diverging"

  cpt <- lapply(seq_along(destfile), function(i) {
    read_cpt(destfile[i], cite = cite, type = type[i])
  })
  names(cpt) <- nm

  is <- !vapply(cpt, is.null, FALSE)
  cpt <- cpt[is]
  unlink(destfile[!is])

  cpt
}


# Function to validate color scheme

check_scheme <- function(x) {
  checkmate::assertDataFrame(x$data,
    any.missing = FALSE,
    min.rows = 2,
    min.cols = 1,
    null.ok = TRUE
  )
  pattern <- "^#(\\d|[a-f]){6}$"
  checkmate::assertCharacter(x$data$color,
    pattern = pattern,
    ignore.case = TRUE,
    null.ok = TRUE
  )
  is_color(x$data$color) |> all() |> stopifnot()

  checkmate::qassert(x$data$name, c("0", "S", "X(0,)"))
  checkmate::qassert(x$gray, c("0", "S", "X(0,)"))
  checkmate::assertSubset(x$gray, x$data$name)

  checkmate::assertNumeric(x$data$value,
    finite = TRUE, unique = TRUE,
    sorted = TRUE, null.ok = TRUE
  )

  checkmate::assertSubset(x$type, c("Qualitative", "Diverging", "Sequential"))
  checkmate::assertString(x$cite)
  checkmate::assertNumber(x$nmax)

  checkmate::assertCharacter(x$back, pattern = pattern, ignore.case = TRUE, null.ok = TRUE)
  checkmate::assertCharacter(x$fore, pattern = pattern, ignore.case = TRUE, null.ok = TRUE)
  checkmate::assertCharacter(x$nan, pattern = pattern, ignore.case = TRUE, null.ok = TRUE)
  stopifnot(is_color(x$back, null_ok = TRUE))
  stopifnot(is_color(x$fore, null_ok = TRUE))
  stopifnot(is_color(x$nan, null_ok = TRUE))

  checkmate::assertCharacter(x$note, null.ok = TRUE)

  invisible()
}


# Function to convert colors to gray/grayscale.

# Code from TeachingDemos::col2grey function,
# authored by Greg Snow and accessed August 29, 2018
# at https://CRAN.R-project.org/package=TeachingDemos
# and licensed under Artistic-2.0
# https://cran.r-project.org/web/licenses/Artistic-2.0
# Function integrated here without logical changes.

col2gray <- function(cols) {
  rgb <- grDevices::col2rgb(cols)
  gry <- rbind(c(0.3, 0.59, 0.11)) %*% rgb
  grDevices::rgb(gry, gry, gry, maxColorValue = 255)
}


# Function to print as LaTeX table

# Code adapted from inlmisc::print_xtableTable function at
# https://github.com/USGS-R/inlmisc/blob/main/R/print_xtableTable.R
# Accessed on 2022-02-23
# CC0 1.0 Universal Public Domain Dedication.

print_table <- function(d,
                        colheadings = NULL,
                        align = NULL,
                        digits = NULL,
                        label = NULL,
                        title = NULL,
                        headnotes = NULL,
                        footnotes = NULL,
                        nrec = nrow(d),
                        hline = NULL,
                        na = "\\textemdash",
                        rm_dup = NULL,
                        landscape = FALSE,
                        file = "",
                        ...) {

  stopifnot(inherits(d, c("matrix", "data.frame")))
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  checkmate::assertDataFrame(d, min.rows = 1, min.cols = 1)

  if (inherits(colheadings, c("matrix", "data.frame"))) {
    colheadings <- apply(colheadings, 2, as.character)
  } else {
    checkmate::assertCharacter(colheadings, len = ncol(d), null.ok = TRUE)
    if (is.null(colheadings)) colheadings <- colnames(d)
    colheadings[is.na(colheadings)] <- ""
    colheadings <- t(matrix(colheadings))
  }
  checkmate::assertMatrix(colheadings, ncols = ncol(d), min.rows = 1)

  checkmate::assertCharacter(align,
    any.missing = FALSE,
    min.len = 1,
    max.len = ncol(d),
    null.ok = TRUE
  )
  if (!is.null(align)) align <- rep(align, length.out = ncol(d))

  checkmate::assertIntegerish(digits,
    any.missing = FALSE,
    min.len = 1,
    max.len = ncol(d),
    null.ok = TRUE
  )
  if (!is.null(digits)) digits <- rep(digits, length.out = ncol(d))

  checkmate::assertString(label, null.ok = TRUE)
  checkmate::assertString(title, null.ok = TRUE)
  checkmate::assertString(headnotes, null.ok = TRUE)
  checkmate::assertString(footnotes, null.ok = TRUE)
  checkmate::assertIntegerish(nrec,
    lower = 1,
    any.missing = FALSE,
    min.len = 1,
    max.len = 2
  )
  checkmate::assertIntegerish(hline,
    lower = 1,
    upper = nrow(d) - 1,
    any.missing = FALSE,
    null.ok = TRUE
  )
  checkmate::assertString(na, null.ok = TRUE)
  checkmate::assertInt(rm_dup, lower = 1, upper = ncol(d), null.ok = TRUE)
  checkmate::assertFlag(landscape)

  font <- "\\normalfont\\bfseries\\sffamily"

  colheadings[is.na(colheadings)] <- ""
  if (nrow(colheadings) > 1) {
    colheadings <- apply(colheadings, 2, function(x) {
      x[duplicated(x)] <- ""
      is <- x == ""
      c(x[!is], x[is])
    })
  }

  cmd <- NULL
  for (i in seq_len(nrow(colheadings))) {
    x <- colheadings[i, 1]
    cols <- 1L
    for (j in seq_len(ncol(colheadings) - 1)) {
      n <- length(cols)
      if (x[n] == colheadings[i, j + 1]) {
        cols[n] <- cols[n] + 1L
      } else {
        x[n + 1] <- colheadings[i, j + 1]
        cols[n + 1] <- 1L
      }
    }

    line <- NULL
    rows <- rep(1L, length(x))
    if (i < nrow(colheadings)) {
      cnt <- 0L
      for (k in seq_along(x)) {
        idx <- cnt + 1L
        cnt <- cnt + cols[k]
        if (x[k] != "" & all(colheadings[i + 1, idx:cnt] == "")) {
          rows[k] <- nrow(colheadings) - i + 1L
        }
        if (rows[k] == 1 & cols[k] > 1) {
          line <- paste0(line, sprintf("\\cmidrule(lr){%d-%d}", idx, cnt))
        }
      }
    }

    is <- x != ""
    x[is] <- sprintf("{%s \\makecell{%s}}", font, x[is])

    is <- rows > 1
    fmt <- "\\multirow{%d}{*}[-0.5\\dimexpr \\aboverulesep + \\belowrulesep + \\cmidrulewidth]{%s}"
    x[is] <- sprintf(fmt, rows[is], x[is])

    x <- sprintf("\\multicolumn{%d}{c}{%s}", cols, x)

    cmd[i] <- paste0(paste(x, collapse = " & "), " \\\\ ", line, "\n")
  }

  if (methods::hasArg("include.rownames") && list(...)$include.rownames) {
    cmd <- paste("&", cmd)
  }

  add_to_row <- list()
  add_to_row$pos <- list(0)
  add_to_row$command <- paste(cmd, collapse = "")

  cap1 <- strwrap(title, width = .Machine$integer.max)
  cap2 <- strwrap(headnotes, width = .Machine$integer.max)

  idxs <- which(unlist(lapply(d, is.factor)))
  d[, idxs] <- lapply(d[, idxs, drop = FALSE], as.character)

  n <- nrow(d)
  if (n > nrec[1]) {
    if (length(nrec) == 1) nrec[2] <- nrec[1]
    n <- unique(c(cumsum(c(nrec[1], rep(nrec[2], (n - nrec[1]) %/% nrec[2]))), n))
  }

  print_xtable <- xtable::print.xtable
  formals(print_xtable)$file <- file
  formals(print_xtable)$type <- "latex"
  formals(print_xtable)$caption.placement <- "top"
  formals(print_xtable)$size <- "\\small"
  formals(print_xtable)$NA.string <- na
  formals(print_xtable)$include.colnames <- FALSE
  formals(print_xtable)$sanitize.text.function <- identity
  formals(print_xtable)$sanitize.colnames.function <- function(x) {
    x
  }
  formals(print_xtable)$include.rownames <- FALSE
  formals(print_xtable)$math.style.exponents <- TRUE
  formals(print_xtable)$format.args <- list("big.mark" = ",")
  formals(print_xtable)$booktabs <- TRUE
  formals(print_xtable)$comment <- FALSE

  for (i in seq_along(n)) {
    if (i > 1) cat("\n\\clearpage\n", file = file, append = TRUE)
    if (i == 2) cat("\\captionsetup[table]{list=no}\n", file = file, append = TRUE)
    if (landscape) cat("\\begin{landscape}\n", file = file, append = TRUE)

    if (i == 1) {
      idxs <- 1:n[i]
      caption <- c(sprintf("%s\\par \\medskip [\\footnotesize{%s}]", cap1, cap2), cap1)
    } else {
      idxs <- (n[i - 1] + 1):n[i]
      caption <- sprintf("%s---Continued", cap1)
      label <- NULL
      cat("\\addtocounter{table}{-1}\n", file = file, append = TRUE)
    }

    tbl <- d[idxs, ]
    if (!is.null(rm_dup)) {
      for (j in rev(seq_len(rm_dup))) {
        tbl[[j]][duplicated(tbl[, seq_len(j), drop = FALSE])] <- ""
      }
    }

    tbl <- xtable::xtable(tbl)
    if (length(caption) > 0) {
      xtable::caption(tbl) <- caption
      xtable::label(tbl) <- label
    }

    row_names <- utils::type.convert(rownames(d))
    row_align <- ifelse(is.numeric(row_names), "r", "l")
    row_digits <- ifelse(is.double(row_names), format.info(row_names)[2], 0)
    if (!is.null(align)) xtable::align(tbl) <- c(row_align, align)

    x <- switch(1 + is.null(digits),
      digits,
      rep(3, ncol(d))
    )
    xtable::digits(tbl) <- c(row_digits, x)

    hline_after <- sort(
      unique(
        stats::na.omit(c(-1, 0, match(c(hline, nrow(d)), idxs)))
      )
    )

    if (!is.null(footnotes) && i == length(n)) {
      fmt <- "\\midrule\n\\multicolumn{%s}{l}{\\makecell[l]{%s}} \\\\\n"
      add_to_row$command[2] <- sprintf(fmt, ncol(tbl), footnotes)
      add_to_row$pos[[2]] <- nrow(tbl)
      hline_after <- utils::head(hline_after, -1)
    }

    print_xtable(x = tbl, hline.after = hline_after, add.to.row = add_to_row, ...)

    if (landscape) {
      cat("\\end{landscape}\n", file = file, append = TRUE)
    }
    if (i > 1 && i == length(n)) {
      cat("\\captionsetup[table]{list=yes}\n", file = file, append = TRUE)
    }
  }

  invisible()
}
