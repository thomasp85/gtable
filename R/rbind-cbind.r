#' Row and column binding for gtables.
#'
#' @param ... gtables to combine (\code{x} and \code{y})
#' @param size How should the widths (for rbind) and the heights (for cbind)
#'   be combined across the gtables: take values from \code{first},
#'   or \code{last} gtable, or compute the \code{min} or \code{max} values.
#'   Defaults to \code{max}.
#' @param z A numeric vector indicating the relative z values of each gtable.
#'   The z values of each object in the resulting gtable will be modified
#'   to fit this order. If \code{NULL}, then the z values of obects within
#'   each gtable will not be modified.
#' @name bind
NULL

#' @rdname bind
#' @method rbind gtable
#' @export
rbind.gtable <- function(..., size = "max", z = NULL) {
  gtables <- list(...)
  if (!is.null(z)) {
    gtables <- z_arrange_gtables(gtables, z)
  }
  Reduce(function(x, y) rbind_gtable(x, y, size = size), gtables)
}

rbind_gtable <- function(x, y, size = "max") {
  stopifnot(ncol(x) == ncol(y))
  x_row <- nrow(x)
  y_row <- nrow(y)
  if (x_row == 0) return(y)
  if (y_row == 0) return(x)

  lay_x <- unclass(x$layout)
  lay_y <- unclass(y$layout)

  x$layout <- new_data_frame(
    t = c(lay_x$t, lay_y$t + x_row),
    l = c(lay_x$l, lay_y$l),
    b = c(lay_x$b, lay_y$b + x_row),
    r = c(lay_x$r, lay_y$r),
    z = c(lay_x$z, lay_y$z),
    clip = c(lay_x$clip, lay_y$clip),
    name = c(lay_x$name, lay_y$name)
  )

  x$heights <- insert.unit(x$heights, y$heights)
  x$rownames <- c(x$rownames, y$rownames)

  size <- match.arg(size, c("first", "last", "max", "min"))
  x$widths <- switch(size,
    first = x$widths,
    last = y$widths,
    min = compare_unit(x$widths, y$widths, pmin),
    max = compare_unit(x$widths, y$widths, pmax)
  )

  x$grobs <- append(x$grobs, y$grobs)

  x
}

#' @rdname bind
#' @method cbind gtable
#' @export
cbind.gtable <- function(..., size = "max", z = NULL) {
  gtables <- list(...)
  if (!is.null(z)) {
    gtables <- z_arrange_gtables(gtables, z)
  }
  Reduce(function(x, y) cbind_gtable(x, y, size = size), gtables)
}

cbind_gtable <- function(x, y, size = "max") {
  stopifnot(nrow(x) == nrow(y))
  x_col <- ncol(x)
  y_col <- ncol(y)
  if (x_col == 0) return(y)
  if (y_col == 0) return(x)

  lay_x <- unclass(x$layout)
  lay_y <- unclass(y$layout)

  x$layout <- new_data_frame(
    t = c(lay_x$t, lay_y$t),
    l = c(lay_x$l, lay_y$l + x_col),
    b = c(lay_x$b, lay_y$b),
    r = c(lay_x$r, lay_y$r + x_col),
    z = c(lay_x$z, lay_y$z),
    clip = c(lay_x$clip, lay_y$clip),
    name = c(lay_x$name, lay_y$name)
  )

  x$widths <- insert.unit(x$widths, y$widths)
  x$colnames <- c(x$colnames, y$colnames)

  size <- match.arg(size, c("first", "last", "max", "min"))
  x$heights <- switch(size,
    first = x$heights,
    last = y$heights,
    min = compare_unit(x$heights, y$heights, pmin),
    max = compare_unit(x$heights, y$heights, pmax)
  )

  x$grobs <- append(x$grobs, y$grobs)

  x
}
