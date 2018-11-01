#' Remove a grob from a gtable
#'
#' This function takes grob indexes or names and remove them from the gtable.
#'
#' @param x A gtable object
#' @param grob Either a character or integer vector identifying grobs to remove
#'
#' @return A gtable without the specified grobs
#'
#' @export
#'
gtable_remove_grob <- function(x, grob) {
  if (!is.gtable(x)) stop("x must be a gtable", call. = FALSE)
  if (is.character(grob)) {
    grob <- match(grob, .subset2(x$layout, "name"))
  }
  if (anyNA(grob) || any(grob > length(x))) {
    stop("grob must reference existing grob names or indices", call. = FALSE)
  }
  x$grobs <- x$grobs[-grob]
  x$layout <- new_data_frame(list(
    t = .subset2(x$layout, "t")[-grob],
    l = .subset2(x$layout, "l")[-grob],
    b = .subset2(x$layout, "b")[-grob],
    r = .subset2(x$layout, "r")[-grob],
    z = .subset2(x$layout, "z")[-grob],
    clip = .subset2(x$layout, "clip")[-grob],
    name = .subset2(x$layout, "name")[-grob]
  ))
  x
}
