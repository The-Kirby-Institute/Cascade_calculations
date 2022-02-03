#' Function to add empty values into a vector
#'
#' This is particualrly useful function for placing tick marks between labels
#' in ggplot objects
#' @param x vector
#' @param nth specify which elements are removed, i.e., every nth
#' @param empty logical specifying where elements are dropped or replaced 
#' @param inverse logical specifiying whether to drop every nth (FALSE; default)
#'   or save every nth (TRUE)
#'
#' @return Vector with every nth value removed or made into an empty string
#'
#' @references \url{https://stackoverflow.com/questions/34533472/insert-blanks-into-a-vector-for-e-g-minor-tick-labels-in-r}
#'
#' @export
EveryNth <- function(x, nth, empty = TRUE, inverse = FALSE) {
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}
