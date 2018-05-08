#' Function to extract legend from a ggplot
#'
#' Function to extract the legend from a ggplot for manipulation in a
#' publication plot. The code was obtained from a forum referenced below.
#'
#' @param myggplot ggplot plot object
#'
#' @return Legend object as a separate plot object.
#'
#' @author Richard T Gray, \email{Rgray@kirby.unsw.edu.au}
#' @references \url{http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization#change-legend-position}
#'
#' @export
#' @import ggplot2
#'
GetLegend<-function(myggplot){

  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]

  return(legend)
}
