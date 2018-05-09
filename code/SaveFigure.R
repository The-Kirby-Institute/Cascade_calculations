#' Wrapper function to save ggplots
#'
#' This is ggsave Wrapper function with defaiults so we don't have to keep
#' entering everything in ggsave.
#'
#' @param folder Character string for the folder where figure will be saved.
#' @param filename Character string specifiying the file name.
#' @param figure ggplot object.
#' @param format String specifying the file format of the figure. Default ".png".
#' @param width Saved plot width. Default is 15 cm.
#' @param height Saved plot height. Default 10 cm.
#' @param units String specifying units for plot dimension. Default centimetres.
#' @param ... Additional ggsave inputs.
#'
#' @return None. Saves file in specified directory.
#'
#' @author Richard T Gray, \email{Rgray@kirby.unsw.edu.au}
#'
#' @export
#' @importFrom ggplot2 ggsave
#'
SaveFigure <- function(folder, filename, figure,
                       format = ".png", width = 15,
                       height = 10, units = "cm", ...) {

  ggsave(file.path(folder, paste(filename, format, sep = "")),
         plot = figure,
         width = width, height = height, units = units, ...)
}
