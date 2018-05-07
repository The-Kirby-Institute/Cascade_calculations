# Functions for generating results and plots

# Richard T. Gray

# This script contains functions useful for generating summary results 
# and plots for the HIV cascade calculations. These functions are primarily 
# used in the 4-HivCascadePlots.Rmd script but can be used separately. 

# Load required libraries for functions to work
library(tidyverse)
library(scales)

# Plotting functions ----------------------------------------------------------

#' Plot diagnosed people living with HIV by age results
#'
#' This function produces ggplot plot handles for the number and proportion of 
#' diagnosed people living with HIV by agebin.
#'
#' @details The idea of this function is to produce plots for the default HIV
#' cascade agebins (5 year bins: 0-4, 5-9, ...., 80-84, 85+). This function 
#' requires the libraries ggplot2 and scales and for the PlotOptions.R and 
#' PlotColors.R to be sourced. 
#'
#' @param pldhiv Dataframe in long format showing estimated number of people
#' living with diagnosed HIV by agebin and year. Column names must include year
#' agebin and value (for the estimates) 
#' @param startyear Year to start x-axis on plot. Optional and set to 1986 as 
#' default
#' @param plotcolors Vector of color strings for plots. Number of colors must 
#' equal the number of age bins. Optional and NULL by default. 
#' @param grayscale Logical sepcifying if grayscale should be used in the plots.
#' Overwrites the plotcolors argument if TRUE. Optional and set to FALSE by 
#' default
#'
#' @return List with plot handles for number and proportion by age
#' 
#' @author Richard T. Gray, \email{Rgray@kirby.unsw.edu.au}
#' 
PlotAgeCascade <- function(pldhivage, startyear = 1986, plotcolors = NULL,
  grayscale = FALSE) {
  
  # Setup data defaults
  ageResults <- pldhivage %>% filter(year >= startyear)
  nages <- length(unique(pldhivage$agebin))
  analysisYear <- max(pldhivage$year)
  
  # X-axis values for plotting
  xValues <- seq(startyear, analysisYear, by = 5)
  
  # Setup default colours
  if (grayscale) {
    plotcolors <- NULL # ignore colours if using grayscale
  }
  
  if(is.null(plotcolors)) {
    if (grayscale) {
      getPalette <- gray.colors(nages, start = 0, end = 0.95)
      plotcolors <- rev(getPalette)
    } else {
      palette <- PlotColors("crayons")
      colIndices <- round(seq(1, length(palette), length = nages))
      plotcolors <- unname(palette[colIndices])
    }
  } 
  
  # Produce bar plot of numbers
  barPlot <- ggplot(data = ageResults, aes(x = year, y = value, 
      fill = agebin)) + 
    geom_bar(stat = "identity") + 
    xlab("Year") + 
    ylab("Number diagnosed with HIV") +
    scale_fill_manual(values = plotcolors, name = "Age",
      guide = guide_legend(reverse=FALSE)) + 
    scale_x_continuous(breaks = xValues) + 
    PlotOptions() + 
    theme(legend.position = "right") #, 
      # legend.text = element_text(size = 12),
      # legend.title = element_text(size = 12),
      # plot.title = element_text(hjust = 0.5, size = 14),
      # axis.title.x = element_text(size=12),
      # axis.text.x = element_text(size=12),
      # axis.title.y = element_text(size=12),
      # axis.text.y = element_text(size=12))
  
  # Produce barplot of proportions
  propPlot <- ggplot(data = ageResults, aes(x = year, y = value, 
    fill = agebin)) + 
    geom_bar(stat="identity", position = "fill") + 
    xlab("Year") + 
    ylab("Proportion diagnosed with HIV (%)") +
    scale_fill_manual(values = plotcolors, name = "Age") +
    scale_y_continuous(label = percent) + 
    scale_x_continuous(breaks = xValues)  + 
    PlotOptions() +
    theme(legend.position = "right") #,
      # legend.text = element_text(size = 12), 
      # legend.title = element_text(size = 13), 
      # plot.title = element_text(hjust = 0.5, size = 12), 
      # axis.title.x = element_text(size=12),
      # axis.text.x = element_text(size=12), 
      # axis.title.y = element_text(size=12), 
      # axis.text.y = element_text(size=12))

  # Return plot handles
  return(list(barPlot, propPlot))
  
}
