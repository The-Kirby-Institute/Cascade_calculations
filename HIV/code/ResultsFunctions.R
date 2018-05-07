## Functions for generating results and plots

# Richard T. Gray

# functions useful for generating summary results and plots for the cascade
# calculations. These functions are primarily used in the 
# 4-HivCascadePlots.Rmd script.

# Libraries for functions to work
library(tidyverse)
library(scales)
 
# Plotting functions ------------------------------------------------------

# PlotOptions.R and PlotColors.R need to be sourced for these functions to work 

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
      getPalette <- gray.colors(nrow(binPldhiv), start = 0, end = 0.95)
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
    ylab("Number HIV diagnosed") +
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
    ylab("Proportion HIV diagnosed (%)") +
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
  
  # Save results if requested?
  
  
  # Return plot handles
  return(list(barPlot, propPlot))
  
}
