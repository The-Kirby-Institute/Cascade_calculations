## Functions for generating results and plots

# Richard T. Gray

# functions useful for generating summary results and plots for the cascade
# calculations. These functions are primarily used in the 
# 4-HivCascadePlots.Rmd script.

 
# Plotting functions ------------------------------------------------------

# PlotOptions.R needs to be sourced for these functions to work 
# Library ggplot2/tidyverse is also required. 

PlotAgeCascade <- function(pldhivage, startyear = 1986, plotcolors = NULL, 
  grayscale = FALSE, ) {
  
  ageResults <- filter(pldhivage, year >= startyear)

  barPlotAll <- ggplot(data = ageResults, aes(x = year, y = number, 
    fill = age)) + geom_bar(stat = "identity") + 
    xlab("Year") + ylab("Number of people living with diagnosed HIV") +
    scale_fill_manual(values = mainColours, name = "Age",
      guide = guide_legend(reverse=FALSE)) + 
    scale_x_continuous(breaks = xValues) + 
    plotOpts + ggtitle("Age distribution of people living with diagnosed HIV\n over time") + theme(legend.position = "right",
      legend.text = element_text(size = 12), legend.title = element_text(size = 12), plot.title = element_text(hjust = 0.5, size = 14), axis.title.x = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=12), axis.text.y = element_text(size=12))
  
  # Now do the barplot of proportions
  propPlotAll <- ggplot(data = pldhivTidy, aes(x = year, y = number, 
    fill = age)) + geom_bar(stat="identity", position = "fill") + 
    xlab("Year") + ylab("Proportion of people living with diagnosed HIV") +
    scale_fill_manual(values = mainColours, name = "Age") + 
    scale_x_continuous(breaks = xValues)  + ggtitle("Age distribution of people living with diagnosed HIV over time") + 
    plotOpts + theme(legend.position = "right",
      legend.text = element_text(size = 12), legend.title = element_text(size = 13), plot.title = element_text(hjust = 0.5, size = 12), axis.title.x = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=12), axis.text.y = element_text(size=12))
  
  
  
}
