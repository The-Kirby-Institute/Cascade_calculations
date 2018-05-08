# Functions for generating results and plots

# Richard T. Gray

# This script contains functions useful for generating summary results 
# and plots for the HIV cascade calculations. These functions are primarily 
# used in the 4-HivCascadePlots.Rmd script but can be used separately. 

# Load required libraries for functions to work
library(tidyverse)
library(scales)

# Plotting functions ------------------------------------------------------

#' Plot number of PLDHIV over time
#' 
#' This function produces a plot of the number of people living with 
#' diagnosed HIV over time for an inputed set of scenarios.
#' 
#' @details The idea of this function is to produce plots for the  
#' 
#' @param pldhiv Dataframe in long format showing estimated number of 
#' people living with diagnosed HIV each year for each set. Column names
#'  must include year agebin, set and pldhiv (for the estimates).
#' 
#' @return 
#' 
#' @author Richard T. Gray, \email{Rgray@kirby.unsw.edu.au}
#' 
PlotPldhivProjection <- function(pldhiv, startyear, endyear, 
  sets = NULL, setnames = NULL, plotcolors = NULL,
  xvalues = NULL) {
  
  # Set up defaults
  if (is.null(sets)) {
    sets <- unique(pldhiv$set)
  } 
  
  if (is.null(setnames)) {
    sets <- unique(pldhiv$set)
  }
  
  if (is.null(xvalues)) {
    xvalues <- seq(startYear, endyear, by = 5)
  }
  
  # Quick error check
  if (length(sets) != length(setnames)) {
    stop("Number of scenario names incompatible with scenarios")
  }
  
  # Plot specs
  if (is.null(plotcolors)) {
    cols <- PlotColors()
  } else {
    cols <- plotcolors
  }
  
  setCols <- cols[1:length(sets)]
  names(setCols) <- sets
  
  # Set up results
  results <- pldhiv %>%
    filter(year >= startyear, year <= endyear, set %in% sets)
  
  # Plot the scenarios
  plotPldhiv <- ggplot(data = results, aes(x = year, y = pldhiv, 
    group = set, colour = set)) +
    geom_line() +
    scale_colour_manual(name = "",
      breaks = sets,
      limits = sets,
      labels = setnames,
      values = setCols[sets]) +
    scale_y_continuous(label = comma, limits = c(0, NA)) + 
    scale_x_continuous(breaks = xvalues) + 
    labs(x = "Year", y = "Number diagnosed") + 
    PlotOptions() + 
    theme(legend.position = "right")
  
  # Return plot handle
  return(plotPldhiv)
  
}

#' Plot diagnosed people living with HIV by age results
#'
#' This function produces ggplot plot handles for the number and proportion 
#' of 
#' diagnosed people living with HIV by agebin.
#'
#' @details The idea of this function is to produce plots for the default 
#' HIV cascade agebins (5 year bins: 0-4, 5-9, ...., 80-84, 85+). This 
#' function requires the libraries ggplot2 and scales and for the 
#' PlotOptions.R and PlotColors.R to be sourced. The plotting is setup to 
#' produce a nice figure file using ggsave with width = 17.5 cm and 
#' height = 13 cm. 
#'
#' @param pldhiv Dataframe in long format showing estimated number of 
#' people living with diagnosed HIV by agebin and year. Column names must 
#' include year agebin and value (for the estimates).
#' @param startyear Year to start x-axis on plot. Optional and set to 1986 
#' as default
#' @param agebins Vector of strings specifiy age bins to be plotted. 
#' Otional and set to NULL which corresponds to all bins. 
#' @param plotcolors Vector of color strings for plots. Number of colors 
#' must equal the number of age bins. Optional and NULL by default. 
#' @param grayscale Logical sepcifying if grayscale should be used in the 
#' plots.
#' Overwrites the plotcolors argument if TRUE. Optional and set to FALSE by 
#' default
#' @param agenames Vector of strings specifying the legend values. Number 
#' of labels must equal the number of age bins. Optional and NULL by 
#' default.
#'
#' @return List with plot handles for number and proportion by age
#' 
#' @author Richard T. Gray, \email{Rgray@kirby.unsw.edu.au}
#' 
PlotAgeCascade <- function(pldhivage, startyear = 1986, agebins = NULL, 
  plotcolors = NULL, grayscale = FALSE, agenames = NULL) {
  
  # Setup data defaults
  if (is.null(agebins)) {
    ages <- unique(pldhivage$agebin)
  } else {
    ages <- agebins
  }
  ageResults <- pldhivage %>% filter(year >= startyear, agebin %in% ages)
  nages <- length(ages)
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
      cbPalette <- PlotColors("cbPalette")
      getPalette <- colorRampPalette(cbPalette[3:9])
      plotcolors <- rev(getPalette(nages))
    }
  } 
  
  # Set defaults agenames
  if (is.null(agenames)) {
    agenames <- str_replace(ages, "a", "")
    agenames <- str_replace(agenames, "_", "-")
  }
  
  # Do some error checking
  if (length(plotcolors) != nages) {
    stop("Number of colours does not match number of age bins")
  }
  
  if (length(agenames) != nages) {
    stop("Number of age labels does not match number of age bins")
  }
  
  # Produce bar plot of numbers
  barPlot <- ggplot(data = ageResults, aes(x = year, y = value, 
      fill = agebin)) + 
    geom_bar(stat = "identity") + 
    xlab("Year") + 
    ylab("Number diagnosed with HIV") +
    scale_fill_manual(values = plotcolors, name = "Age",
      labels = agenames) + 
    scale_y_continuous(label = comma) + 
    scale_x_continuous(breaks = xValues) + 
    PlotOptions() + 
    theme(legend.position = "right", 
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.title.x = element_text(size=12),
      axis.text.x = element_text(size=12),
      axis.title.y = element_text(size=12),
      axis.text.y = element_text(size=12))
  
  # Produce barplot of proportions
  propPlot <- ggplot(data = ageResults, aes(x = year, y = value, 
    fill = agebin)) + 
    geom_bar(stat="identity", position = "fill") + 
    xlab("Year") + 
    ylab("Proportion diagnosed with HIV (%)") +
    scale_fill_manual(values = plotcolors, name = "Age", 
      labels = agenames) +
    scale_y_continuous(label = percent) + 
    scale_x_continuous(breaks = xValues)  + 
    PlotOptions() +
    theme(legend.position = "right", 
      legend.text = element_text(size =12), 
      legend.title = element_text(size = 13),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.x = element_text(size=12),
      axis.text.x = element_text(size=12),
      axis.title.y = element_text(size=12),
      axis.text.y = element_text(size=12))

  # Return plot handles
  return(list(barPlot, propPlot))
  
}

#' Plot distribution of PLDHIV by age
#' 
#' This function produces plot of the distrubtion of people living with 
#' diagnosed HIV by age group for a given year.
#' 
#' @details The idea of this function is to produce plots for the default 
#' HIV cascade agebins (5 year bins: 0-4, 5-9, ...., 80-84, 85+) for a 
#' given year. This function requires the libraries ggplot2 and scales and 
#' for the PlotOptions.R and PlotColors.R to be sourced. Multiple scenarios #' and distributions can be plotted if they are stored separately under 
#' "scenario" in the input data frame. The plotting is setup to produce a 
#' nice figure file using ggsave with width = X cm and height = Y cm.  
#' 
#' @param pldhiv Dataframe in long format showing estimated number of 
#' people living with diagnosed HIV by agebin and year. Column names must 
#' include year agebin and value (for the estimates).
#' 
#' @return 
#' 
#' @author Richard T. Gray, \email{Rgray@kirby.unsw.edu.au}
#' 
PlotAgeDist <- function(distResults, resultsyear, sets = NULL, 
  plotcolors = NULL, agenames = NULL, setnames = NULL) {
  
  # Set up defaults
  if (is.null(sets)) {
    sets <- unique(pldhiv$set)
  } 
  
  if (is.null(setnames)) {
    sets <- unique(pldhiv$set)
  }
  
  # Quick error check
  if (length(sets) != length(setnames)) {
    stop("number of setnames incompatible with sets")
  }
  
  # List of standard age bins for ordering the factor, annoying
  ageList <- c("a0_4", "a5_9","a10_14", "a15_19", "a20_24", "a25_29", 
    "a30_34", "a35_39", "a40_44", "a45_49", "a50_54", "a55_59", 
    "a60_64", "a65_69", "a70_74", "a75_79", "a80_84", "a85+")
  
  # Sort out results
  results <- distResults %>% 
    filter(year == resultsyear, set %in% sets) %>%
    group_by(set) %>%
    mutate(proportion = value / sum(value)) %>%
    mutate(agebin = factor(agebin, levels = ageList)) %>% # reorder...grrr
    ungroup() 
  
  # Plot specs
  if (is.null(plotcolors)) {
    cols <- PlotColors()
  } else {
    cols <- plotcolors
  }
  setCols <- cols[1:length(sets)]
  names(setCols) <- sets
  
  # Set defaults agenames
  if (is.null(agenames)) {
    agenames <- str_replace(ages, "a", "")
    agenames <- str_replace(agenames, "_", "-")
  }
  
  # Generate distribution plot for year and sets
  plotDist <- ggplot(data = results, aes(x = agebin, y = proportion, 
    group = set, colour = set)) +
    geom_line() +
    scale_colour_manual(name = "",
      breaks = sets,
      limits = sets,
      labels = setnames,
      values = setCols[sets]) +
    scale_y_continuous(label = percent) + 
    scale_x_discrete(labels = agenames) + 
    labs(x = "Age", y = "Percentage of total") + 
    PlotOptions() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "right")
  
  # Return plot handles
  return(plotDist)
  
}
