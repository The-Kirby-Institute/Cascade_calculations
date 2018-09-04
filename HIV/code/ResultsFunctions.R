# Functions for generating results and plots

# Richard T. Gray

# This script contains functions useful for generating summary results 
# and plots for the HIV cascade calculations. These functions are primarily 
# used in the 4-HivCascadePlots.Rmd script but can be used separately. 

# Load required libraries for functions to work
library(tidyverse)
library(scales)

# Calculations ------------------------------------------------------------

#' Calculate average age of PLDHIV 
AverageAge <- function(pldhivage, resultsyear, agebins = NULL, 
  agemidpoints = NULL) {
  
  # Setup defaults
  if (is.null(agebins)) {
    ages <- unique(pldhivage$agebin)
  } else {
    ages <- agebins
  }
  
  # Sort out results
  results <- pldhivage %>% 
    filter(year == resultsyear, agebin %in% ages)
    
  # Set up agemidpoints
  if (is.null(agemidpoints)) {
    # Read in age bins and calculate midpoint automatically from lower
    # and upper  values (assume agebin format is "aX_Y" or "aX+". 
    ageStrings <- str_split(ages, "_")
    lowerStr <- sapply(ageStrings, function(x) str_replace(x[1], "a", ""))
    upperStr <- sapply(ageStrings, function(x) x[2])
    lower <- unname(sapply(lowerStr, function(x) as.numeric(str_replace(x, 
      "\\+", ""))))
    upper <- as.numeric(upperStr)
    
    if (is.na(tail(upper,1))) {
      upper[length(upper)] <- tail(lower, 1)
    }
    
    midpoints <- (lower + upper) / 2
  } else {
    midpoints <- agemidpoints
  } 
  
  # Calculate average age
  nages <- length(ages)
  average <- sum(midpoints * results$value) / sum(results$value)
  
  # Return average age
  return(average)
}

#' Merge number of diagnosed people living with HIV in age groups
#'
#' This function produces a data frame combining the number of PLDHIV in 
#' by age into merged age groups. 
#'
#' @details The idea of this function is to calculate the number of people 
#' across multiple HIV cascade agebins (5 year bins: 0-4, 5-9, ...., 
#' 80-84, 85+). This function requires the tidyverse library  to be 
#' sourced. 
#'
#' @param pldhiv Dataframe in long format showing estimated number of 
#' people living with diagnosed HIV by agebin and year. Column names must 
#' include year, agebin, value (for the estimates), lower and upper.
#' @param agebins List containing vectors of strings specifiy the age bins 
#' to be merged. Must be combnations of the standard list: c("a0_4", 
#' "a5_9","a10_14", "a15_19", "a20_24", "a25_29", "a30_34", "a35_39", 
#' "a40_44", "a45_49", "a50_54", "a55_59", "a60_64", "a65_69", "a70_74", 
#' "a75_79", "a80_84", "a85+")
#' @param agenames Vector of strings specifying the names of the new
#' merged age bins. Number of labels must equal the number of vectors of 
#' age bins in the parameter agebins.
#'
#' @return Data frame in long format with the column names year, agebin and
#' value (for the estimates).
#' 
#' @author Richard T. Gray, \email{Rgray@kirby.unsw.edu.au}
#' 
MergeAgeCascade <- function(pldhivage, agebins, agenames) {
  
  # Do some error checking
  if (length(agebins) != length(agenames)) {
    stop("NUmber of age names is different to number of age groups")
  }
    
  # Sort out results
  results <- pldhivage %>% 
    select(year, agebin, value, lower, upper)
  
  # Initialize return data frame
  mergedAges <- data_frame(year = integer(), 
    agebin = character(),
    value = numeric(),
    lower = numeric(),
    upper = numeric())
  
  # Loop through agebin list extract estimates and merge
  for (ii in 1:length(agebins)) {
    # Extract numbers and sum
    tempResults <- results %>%
      filter(agebin %in% agebins[[ii]]) %>%
      group_by(year) %>%
      summarise(value = sum(value),
        lower = sum(lower),
        upper = sum(upper))
    
    # Add name of age bin
    tempResults$agebin <- agenames[[ii]]
    
    # Re-order
    tempResults <- tempResults %>%
      select(year, agebin, value, lower, upper)
    
    # Bind into mergedAges
    mergedAges <- bind_rows(mergedAges, tempResults)
  }
  
  # Return final data frame
  return(mergedAges)
  
}

# Plotting functions ------------------------------------------------------



PlotCascade <- function(cascade, year = NULL, ymax = NULL, 
  retained = FALSE, plotcolours = NULL, steplabels = NULL, ranges = TRUE, 
  percentages = FALSE, pheight = NULL, targetlines = c("none", "90", "95"), 
  targetlabels = FALSE) {
  
  # Argument checking and setup defaults if not specified
  if (is.null(year)) {
    cascadeYear <- max(cascade$year) # default to latest year 
  } else {
    cascadeYear <- year
  } 

  if (is.null(ymax)) {
    ymax <- NA
  } else {
    yBreaks <- seq(0, ymax, by = ymax/4)
  }
  
  if (retained == FALSE) {
    steps <- c("infected", "pldhiv","numART", "suppressed")
    nsteps <- 4
    artIndex <- 3
  } else {
    steps <- c("infected", "pldhiv", "retained", "numART", "suppressed")
    nsteps <- 5
    artIndex <- 4
  }
  
  if (is.null(plotcolours)) {
    plotcolours <- "grey75" # default is gray scale
  } 
  
  if (is.null(steplabels)) {
     steplabels <- steps
  }
  
  if (targetlines[1] != "none") {
    targetlines <- match.arg(targetlines)
    targetValue <- as.numeric(targetlines) / 100
  } 
  
  if (targetlabels) {
    if (targetlines[1] == "none") {
      warning("targetlines not set to plot so targetlabels not displayed")
      targetlabels <- FALSE
    } 
      barWidth <- 0.6
  } else {
    barWidth <- 0.8
  }
  
  # Set up results we want to plot
  estimates <- cascade %>%
    filter(year == cascadeYear, stage %in% steps)
  
  # Basic plot
  plotBar <- ggplot(data = estimates, aes(x = stage, y = value)) + 
    scale_x_discrete(limits = steps, labels = steplabels) + 

    ylab("Number of people") + xlab("") +  
    PlotOptions() + theme_classic() + 
    theme(
      axis.title.y = element_text(size=12, face = "bold"),
      axis.text.x = element_text(size=9),
      axis.text.y = element_text(size=11))
  
  # Sort out y-axis labels
  if (is.na(ymax)) {
    plotBar <- plotBar +
      scale_y_continuous(expand = c(0,0), limits = c(0, ymax), 
        labels = comma)
  } else {
    plotBar <- plotBar +
      scale_y_continuous(expand = c(0,0), limits = c(0, ymax), 
        labels = comma, breaks = yBreaks)
  }
  
  # Add bar chart
  if (plotcolours[1] == "grey75"){
    plotBar <- plotBar +
      geom_bar(stat = "identity", color = "black", 
        fill = plotcolours, width = barWidth)
  } else { 
    plotBar <- plotBar +
      geom_bar(stat = "identity", color = plotcolours, 
        fill = plotcolours, width = barWidth)
  } 
  
  # Add ranges
  if (ranges) {
    plotBar <- plotBar + 
      geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1,
        color = "black", size = 1.1) 
  }
  
  # Add percents
  if (percentages) {
    
    if (is.null(pheight)) {
      yPercent <- 0.05 * max(estimates$value)
    } else {
      yPercent <- pheight
    }
    
    plotBar <-  plotBar + 
      geom_text(x = c(1:nsteps), y = rep(yPercent,nsteps),
        label = percent(estimates$value /
            estimates$value[1]),
        size = 3, colour = "black")
    }
  
  # Add target lines
  if(targetlines[1] != "none") {
    plotBar <-  plotBar +
      geom_hline(yintercept = targetValue * estimates$value[1],
        linetype = 2) + 
      geom_hline(yintercept = targetValue * estimates$value[2],
        linetype = 2) +
      geom_hline(yintercept = targetValue * estimates$value[artIndex],
        linetype = 2)
  } 
  
  # Add labels for target lines
  if(targetlabels) {
    # Need some extra libraries to do this
    LoadLibrary(grid)
    LoadLibrary(ggplotify)
    
    # Create annotated plot 
    plotBar <-  plotBar +
      theme(plot.margin = unit(c(0.5, 5, 0, 0), "cm")) + 
      annotation_custom(
        grob = textGrob(
          label = paste0(100 * targetValue, "% PLHIV diagnosed"), 
          hjust = -0.1, 
          gp = gpar(cex = 0.85, fontface = "bold")),
        ymin = targetValue * estimates$value[1],
        ymax = targetValue * estimates$value[1],
        xmin = nsteps + 0.5,
        xmax = nsteps + 0.5) +
      annotation_custom(
        grob = textGrob(
          label = paste0(100 * targetValue, "% diagnosed on ART"), 
          hjust = -0.1, 
          gp = gpar(cex = 0.85, fontface = "bold")),
        ymin = targetValue * estimates$value[2],
        ymax = targetValue * estimates$value[2],
        xmin = nsteps + 0.5,
        xmax = nsteps + 0.5) +
      annotation_custom(
        grob = textGrob(
          label = paste0(100 * targetValue, "% on ART suppressed"), 
          hjust = -0.1, 
          gp = gpar(cex = 0.85, fontface = "bold")),
        ymin = targetValue * estimates$value[artIndex],
        ymax = targetValue * estimates$value[artIndex],
        xmin = nsteps + 0.5,
        xmax = nsteps + 0.5)
    plotBar <- ggplot_gtable(ggplot_build(plotBar))
    plotBar$layout$clip[plotBar$layout$name == "panel"] <- "off"
    
    # Convert back to a ggplot
    plotBar <- as.ggplot(plotBar)
  }
  
  # Return final bar chart
  return(plotBar)
}

#' Plot number of PLDHIV over time for multiple projections
#' 
#' This function produces a plot of the number of people living with 
#' diagnosed HIV over time for an inputed set of scenarios.
#' 
#' @details The idea of this function is to produce plots for the  
#' 
#' @param pldhiv Dataframe in long format showing estimated number of 
#' people living with diagnosed HIV each year for each set. Column names
#' must include year, set and pldhiv (for the estimates).
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
