#' Plot an STI cascade bar chart
#' 
#' This function produces a ggplot of a STI cascade bar chart for reporting 
#' and publication purposes. 
#' 
#' @details The idea of this function is to be able to produce a quick plot
#' from an inputed STI cascade data frame with the ability to add various
#' options. It has been designed to replicate previously published and 
#' reported cascade figures. This function requires the PlotOptions and 
#' LoadLibrary functions to be sourced (or loaded via LeftysRpkg). 
#' 
#' @param cascade Dataframe containing HIV cascade estimates. The dataframe
#'  must have column names year, stage, value, lower and upper. 
#' @param year Integer specifying the specific year of cascade estimates to 
#' be plotted. Optional and NULL by default. If NULL the latest year 
#' estimates will be plotted.  
#' @param ymax Numeric specifying the maximum value for the y-axis limits. 
#' Optional and NULL by default. If NUll the default range produced by 
#' ggplot will be used. If a value is entered then the y-axis breaks will 
#' be given by seq(0, ymax, by = ymax/4). 
#' @param plotcolours Vector of color strings for plots. Number of colors 
#' must equal the number of steps in the cascade. Optional and NULL by 
#' default. If NUll the cascade will be plotted in grey. 
#' @param steplabels A vector of strings specifying the labels describing 
#' each step of the cascade. The number of strings must equal the number of
#' steps. Optional and NULL by default. If NUll the labels will just be the 
#' stage names in the cascade dataframe. 
#' @param ranges Logical specifiying if error bars for the estimated range 
#' are plotted. Optional and TRUE by default. 
#' @param percentages Logical specifying if the percentage for the value of
#' each step relative to the first step is displayed on the plot. Optinal 
#' and FALSE by default. If TRUE the percentage will be plotted at a height
#' specified by pheight.
#' @param pheight A numeric specifying where the percentage values will be
#' displayed. Optional and NULL by default. If NUll then percentages will
#' be displayed at a height of y = 0.05 * max(cascade$value). 
#' 
#' @return A ggplot of the resulting STI cascade
#' 
#' @author Richard T. Gray, \email{Rgray@kirby.unsw.edu.au}
#' 
#' @import tidyverse, scales, (LeftysRpkg)
#' 
PlotStiCascade <- function(cascade, year = NULL, ymax = NULL, plotcolours = NULL, 
    steplabels = NULL, ranges = TRUE, percentages = FALSE, pheight = NULL) {
    
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

    if (is.null(plotcolours)) {
        plotcolours <- "grey75" # default is gray scale
    } 
    
    if (is.null(steplabels)) {
        steplabels <- steps
    }
    
    steps <- c("Infected", "Diagnosed","Treated", "Retested")
    nsteps <- 4
    barWidth <- 0.8
    
    # Set up results we want to plot
    estimates <- cascade |>
        filter(year == cascadeYear)
    
    # Basic plot
    plotBar <- ggplot(data = estimates, aes(x = stage, y = value)) + 
        scale_x_discrete(limits = steps, labels = steplabels) + 
        ylab("Number of people") + xlab("") +  
        PlotOptions() + 
        theme(
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(size = 11),
            axis.text.y = element_text(size = 11))
    
    # Sort out y-axis
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
            geom_text(x = c(1:nsteps), y = rep(yPercent, nsteps),
                label = percent(c(
                    estimates$value[1] / estimates$value[1],
                    estimates$value[2] / estimates$value[1],
                    estimates$value[3] / estimates$value[2],
                    estimates$value[4] / estimates$value[3])),
                size = 4, colour = "black")
    }
    
    # Return final bar chart
    return(plotBar)
}