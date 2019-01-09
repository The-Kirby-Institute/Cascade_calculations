#' Fill in missing ages
#' 
#' This function is used to fill in missing ages with zeros for HIV 
#' cascades split by age
#'
#' @param ages Character vector specifying ages we want values for. Almost 
#' always. c("a0_4", "a5_9","a10_14", "a15_19", "a20_24", "a25_29", 
#' "a30_34", "a35_39", "a40_44", "a45_49", "a50_54", "a55_59",
#'  "a60_64", "a65_69", "a70_74", "a75_79", "a80_84", "a85+")
#' @param agedDf Data frame with aged data. Column names must be a subset 
#' of ages. 
#' @param missingValue Numeric value to fill in for missing age values
#'  
#' @return A data frame with all ages and values filled in for missing ages
#' 
#' @author Richard T Gray, \email{Rgray@kirby.unsw.edu.au}
#' 
#' @export
#' @import tidyverse

FillMissingAge <- function(ages, agedDf, missingValue = 0) {

  # Initialize output data frame
  fillMissing <- agedDf
  
  # Loop through the ages we want
  colNames <- colnames(agedDf)
  
  for (ii in seq(along = ages)) {
    if (!(ages[ii] %in% colNames)) {
      # Missing so add column and rearrange to correct position
      fillMissing[ages[ii]] <- rep(missingValue, nrow(agedDf))
    }
  }
  
  # Return final data frame
  return(fillMissing)
  
}
