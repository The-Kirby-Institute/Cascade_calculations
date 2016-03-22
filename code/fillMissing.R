## R function to fill in missing data

# R. T. Gray

FillMissing <- function(years, yearsValues, avalValues) {
  # Fill in missing years data using previous years estimates. 
  # Primarily used for cumulative diagnoses
  # Args:
  #   years: A vector of years we want values for.
  #   yearDiagnoses: A vector of years we have data for. 
  #   cumulativeDiagnoses : A vector with the available values.
  # Returns:
  #   FillMissing: A data frame with all years and 
  #     values filled in for missing years.
  #
  # -----------------------------------------------------------------------
  
  # Initialize output dataframe
  fillmissing <- data.frame(year = years, value = rep(NA,length(years)))
  
  # Loop through the years we want
  for (ii in seq(along = years)) {
    
    if (years[ii] %in% yearsValues) {
      # Have the data so add to output
      index <- match(years[ii],yearsValues)
      fillmissing$value[ii] <- avalValues[index]
    } else {
      # Missing so assume so make the current value equal last years value
      if (ii > 1) {
        fillmissing$value[ii] <- fillmissing$value[ii-1] 
      } else {
        # If the first is missing set initial value to zero
        fillmissing$value[ii] <- 0
      }
    }  
  }
  
  # Return final data frame
  return(fillmissing)
}
