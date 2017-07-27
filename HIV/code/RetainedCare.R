## Function to calculate retained in care

# Richard T. Gray

RetainedCare <- function(hivDiagnosed, retained, lower, upper, years) {
  # Calculate the number of people living with diagnosed HIV.
  #
  # Args:
  #   pldhiv: 
  #   retained:
  #   lower:
  #   upper:
  #   years
  # Returns:
  #   A data frame with the estimated number retained in care
  
  # Setup ----------------------------------------------------------------
  
  # Check lengths of inputs
  if (length(retained) != length(lower) || length(lower) != length(upper)) {
    stop("Lengths of retained data inconsistent")  
  }
  
  nyears <- length(years)
  
  if (length(retained) !=1 && length(retained) != nyears) {
    stop("Length of retained data not equal to number of years") 
  }
  
  # Setup data vectors for calculations ----------------------------------
  if (length(retained) == 1) {
    # Single estimate so convert to vector
    retainedData <- rep(retained, nyears)
    lowerData <- rep(lower, nyears)
    upperData <- rep(upper, nyears)
  } else {
    retainedData <- retained
    lowerData <- lower
    upperData <- upper
  }
  
  # Generate estimates --------------------------------------------------- 
  
  diagnosedYears <- hivDiagnosed %>%
    filter(year %in% years) 
  
  hivRetained <- diagnosedYears %>%
    select(stage, year, population)
    
  hivRetained$value <- diagnosedYears$value * retainedData
  hivRetained$lower <- diagnosedYears$lower * lowerData
  hivRetained$upper <- diagnosedYears$upper * upperData

  hivRetained$stage <- "retained"
  
  # Return final data 
  return(hivRetained)
}
