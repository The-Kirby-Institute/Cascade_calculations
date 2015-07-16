fillDiagnoses <- function(years, yearsDiagnoses, cumulativeDiagnoses) {
  # Fill in missing years for cumulative diagnoses
  # Args:
  #   years: A vector of years we want cumulative diagnoses data for
  #   yearDiagnoses: A vector of years we have diagnoses data for. We assume 
  #     missing years have zero annual notifications 
  #   cumulativeDiagnoses : A vector with the available cumulative diagnoses
  # Returns:
  #   filledDiagnoses: A data frame with all years and 
  #       cumulative diagnoses filled in for missing years
  
  # ------------------------------------------------------ 
  
  # Initialize output dataframe
  filldiagnoses <- data.frame(year = years, total = rep(NA,length(years)))
  
  # Loop through the years we want
  for (ii in seq(along = years)) {
    
    if (years[ii] %in% yearsDiagnoses) {
      # Have the data so add to output
      index <- match(years[ii],yearsDiagnoses)
      filldiagnoses$total[ii] <- cumulativeDiagnoses[index]
    } else {
      # Missing so assume zero and make the current cumulative diagnoses 
      # equal last years value
      if (ii > 1) {
        filldiagnoses$total[ii] <- filldiagnoses$total[ii-1] 
      } else {
        # If the first is missing set cumulative diagnoses to zero
        filldiagnoses$total[ii] <- 0
      }
    }  
  }
  
  # Return final data frame
  return(filldiagnoses)
}
