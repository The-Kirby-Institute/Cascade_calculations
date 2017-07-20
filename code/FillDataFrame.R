## R function to fill in missing data with zeros in data frame

# R. T. Gray

FillDataFrame <- function(years, df) {
  # Fill in missing years with zeros
  # Primarily used for cumulative diagnoses
  # Args:
  #   years: A vector of years we want values for.
  #   df: A data farme with missing year values
  # Returns:
  #   A data frame with all years and values filled in for missing years.
  #
  # -----------------------------------------------------------------------
  
  insertRow <- function(existingDF, newrow, r) {
    existingDF[seq(r+1, nrow(existingDF) + 1), ] <- 
      existingDF[seq(r, nrow(existingDF)), ]
    existingDF[r, ] <- newrow
    return(existingDF)
  }
  
  # Initialize output dataframe
  fillMissing <- df
  
  # Loop through the years we want
  for (ii in seq(along = years)) {
    if (!(years[ii] %in% df$year)) {
      # Missing so make the current value equal to zero
      fillMissing <- insertRow(fillMissing, c(years[ii], 
                                              rep(0, ncol(df) - 1)), ii)
    }  
  }
  
  # Return final data frame
  return(fillMissing)
}
