## R function to fill in missing data with zeros in data frame

# R. T. Gray

FillDataFrame <- function(years, df, cumulative = FALSE) {
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
    # To insert newrow it appears you need existingDF to be a data frame
    # not a tibble
    newDF <- as.data.frame(existingDF)
    if (r == (nrow(existingDF)+1)) {
      newDF <- rbind(existingDF, newrow)
    } else {
      newDF[seq(r+1, nrow(existingDF) + 1), ] <- 
        existingDF[seq(r, nrow(existingDF)), ]
      newDF[r, ] <- newrow 
    }
    
    if (is_tibble(existingDF)) {
      newDF <- as_tibble(newDF)
    }
    
    return(newDF)
  }
  
  # Initialize output dataframe
  fillMissing <- df
  
  # Loop through the years we want
  for (ii in seq(along = years)) {
    if (!(years[ii] %in% df$year)) {
      if (cumulative) {
        # Missing so make the current value equal to previous value 
        tempRow <- fillMissing[ii-1, 2:ncol(fillMissing)]
        if (nrow(tempRow) == 0) {
          tempRow <- rep(0, ncol(df) - 1)
        }
        fillMissing <- insertRow(fillMissing, c(years[ii], 
          tempRow), ii)
      } else {
        # Missing so make the current value equal to zero
        fillMissing <- insertRow(fillMissing, c(years[ii], 
          rep(0, ncol(fillMissing) - 1)), ii)
      }  
    }
  }
  
  # Return final data frame
  return(fillMissing)
}
