livingDiagnosed <- function(cumdiagnoses, deathrate, migration, 
                            arrivals = 0, departs = 0, 
                            otherdiags = 0) {
  # Calculate the number of people living with diagnosed HIV.
  #
  # Args:
  #   cumdiagnoses: A numeric vector of cumulative diagnoses. 
  #   deathrate: A numeric vector of deathrates.
  #   migration: A numeric vector of rates of migration overseas.
  #   arrivals (optional): If specified, a numeric vector of the rate 
  #     people arrive in the population from interstate.
  #   departs (optional):  If specified, is a numeric vector of the rate 
  #     people depart interstate.
  #   otherdiags (optional): If specified, is a numeric vector giving the 
  #     number of people living with diagnosed HIV interstate
  # Returns:
  #   A numeric vector with the cumulative number of people living with 
  #   diagnosed HIV.
  #
  # Notes: Each of the input arguments are vectors which must be of equal 
  #   length. The length corresponds to the number of years considered 
  #   in the calculation.
  # 
  #   All optional vectors must be used or none
  #
  # 
  
  # Setup ----------------------------------------------------------------
  
  # Calculate number of years from length of cumdiagnoses
  nyears <- length(cumdiagnoses)
  
  # If optional inputs equal to zero convert to vectors
  if (arrivals == 0) {
    arrivals <- rep(0, nyears)
  }
  
  if (departs == 0) {
    departs <- rep(0, nyears)
  }
  
  if (otherdiags == 0) {
    otherdiags <- rep(0, nyears)
  }
  
  # Error handling -------------------------------------------------------
  # Make sure all the vectors are the right length and the vectors of rates
  # are between zero and one. 
  
  vectorLengths <- c(nyears, length(cumdiagnoses), length(deathrate),
                     length(migration), length(arrivals), length(departs), 
                     length(otherdiags))
  
  if (length(unique(vectorLengths)) != 1) {
    error(" Input vectors have different lengths")
  }
  
  # Main program ----------------------------------------------------------

  # Calculate annual diagnoses
  annualdiags <- c(cumdiagnoses[1],diff(cumdiagnoses))
  
  # Initialize output array
  nliving <- rep(NA,nyears)
  nliving[1] <- annualdiags[1]
  
  # Loop through input parameters and calculate numLiving
  for (ii in 2:nyears) {
    nliving[ii] <- nliving[ii-1] + annualdiags[ii] - (deathrate[ii-1] +  
      migration[ii-1] + departs[ii-1]) * nliving[ii-1] + arrivals[ii-1] * otherdiags[ii-1]
    
  }
  
  # Return final output
  return(nliving)
  
}
