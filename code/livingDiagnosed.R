## R function to calculate number of PLDHIV

# R. T. Gray

LivingDiagnosed <- function(annualdiags, propunique, deathrate, migration,  
                            propstay = NULL, arrivals = NULL, 
                            departs = NULL, pldhiv = NULL) {
  # Calculate the number of people living with diagnosed HIV.
  #
  # Args:
  #   cumdiagnoses: A numeric vector of cumulative diagnoses. 
  #   deathrate: A numeric vector of deathrates.
  #   migration: A numeric vector of rates of migration overseas.
  #   propstay: A numeric vector with percentage who stay post diagnosis.
  #     Equal to 1 minus proportion who leave post diagnosis.
  #   arrivals (optional): If specified, a numeric vector of the rate 
  #     people arrive in the population from interstate.
  #   departs (optional):  If specified, is a numeric vector of the rate 
  #     people depart interstate.
  #   pldhiv (optional): If specified, is a numeric vector giving the 
  #     overall number of people living with diagnosed HIV
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
  if (!is.null(propstay)) {
    if (length(propstay == 1)) {
      propstay <- rep(propstay, nyears)
    } 
  } else {
    propstay <- rep(1, nyears)
  }
  
  if (is.null(arrivals)) {
    arrivals <- rep(0, nyears)
  }
  
  if (is.null(departs)) {
    departs <- rep(0, nyears)
  }
  
  if (is.null(pldhiv)) {
    pldhiv <- rep(0, nyears)
  }
  
  # Error handling -------------------------------------------------------
  # Make sure all the vectors are the right length and the vectors of rates
  # are between zero and one. 
  
  vectorLengths <- c(nyears, length(cumdiagnoses), length(deathrate),
                     length(migration), length(arrivals), length(departs), 
                     length(pldhiv))
  
  if (length(unique(vectorLengths)) != 1) {
    stop(" Input vectors have different lengths")
  }
  
  # Main program ----------------------------------------------------------

  # Calculate annual diagnoses
  # annualdiags <- propstay * c(cumdiagnoses[1], diff(cumdiagnoses))
  
  # Initialize output array
  nliving <- rep(NA,nyears)
  nliving[1] <- annualdiags[1]
  
  ndead <- rep(NA,nyears)
  ndead[1] <- 0
  
  nmigrants <- rep(NA,nyears)
  nmigrants[1] <- 0
  
  ndeparts <- rep(NA,nyears)
  ndeparts[1] <- 0
  
  narrivals <- rep(NA,nyears)
  narrivals[1] <- 0
  
  nleave <- rep(NA,nyears)
  nleave[1] <- 0
  
  # Loop through input parameters and calculate numLiving
  for (ii in 2:nyears) {
    nliving[ii] <- nliving[ii-1] + propstay[ii] * propunique[ii] * 
      annualdiags[ii] - (deathrate[ii-1] + migration[ii-1] + 
                           departs[ii-1]) * nliving[ii-1] + 
      arrivals[ii-1] * (pldhiv[ii-1] - nliving[ii-1])
    
    nduplicates <- (1 - propunique[ii]) * nliving[ii-1]
    
    ndead[ii] <- deathrate[ii-1] * nliving[ii-1]
      
    nmigrants[ii] <- migration[ii-1] * nliving[ii-1]
      
    ndeparts[ii] <- departs[ii-1] * nliving[ii-1]
    
    narrivals[ii] <- arrivals[ii-1] * (pldhiv[ii-1] - nliving[ii-1])
    
    nleave[ii] <- (1 - propstay[ii]) * nliving[ii-1]
    
  }
 
  # Put all the outputs into a dataframe
  
  # Return final output
  #return(nliving)
  return(data.frame(nliving, nduplicates, ndead, nmigrants, ndeparts, narrivals, nleave))
}
