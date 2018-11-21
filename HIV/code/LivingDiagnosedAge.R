## R function to calculate number of PLDHIV by age

# R. T. Gray

LivingDiagnosedAge <- function(annualdiags, propunique, deathrate, 
  migration, agedeath, agemigrate, propstay = NULL, arrivals = NULL, 
  departs = NULL, pldhiv = NULL, normalize = NULL) {
  # Calculate the number of people living with diagnosed HIV by age.
  #
  # Args:
  #   annualdiagnoses: A matrix of annual diagnoses by age and year. 
  #   deathrate: A numeric vector of overall deathrate by year.
  #   migration: A numeric vector of overall rate of emigration by year.
  #   propstay: A numeric vector with overall percentage who stay post 
  #     diagnosis by year. Equal to 1 minus proportion who leave post 
  #     diagnosis. Assumed to be same for each age group.
  #   agedeath: A matrix of relative annual deathrate by age and year. 
  #     Multiply by deathrate to get actual death rates for each age.
  #   agemigrate: A matrix of relative annual emigration rates by age and 
  #     year. Multiply by migration to get actual death rates for each age.
  #   arrivals (optional): If specified, a numeric vector of the overall 
  #     rate people arrive in the population from interstate by year.
  #   departs (optional):  If specified, is a numeric vector of the overall
  #     rate people depart interstate by year.
  #   pldhiv (optional): If specified, is a matrix giving the 
  #     overall number of people living with diagnosed HIV by age and year. 
  #   normalize (optional): If specified, is a numeric vector giving the 
  #     overall number of people living with diagnosed HIV across age 
  #     groups by year. This is so we can correct the number in each age 
  #     bin each year so the sum matches the normalized value.  
  # Returns:
  #   A data frame with the cumulative number of people living with 
  #   diagnosed HIV by age and year plus....
  #
  # Notes: Each of the input arguments are vectors which must be of equal 
  #   length. The length corresponds to the number of years considered 
  #   in the calculation.
  # 
  #   All optional vectors must be used or none
  #
  # Setup ----------------------------------------------------------------
  
  # Calculate number of years from length of cumdiagnoses
  nyears <- ncol(annualdiags)
  nages <- nrow(annualdiags)
  
  ageList <- rownames(annualdiags)
  yearList <- colnames(annualdiags)
  
  # If optional inputs equal to zero convert to vectors
  if (!is.null(propstay)) {
    if (length(propstay == 1)) {
      propstay <- rep(propstay, nyears)
    } 
  } else {
    propstay <- rep(1, nyears)
  }
  
  if (is.null(arrivals)) {
    arrivals <- matrix(0, nages, nyears)
  }
  
  if (is.null(departs)) {
    departs <- matrix(0, nages, nyears)
  }
  
  if (is.null(pldhiv)) {
    pldhiv <- matrix(0, nages, nyears)
  }
  
  if (is.null(normalize)) {
    doNormalize <- FALSE
    normalize <- rep(0, nyears)
  } else {
    doNormalize <- TRUE
  }
  
  # Error handling -------------------------------------------------------
  # Make sure all the vectors are the right length and equal to the number of 
  # columns for the matrices. 
  # Make sure all the vectors of rates are between zero and one. 
  # Make sure all matrices have the same number of rows and equal to the 
  # number of agebins (should be 18)
  
  vectorLengths <- c(nyears, ncol(annualdiags), length(deathrate),
    length(migration), ncol(arrivals), ncol(departs), ncol(pldhiv), 
    ncol(agedeath), ncol(agemigrate), length(normalize))
  
  if (length(unique(vectorLengths)) != 1) {
    stop("Input vectors have different lengths")
  }
  
  matrixRows <- c(nrow(annualdiags), nrow(agedeath), nrow(agemigrate), 
    nrow(pldhiv), 18)
  
  if (length(unique(matrixRows)) != 1) {
    stop("Input matrices have incorrect number of rows")
  }
  
  # Main program ----------------------------------------------------------
  
  # Initialize output array
  nliving <- matrix(0, nages, nyears)
  nliving[, 1] <- annualdiags[, 1]
  
  nduplicates <- matrix(0, nages, nyears)
  nduplicates[, 1] <- 0
    
  ndead <- matrix(0, nages, nyears)
  ndead[, 1] <- 0
  
  nmigrants <- matrix(0, nages, nyears)
  nmigrants[, 1] <- 0
  
  ndeparts <- matrix(0, nages,nyears)
  ndeparts[, 1] <- 0
  
  narrivals <- matrix(0, nages, nyears)
  narrivals[, 1] <- 0
  
  nleave <- matrix(0, nages, nyears)
  nleave[, 1] <- 0
  
  # Create an aging rate vector - 5-year age bins. No one 
  # ages in last age bin
  ageRate <- c(rep(0.2, nages - 1), 0)
  
  # Loop through input parameters and calculate numLiving
  for (ii in 2:nyears) {
    
    for (jj in 1:nages) {
      
      # Calculate number of people moving up in age
      if (jj == 1) {
        # If in first age group no-one ages from younger group
        ageUp <- 0
      } else {
        ageUp <- nliving[jj - 1, ii - 1] * ageRate[jj-1]
      }
      
      # Update current age group for current year - assuming same 
      # overseas migration rate for each age group equal to overall 
      # migration rate
      nliving[jj, ii] <- nliving[jj, ii - 1] + 
        propunique[ii] * annualdiags[jj, ii] - 
        (1 - propstay[ii-1]) * propunique[ii-1] * annualdiags[jj, ii-1] - 
        deathrate[ii-1] * agedeath[jj, ii-1] * nliving[jj, ii-1] -
        migration[ii-1] * agemigrate[jj, ii-1] * nliving[jj, ii-1] -
        departs[jj, ii-1] * nliving[jj, ii-1] +
        arrivals[jj, ii-1] * (pldhiv[jj, ii-1] - nliving[jj, ii-1]) -
        nliving[jj, ii - 1] * ageRate[jj] + ageUp
      
      # For low numbers the previous calculation could give number less
      # than zero. In this case repalce with zero
      if (nliving[jj, ii] < 0) {
        nliving[jj, ii] <- 0
      }
    }
    
    # If specified normalize age estimates to overall estimate
    if (doNormalize) {
      if (sum(nliving[, ii]) != 0) {
        nliving[, ii] <- nliving[, ii] * normalize[ii] / sum(nliving[, ii])
      } 
    }
    
  } 
  
  rownames(nliving) <-ageList
  colnames(nliving) <- yearList
  
  return(nliving)
}
