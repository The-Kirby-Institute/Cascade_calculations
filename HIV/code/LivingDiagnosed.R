## R function to calculate number of PLDHIV

# R. T. Gray

LivingDiagnosed <- function(annualdiags, propunique, deathrate, migration,  
  propstay = NULL, arrivals = NULL, departs = NULL, pldhiv = NULL, 
  adjustment = NULL) {
  # Calculate the number of people living with diagnosed HIV.
  #
  # Args:
  #   annualdiags: A numeric vector of annual diagnoses. 
  #   propunique: 
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
  #   adjustment (optional): 
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
  nyears <- length(annualdiags)
  
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
  
  if (is.null(adjustment)) {
    adjustment <- rep(1, nyears)
  }
  
  # Error handling -------------------------------------------------------
  # Make sure all the vectors are the right length and the vectors of rates
  # are between zero and one. 
  
  vectorLengths <- c(nyears, length(annualdiags), length(deathrate),
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
  
  nduplicates <- rep(NA,nyears)
  nduplicates[1] <- 0
    
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
  
  nextra <- rep(NA,nyears)
  nextra[1] <- 0
  
  # Loop through input parameters and calculate numLiving
  for (ii in 2:nyears) {
    
    # The following formula needs to have the same form as the formula in
    # 0-GenerateAdjustments.Rmd to ensure the deathrates/deaths match. 
    
    # NOTE: In mid 2022 I changed from the old formulas below to the new formulas. 
    # I had previously been using the old formula (with the "new" formula 
    # commented out!). I don't think it makes a massive difference to the 
    # estimates but I think the new formula better captures what we want 
    # conceptually. 
    
    # Old formula: Based on an Euler approximation approach but given the
    # annual time difference doesn't really capture changes on an annual basis
    # appropriately especially with the effects of the COVID-19 pandemic on 
    # emigration since 2020. This formula was used up to mid 2022 for 
    # cascade estimates.  
    
    # nliving[ii] <- nliving[ii-1] + propunique[ii] * annualdiags[ii] -
    #   (1-propstay[ii-1]) * propunique[ii-1] * annualdiags[ii-1] - 
    #   (deathrate[ii-1] + migration[ii-1] + departs[ii-1]) * nliving[ii-1] +
    #   arrivals[ii-1] * (pldhiv[ii-1] - nliving[ii-1])
    
    # Old formula annual estimates
    # nduplicates[ii] <- (1 - propunique[ii]) * annualdiags[ii]
    # 
    # ndead[ii] <- deathrate[ii] * nliving[ii]
    # 
    # nmigrants[ii] <- migration[ii] * nliving[ii]
    # 
    # ndeparts[ii] <- departs[ii] * nliving[ii]
    # 
    # narrivals[ii] <- arrivals[ii] * (pldhiv[ii] - nliving[ii])
    # 
    # nleave[ii] <- (1 - propstay[ii]) * propunique[ii] *
    #   annualdiags[ii]
    
    # New formula: Used since mid-2022. Calculations provide the number at 
    # the end of the year using the number at the end of the previous year and
    # changes through the year (assumes new diagnoses stay in the population 
    # except for those who leave immediately) 
    nliving[ii] <- nliving[ii-1] + 
      propstay[ii] * propunique[ii] * annualdiags[ii] - 
      (deathrate[ii] + adjustment[ii] * migration[ii] + departs[ii]) * nliving[ii-1] +
      arrivals[ii] * (pldhiv[ii-1] - nliving[ii-1])
    
    # With small numbers can sometimes get a negative so check and replace with 0
    nliving[ii] <- ifelse(nliving[ii] < 0, 0, nliving[ii])

    # Annual estimates for new formula
    nduplicates[ii] <- (1 - propunique[ii]) * annualdiags[ii]

    ndead[ii] <- deathrate[ii] * nliving[ii-1]

    nmigrants[ii] <- adjustment[ii] * migration[ii] * nliving[ii-1]

    ndeparts[ii] <- departs[ii] * nliving[ii-1]

    narrivals[ii] <- arrivals[ii] * (pldhiv[ii-1] - nliving[ii-1])

    nleave[ii] <- (1 - propstay[ii]) * propunique[ii] *
      annualdiags[ii]
    
    # Additional population movements 
    nextra[ii] <- (adjustment[ii] - 1) * migration[ii] * nliving[ii-1]
    
  }
 
  # Put all the outputs into a data frame and return
  return(data.frame(
    pldhiv  = nliving,
    diagnoses = annualdiags,
    duplicates = nduplicates,
    deaths = ndead, 
    emigrants = nmigrants,
    diag_departs = nleave,
    inter_departs = ndeparts,
    inter_arrivals = narrivals,
    extra_migrants = nextra)
  )
}
