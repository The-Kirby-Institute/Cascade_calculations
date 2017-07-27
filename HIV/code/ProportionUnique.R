## R function to estimate proportion unique

# R. T. Gray and N.A. Bretana

ProportionUnique <- function(hivSetOrig, hivResults, origHivSet){

  
  # Days to ignore in duplicate calculations
  ignore <- c(1,15) 
  # Store number unique diagnoses cumulatively and annually. For annual 
  # unique cases assume all the first years of disgnoses are unique. 

  # Do calculations for all notifications first-----------------------------

  dobAll <- select(hivSetOrig, dob, yeardiagnosis) # Overall
  numberUniqueAll <- numUnique(dobAll, allYears, ignore)
  numberUniqueAll[is.na(numberUniqueAll)] <- 0

  # Add variable for proportion unique - due to statistical calculations 
  # proportion mybe slightly higher than one. In those cases round down to
  # 1.
  propunique <- numberUniqueAll / origHivSet$totalnotifications
  propunique[is.na(propunique)] <- 0
  propunique[propunique > 1] <- 1

  return(propunique)
}
