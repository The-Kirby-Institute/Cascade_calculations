## R functions to calculate appropriate migration rate adjustments 

# Richard T. Gray

# This script contains functions for calculating the relative migration
# rate for a specified sub-population. 

# Define local functions --------------------------------------------------
extractData <- function(data, fcob, fage, fstate, fgender) {

  # Do cob separately because it is a bit tricky
  if (fcob %in% c('non-australia', 'non-aus-eng')) {
    # Special case - not born in Australia/born overseas
    # Use same migration rate for CALD and overseas born generally
    subDataOz <- data %>% 
      filter(year %in% 2004:2014) %>%
      filter(age %in% fage, cob == 'Australia', 
        state %in% fstate, gender %in% fgender) %>% 
      group_by(year) %>%
      summarise(departures = sum(nom),
        erp = sum(erp))
    
    subDataAll <-  data %>% 
      filter(year %in% 2004:2014) %>%
      filter(age %in% fage, cob == 'all', 
        state %in% fstate, gender %in% fgender) %>% 
      group_by(year) %>%
      summarise(departures = sum(nom),
        erp = sum(erp))
     
     subData <- tibble(year = subDataOz$year,
       departures = subDataAll$departures - subDataOz$departures,
       erp = subDataAll$erp - subDataOz$erp) %>%
      mutate(migrate = departures / erp)
    
  } else if (fcob =='non-aus-nz') {
    # Special case - not born in Australia or NZ
    subData <- data %>%
      filter(!(cob %in% c('Australia', 'New Zealand', 'all')))
    
    subDataOz <- data %>% 
      filter(year %in% 2004:2014) %>%
      filter(age %in% fage, cob == 'Australia', 
        state %in% fstate, gender %in% fgender) %>% 
      group_by(year) %>%
      summarise(departures = sum(nom),
        erp = sum(erp))
    
    subDataNz <- data %>% 
      filter(year %in% 2004:2014) %>%
      filter(age %in% fage, cob == 'New Zealand', 
        state %in% fstate, gender %in% fgender) %>% 
      group_by(year) %>%
      summarise(departures = sum(nom),
        erp = sum(erp))
    
    subDataAll <-  data %>% 
      filter(year %in% 2004:2014) %>%
      filter(age %in% fage, cob == 'all', 
        state %in% fstate, gender %in% fgender) %>% 
      group_by(year) %>%
      summarise(departures = sum(nom),
        erp = sum(erp))
    
     subData <- tibble(year = subDataOz$year,
       departures = subDataAll$departures - subDataOz$departures - 
         subDataNz$departures,
       erp = subDataAll$erp - subDataOz$erp - subDataNz$erp) %>%
      mutate(migrate = departures / erp)
  
  } else if (fcob =='mesc') {   
    # Assume same as overall
    subData <-  data %>% 
      filter(year %in% 2004:2014) %>%
      filter(age %in% fage, cob == 'all', 
        state %in% fstate, gender %in% fgender) %>% 
      group_by(year) %>%
      summarise(departures = sum(nom),
        erp = sum(erp)) %>%
      mutate(migrate = departures / erp)
      
  } else {
    subData <- data %>% 
      filter(year %in% 2004:2014) %>%
      filter(age %in% fage, cob %in% fcob, state %in% fstate, 
        gender %in% fgender) %>% 
      group_by(year) %>%
      summarise(departures = sum(nom),
        erp = sum(erp)) %>%
      mutate(migrate = departures / erp)
  }
  
  return(subData)
  
}

predictRates <- function(subrate, allrate, year) {
  relRate <- data.frame(relrate = subrate/ allrate, 
    year = 2004:2014)
  lmRate <- lm(relrate ~ year, data = relRate)
  adjust <- predict(lmRate, 
    data.frame(year = 1980:year))
  return(adjust)
}

# Overall relative rate for a specified population -----------------------
GetMigrate <- function(finalYear, nomData, targetAge, targetGender
  ,targetExposure, targetCob, targetAtsi,targetLocalRegion, targetState,
  targetGlobalRegion, assumeAdult = TRUE, propMale = NULL) {
  
  
  # Sort out age categories - a bit complicated because we generally assume
  # HIV-positive are adults and ERP data unavailable in 5 year bins for 
  # > 75 years old "a75-79" contains all people older than > 75. Older bins 
  # contain the same > 75 ERP value. As there are not many departures in 
  # the > 75 we exclude them and assume the same rate as the 75+ age group.
  if (length(targetAge) == 1 && targetAge[1] == "all") {
    if (assumeAdult) {
      adjustAges <- c("a15_19", "a20_24", "a25_29", "a30_34", "a35_39", 
        "a40_44", "a45_49", "a50_54", "a55_59", "a60_64", 
        "a65_69", "a70_74", "a75_79")
    } else {
      adjustAges <- "all"
    }
  } else {
    # This seems too complicated but I think it works. 
    if (any(c("a80_84", "a85+") %in% targetAge)) {
      if(all(targetAge %in% c("a80_84", "a85+"))) {
        # ftargetAge = "a80_84", "a85+", or c("a80_84", "a85+"). 
        # Replace with rate for "a75_79"
        adjustAges <- "a75_79"
      } else {
        # Contains other ages Remove "a80_84" and "a85+"
        adjustAges <- targetAge[!(targetAge %in% c("a80_84", "a85+"))]
      }
    } else {
      # Use the specified age
      adjustAges <- targetAge
    }
  }
  
  # Adjust for gender ----------------------------------------------------
  
  # All data 
  allData <- extractData(nomData, "all", "all", "all", "all")
  
  # First setup base adjustment based on gender and exposure
  if (targetGender == "male" || 
      (length(targetExposure) == 1 && targetExposure[1] == "msm")) {
    
    # Male data and rates
    maleData <- extractData(nomData, targetCob, adjustAges, targetState, 
      "male")
    relmrate <- predictRates(maleData$migrate, allData$migrate,
      finalYear)
    
  } else if (targetGender == "female") {
    
    # Female data and rates
    femaleData <- extractData(nomData, targetCob, adjustAges, 
      targetState, "female")
    relmrate <- predictRates(femaleData$migrate, allData$migrate,
      finalYear)
    
    
  } else {
    if (is.null(propMale)) {
      # Don't adjust for gender  
      subData <-  extractData(nomData, targetCob, adjustAges, 
        targetState, "all") 
      relmrate <- predictRates(subData$migrate, allData$migrate,
        finalYear)
    } else {
    # Adjust for gender using propMale 
    
    # Male and female calculations
    maleData <- extractData(nomData, targetCob, adjustAges, targetState, 
      "male")
    adjustMales <- predictRates(maleData$migrate, allData$migrate,
      finalYear)
    femaleData <- extractData(nomData, targetCob, adjustAges, 
      targetState, "female")
    adjustFemales <- predictRates(femaleData$migrate, allData$migrate,
      finalYear)  
    
    # Adjust for gender
    relmrate <- propMale * adjustMales  + (1 - propMale) * adjustFemales   
    }
  }
  
  # Return final relative rate
  return(relmrate)
}

# Function for all age groups ---------------------------------------------
GetMigrateAge <- function(year, nomData, targetGender, targetExposure, 
  targetCob, targetAtsi,targetLocalRegion, targetState, 
  targetGlobalRegion, propMale = NULL) {
  
  ages <- c("a00_04", "a05_09", "a10_14", "a15_19", "a20_24", "a25_29",
    "a30_34", "a35_39", "a40_44", "a45_49", "a50_54", "a55_59", "a60_64", 
    "a65_69", "a70_74", "a75_79", "a80_84", "a85+")
  
  relAgeMigrate <- matrix(0, nrow = length(ages), ncol = length(1980:year))
  rownames(relAgeMigrate) <- ages
  
  # Loop over ages 
  for (age in ages) {
    
    if (!is.null(propMale)) {
      propMaleAge <- as.vector(propMale[, age])
    } else {
      propMaleAge <- NULL
    }
    
    tempMigrate <- GetMigrate(year, nomData, age, targetGender,
      targetExposure, targetCob, targetAtsi,targetLocalRegion, 
      targetState, targetGlobalRegion, assumeAdult = FALSE, 
      propMale = propMaleAge)
    
    relAgeMigrate[age, ] <- tempMigrate
  }
  
  return(relAgeMigrate)
}
