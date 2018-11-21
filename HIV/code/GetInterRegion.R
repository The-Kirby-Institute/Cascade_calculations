## R function to calculate interregional migration rates within Australia

# Richard T. Gray

# This script contains functions for calculating the interstate/interregion
# migration rate for a specified sub-population. 

# Define local functions --------------------------------------------------
extractInterData <- function(interData, nomData, fage, fregion, fsex) {
  
  # Fix up age band for nomData
  if (fage == "a75+") {
    nomAge <- "a75_79"
  } else {
    nomAge <- fage
  }
  
  subInterData <- interData %>% 
    filter(year %in% 2004:2014) %>% # only have erp data to 2014
    filter(age %in% fage, 
      region %in% fregion, sex %in% fsex) %>% 
    group_by(year) %>%
    spread(type, value) %>%
    summarise(departures = sum(departures),
      arrivals = sum(arrivals)) %>%
    ungroup()
  
  subErpData <- nomData %>% 
    filter(year %in% 2004:2014) %>%
    filter(age %in% nomAge, cob == "all",
      state %in% fregion, gender %in% fsex) %>% 
    group_by(year) %>%
    summarise(erp = sum(erp)) %>%
    ungroup()
  
  allErpData <- nomData %>% 
    filter(year %in% 2004:2014) %>%
    filter(age %in% nomAge, cob == "all",
      state %in% "all", gender %in% fsex) %>% 
    group_by(year) %>%
    summarise(erp = sum(erp)) %>%
    ungroup()
  
  subData <- inner_join(subInterData, subErpData, by = "year") %>%
    mutate(allerp = allErpData$erp)
  
  return(subData)
}

predictInterRates <- function(interRate, year) {
  moveRate <- data.frame(rate = interRate, year = 2004:2014)
  lmRate <- lm(rate ~ year, data = moveRate)
  movementRate <- predict(lmRate, data.frame(year = 1980:year))
  return(movementRate)
}

# Overall rate for a specified population ------------------------
GetInterRegion <- function(finalYear, nomData, interstateData, 
  interRegionData, targetGender, targetAge, targetState, targetRegion, 
  assumeAdult = TRUE, propMale = NULL) {
  
  # First need to sort out if it is regional or state estimate
  if (targetState[1] != "all" && targetRegion[1] != "all") {
    stop("Cannot do state and regional estimates at same time")
  }
  
  if (targetState[1] == "all" && targetRegion[1] == "all") {
    stop("Cannot do interstate calculations for all of Australia")
  }
  
  allYears <- 1980:finalYear
  hivInterRegion <- tibble(year = allYears)
  
  # Now do calculations
  if (length(targetRegion) == 1 && targetRegion[1] == "all") {
    # We are doing state calculations 
    # First estimate erp for each state
    # states <- c("nsw", "vic", "qld", "nt", "wa", "sa", "tas", "act", "all")
    
    # Sort out age categories - a bit complicated because we generally assume 
    # HIV-positive are adults and ERP data unavailable in 5 year bins for 
    # > 75 years old "a75-79" contains all people older than > 75. Older bins 
    # contain the same > 75 ERP value. As there are not many departures in 
    # the > 75 we exclude them and assume the same rate as the 75+ age group.
    if (length(targetAge) == 1 && targetAge[1] == "all") {
      if (assumeAdult) {
        adjustAges <- c("a15_19", "a20_24", "a25_29", "a30_34", "a35_39", 
          "a40_44", "a45_49", "a50_54", "a55_59", "a60_64", 
          "a65_69", "a70_74", "a75+")
      } else {
        adjustAges <- "all"
      }
    } else {
      # This seems too complicated but I think it works. 
      if (any(c("a75_79", "a80_84", "a85+") %in% targetAge)) {
        if(all(targetAge %in% c("a75_79", "a80_84", "a85+"))) {
          # ftargetAge = "a75_79", "a80_84", "a85+", or 
          # c("a75_79", "a80_84", "a85+"). Replace with rate for "a75+"
          adjustAges <- "a75+"
        } else {
          # Contains other ages remove "a75_79", "a80_84" and "a85+"
          adjustAges <- targetAge[!(targetAge %in% c("a75_79", "a80_84", "a85+"))]
        }
      } else {
        # Use the specified age
        adjustAges <- targetAge
      }
    }
    
    # Adjust for sex ------------------------------------------------------
    
    # First setup base adjustment based on gender and exposure
    if (targetGender == "male" || 
        (length(targetExposure) == 1 && targetExposure[1] == "msm")) {
      
      # Male data and rates
      maleData <- extractInterData(interData, nomData, adjustAges, 
        targetState, "male")
      
      departrate <- predictInterRates(maleData$departures / maleData$erp, 
        finalYear)
      arriverate <- predictInterRates(maleData$arrivals / 
          (maleData$allerp - maleData$erp), finalYear)
      
    } else if (targetGender == "female") {
      
      # Female data and rates
      femaleData <- extractInterData(interstateData, nomData, adjustAges, 
        targetState, "female")
      
      departrate <- predictInterRates(femaleData$departures / 
          femaleData$erp, 
        finalYear)
      arriverate <- predictInterRates(femaleData$arrivals / 
          (femaleData$allerp - femaleData$erp), finalYear)
      
    } else {
      if (is.null(propMale)) {
        # Don't adjust for gender  
        allData <-  extractInterData(interstateData, nomData, adjustAges, 
          targetState, "all") 
        
        departrate <- predictInterRates(allData$departures / allData$erp, 
          finalYear)
        arriverate <- predictInterRates(allData$arrivals / 
            (allData$allerp - allData$erp), finalYear)
        
      } else {
        # Adjust for gender using propMale 
        stop("Weighted adjustement by sex is not ready yeat")
        # Male and female calculations
        # maleData <- extractInterData(interstateData, nomData, adjustAges, 
        #   targetState, "male")
        # 
        # departrateMales <- predictInterRates(maleData$departures / 
        #     maleData$erp, finalYear)
        # arriverateMales <- predictInterRates(maleData$arrivals / 
        #     (maleData$allerp - maleData$erp), finalYear)
        # 
        # femaleData <- extractInterData(interstateData, nomData, adjustAges, 
        #   targetState, "female")
        # 
        # departrateFemales <- predictInterRates(femaleData$departures / 
        #     femaleData$erp, finalYear)
        # arriverateFemales <- predictInterRates(femaleData$arrivals / 
        #     (femaleData$allerp - femaleData$erp), finalYear)  
        # 
        # # Adjust for gender - May need to change propRate for inside and outside 
        # # the region! 
        # departrate <- propMale * departrateMales  + (1 - propMale) * 
        #   departrateFemales
        # arriverate <- propMale * arriverateMales  + (1 - propMale) * 
        #   arriverateFemales   
      }
    }
    
    # Return final rates
    hivInterRegion$departrate <- departrate 
    hivInterRegion$arriverate <- arriverate  
    
  } else {
    # We are doing regional SA4 level calculations 
  }
  
  return(hivInterRegion)
}

# Function for all age groups ---------------------------------------------
GetInterRegionAge <- function(finalYear, nomData, interstateData, 
  interRegionData, targetGender, targetState, targetRegion,
  propMale = NULL) {
  
  # Specify age bins
  ages <- c("a0_4", "a5_9", "a10_14", "a15_19", "a20_24", "a25_29",
    "a30_34", "a35_39", "a40_44", "a45_49", "a50_54", "a55_59", "a60_64", 
    "a65_69", "a70_74", "a75_79", "a80_84", "a85+")
  
  # Initialize outputs
  hivInterAgeDepart <- matrix(0, nrow = length(ages), 
    ncol = length(1980:finalYear))
  rownames(hivInterAgeDepart) <- ages
  hivInterAgeArrive <- hivInterAgeDepart
  
  # Loop over ages 
  for (age in ages) {
    
    if (!is.null(propMale)) {
      propMaleAge <- as.vector(propMale[, age])
    } else {
      propMaleAge <- NULL
    }
    
    tempInterRegion <- GetInterRegion(finalYear, nomData, interstateData, 
      interRegionData, targetGender, age, targetState, targetRegion, 
      assumeAdult = FALSE, propMale = propMaleAge)
    
    # Fill output matrices
    hivInterAgeDepart[age, ] <- tempInterRegion$departrate
    hivInterAgeArrive[age, ] <- tempInterRegion$arriverate
  }
  
  # Return results
  return(list(hivInterAgeDepart, hivInterAgeArrive))
}
