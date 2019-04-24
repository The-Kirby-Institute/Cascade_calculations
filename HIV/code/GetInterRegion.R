## R function to calculate interregional migration rates within Australia

# Richard T. Gray

# This script contains functions for calculating the interstate/interregion
# migration rate for a specified sub-population. 

# Define local functions --------------------------------------------------
extractInterData <- function(interData, nomData, fage, fregion, fsex) {
  
  # Fix up age band for nomData
  if (length(fage) == 1 && fage[1] == "a75+") { 
    # Might produce a warning which can be ignored
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

extractRegionData <- function(regionData, nomData, fage, 
  erpAges, fregion, fsex) {
  
  subRegionData <- regionData %>%
    filter(year %in% 2007:2014) %>% # ERP only up to 2014
    filter(age %in% fage, region %in% fregion, 
      sex %in% fsex) %>%
    group_by(year) %>%
    summarise(departures = sum(departures),
      arrivals = sum(arrivals), erp = sum(erp)) %>%
    ungroup()
  
  allErpData <- nomData %>% 
    filter(year %in% 2007:2014) %>% # Regional data only from 2007
    filter(age %in% erpAges, cob == "all",
      state %in% "all", gender %in% fsex) %>% 
    group_by(year) %>%
    summarise(erp = sum(erp)) %>%
    ungroup()
  
  subData <- subRegionData %>%
    mutate(allerp = allErpData$erp)
  
  return(subData)
  
  
}

predictInterRates <- function(interRate, endYear, dataYears = 2004:2014) {
  moveRate <- data.frame(rate = interRate, year = dataYears)
  lmRate <- lm(rate ~ year, data = moveRate)
  movementRate <- predict(lmRate, data.frame(year = 1980:endYear))
  return(movementRate)
}

# Overall rate for a specified population ------------------------
GetInterRegion <- function(finalYear, nomData, interstateData, 
  interRegionData, targetGender, targetAge, targetState, targetRegion, 
  assumeAdult = TRUE, propMale = NULL) {
  
  # targetRegion <- "all"
  
  # First need to sort out if it is regional or state estimate
  # if (targetState[1] != "all" && targetRegion[1] != "all") {
  #   stop("Cannot do state and regional estimates at same time")
  # }
  
  if (targetState[1] == "all" && targetRegion[1] == "all") {
    stop("Cannot do interstate calculations for all of Australia")
  }
  
  allYears <- 1980:finalYear
  hivInterRegion <- tibble(year = allYears)
  
  # Now do calculations
  if (length(targetRegion) == 1 && targetRegion[1] == "all") {
    # We are doing state calculations 
    # First estimate erp for each state
    # states <- c("nsw", "vic", "qld", "nt", "wa", "sa", "tas", "act",
    #  "all")
    
    # Sort out age categories - a bit complicated because we generally
    # assume HIV-positive are adults and ERP data unavailable in 5 year 
    # bins for 
    # > 75 years old "a75-79" contains all people older than > 75. Older 
    # bins contain the same > 75 ERP value. As there are not many 
    # departures in 
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
          adjustAges <- targetAge[!(targetAge %in% c("a75_79", "a80_84",
            "a85+"))]
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
      maleData <- extractInterData(interstateData, nomData, adjustAges, 
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
        stop("Weighted adjustement by sex is not ready yet")
        # Male and female calculations
        # maleData <- extractInterData(interstateData, nomData, adjustAges, 
        #   targetState, "male")
        # 
        # departrateMales <- predictInterRates(maleData$departures / 
        #     maleData$erp, finalYear)
        # arriverateMales <- predictInterRates(maleData$arrivals / 
        #     (maleData$allerp - maleData$erp), finalYear)
        # 
        # femaleData <- extractInterData(interstateData, nomData, 
        #   adjustAges, targetState, "female")
        # 
        # departrateFemales <- predictInterRates(femaleData$departures / 
        #     femaleData$erp, finalYear)
        # arriverateFemales <- predictInterRates(femaleData$arrivals / 
        #     (femaleData$allerp - femaleData$erp), finalYear)  
        # 
        # # Adjust for gender - May need to change propRate for inside and
        #  outside 
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
    # We are doing regional SA3 level calculations 
    
    # SA3 data is in greater than 5 year age groups so need
    # to adjust. We assume the same inter-region movement rates for
    # all age groups within the larger age group. 
    
    if (length(targetAge) == 1 && targetAge[1] == "all") {
      # We generally assume HIV-positive are adults and targetAge is in 5 
      # year groups but data is in larger bins
      if (assumeAdult) {
        adjustAges <- c("a15_24", "a25_44", "a45_64", "a65+")
        erpAges <- c("a15_19", "a20_24", "a25_29", "a30_34", "a35_39", 
          "a40_44", "a45_49", "a50_54", "a55_59", "a60_64", 
          "a65_69", "a70_74", "a75+")
      } else {
        adjustAges <- "all"
        erpAges <- adjustAges
      }
    } else {
      # If not all or an adult then must be a specific age
      # TODO: Maybe do this for state movement as well
      if (targetAge %in% c("a0_4", "a5_9", "a10_14")) {
        adjustAges <- "a0_14"
        erpAges <- c("a0_4", "a5_9", "a10_14")
      } else if (targetAge %in% c("a15_19", "a20_24")) {
        adjustAges <- "a15_24"
        erpAges <- c("a15_19", "a20_24")
      } else if (targetAge %in% c("a25_29", "a30_34", "a35_39", "a40_44")) {
        adjustAges <- "a25_44"
        erpAges <- c("a25_29", "a30_34", "a35_39", "a40_44")
      } else if (targetAge %in% c("a45_49", "a50_54", "a55_59", "a60_64")) {
        adjustAges <- "a45_64"
        erpAges <- c("a45_49", "a50_54", "a55_59", "a60_64")
      } else if (targetAge %in% c("a65_69", "a70_74", "a75_79", "a80_84", 
        "a85+")) {
        adjustAges <- "a65+"
        erpAges <- c("a65_69", "a70_74", "a75_79", "a80_84", "a85+")
      } else {
        # Must be an incorrect age
        stop("Incorrect age for inter region movement")
      }
    }
    
    # Adjust for sex ------------------------------------------------------
    
    # First setup base adjustment based on gender and exposure
    if (targetGender == "male" || 
        (length(targetExposure) == 1 && targetExposure[1] == "msm")) {
      
      # Male data and rates
      # maleData <- interRegionData %>%
      #   filter(year %in% 2007:2014) %>% # ERP only up to 2014
      #   filter(age %in% adjustAges, region %in% targetLocalRegion, 
      #     sex == "male") %>%
      #   group_by(year) %>%
      #   summarise(departures = sum(departures),
      #     arrivals = sum(arrivals), erp = sum(erp)) %>%
      #   ungroup()
      # 
      # maleAllData <- nomData %>% 
      #   filter(year %in% 2007:2014) %>% # Regional data only from 2007
      #   filter(age %in% erpAges, cob == "all",
      #     state %in% "all", gender == "male") %>% 
      #   group_by(year) %>%
      #   summarise(erp = sum(erp)) %>%
      #   ungroup()
      
      maleData <- extractRegionData(interRegionData, nomData, adjustAges, 
        erpAges, targetRegion, "male")
      
      departrate <- predictInterRates(maleData$departures / maleData$erp, 
        finalYear, dataYears = maleData$year)
      
      arriverate <- predictInterRates(maleData$arrivals / 
          (maleData$allerp - maleData$erp), finalYear, 
        dataYears = maleData$year)
      
    } else if (targetGender == "female") {
      
      # Female data and rates
      femaleData <- extractRegionData(interRegionData, nomData, adjustAges, 
        erpAges, targetRegion, "female")
      
      departrate <- predictInterRates(femaleData$departures / 
          femaleData$erp, finalYear, dataYears = femaleData$year)
      
      arriverate <- predictInterRates(femaleData$arrivals / 
          (femaleData$allerp - femaleData$erp), finalYear,
        dataYears = femaleData$year)
      
    } else {
      if (is.null(propMale)) {
        # Don't adjust for gender  
        allData <-  extractRegionData(interRegionData, nomData, adjustAges, 
        erpAges, targetRegion, "all")
        
        departrate <- predictInterRates(allData$departures / allData$erp, 
          finalYear, dataYears = allData$year)
        
        arriverate <- predictInterRates(allData$arrivals / 
            (allData$allerp - allData$erp), finalYear, 
          dataYears = allData$year)
        
      } else {
        # Adjust for gender using propMale 
        stop("Weighted adjustement by sex is not ready yet")
    
      }
    }
    
    # Return final rates
    hivInterRegion$departrate <- departrate 
    hivInterRegion$arriverate <- arriverate
  }
  
  # Return final movement rates
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
