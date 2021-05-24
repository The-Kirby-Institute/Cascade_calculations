#' Calculate the annual number of people living with diagnosed HIV'
#' 
#' This function calculates the number of people diagnosed with HIV each 
#' year for a specific set of criteria. It also produces files for running 
#' in teh ECDC HIV Modelling Tool and produces projected estimates as 
#' required. It is the main function call by the 1-PldhivCalculations.Rmd 
#' script. A lot of functions need to be sourced prior to calling for this 
#' function to run.   
#' 
#' @param analysisYear Year to run calculations to
#' 
#' @return A list containing multiple data frames
#' 
#' @author Richard T Gray, \email{Rgray@kirby.unsw.edu.au}
#' 
#' @export
#' @import tidyverse
#'  
CalculatePldhiv <- function(analysisYear, saveResults, projectOutput, 
  cascadeName, targetGender, targetAge, targetCob, targetExposure, 
  targetAtsi, targetState, targetLocalRegion, targetGlobalRegion, 
  useImputed, nImputedSets, excludeAborig, doRetained, doUnique, 
  yearUnique, ecdcData, ecdcVersion, excludeOS, projectPldhiv, projectYear, 
  projectName, projectOption, projectDecrease, hivData, allYears, hivBase, 
  hivAdjustments, cleanNom, absInterstate, absInterRegion, ageList, 
  yearList, hivAgeDeath) {
  
  # Initialise optional outputs
  uniqueNotifications <- NULL
  pldhivAllFuture <- NULL
  pldhivAllMinFuture <- NULL
  pldhivAllMaxFuture <- NULL
  hivDiagnosedFuture <- NULL
  
  
  ## Error and option checking -------------------------------------------
  # Error checking and changes in options based on cascade parameters
  # Check lengths of all cascade variables and options
  
  if (sum(c(targetGender, targetAge, targetCob, targetExposure, targetAtsi, 
    targetState, targetLocalRegion, targetGlobalRegion) != "all") > 1 && 
      useImputed == FALSE) {
    if (targetAtsi != "all" && targetCob == "Australia") {
      # We are okay so continue as is
      print("Doing Australian born Indigenous or non-Indigenous so don't 
        need imputed data set")
    } else {
      warning("Multiple selections for subsetting non-imputed data set: 
        SubHivSet() will not work correctly. Changed to imputed data set")
      useImputed <- TRUE
      nImputedSets <- 10 # use default
    }
  }
  
  # TODO: check lengths of all the variables and make sure they equal 
  # numCascades
  
  if ((length(targetGender) > 1) || (length(targetAtsi) > 1)) {
    stop("Error: targetGender or targetAtsi not a single value") 
  }
  
  if (targetAtsi != "all" && targetCob[1] != "Australia" &&
      length(targetCob) != 1) {
    stop("Error: indigenous status only valid for Australian born")   
  }
  
  # Change script options based on cascade parameters and other options---
  interState <- ifelse(targetState[1] != "all", TRUE, FALSE)
  doAge <- ifelse(targetAge == "split", TRUE, FALSE) # TRUE for ageing
  doRetained <- ifelse(doAge, FALSE, doRetained) # Turn off for ageing
  if (doRetained) {
    # Need to specify years for reatined in care calculations
    # This is simply a proportion based on James McMahon's paper
    # which suggested 92-98% of PLDHIV were retained in care. 
    retainedYears <- 2013:analysisYear
  }
  if (targetGlobalRegion != "all") {
    # Global region set then assume everyone is not born in Australia
    targetCob <- "non-australia" 
  }
  
  # Settings for doing deduplication analysis-----------------------------
  doUnique <- ifelse(targetAtsi == "indigenous", FALSE, doUnique)
  yearUnique <- ifelse(is.null(yearUnique), 2100, yearUnique) # Well past
  saveUnique <- ifelse(!saveResults, FALSE, saveUnique)
  
  # Settings for generating ECDC model inputs-----------------------------
  ecdcData <- ifelse(doAge, FALSE, ecdcData) # Turn off for ageing
  ecdcData <- ifelse(targetLocalRegion != "all", FALSE, ecdcData)
  excludeOS <- ifelse(!ecdcData, FALSE, excludeOS) # Turn off
  ecdcModel <- cascadeName # name
  if (excludeOS) {
    # If excludeOS don't save the pldhiv estimates as we are only using 
    # this for ECDC calculations
    saveResults <- FALSE
    doRetained <- FALSE
    projectPldhiv <- FALSE
  }
  
  # Store paramaters for saving-------------------------------------------
  hivParams <- data.frame(cascadeName, targetGender, targetAge, targetCob,
    targetExposure, targetAtsi, targetState, targetLocalRegion,
    targetGlobalRegion, excludeAborig, doRetained,
    doUnique, yearUnique, ecdcData, ecdcVersion, excludeOS, projectPldhiv,
    projectYear, projectName, projectOption, projectDecrease, analysisYear, 
    interState, doAge)
  
  ## Subset notifications ------------------------------------------------
  # Create subsetted notifications dataframe. 
  
  # Look at the overall notifications first
  if (excludeOS) {
    hivData <- filter(hivData, is.na(previ_diag_overseas))
  }
  
  hivSetAll <- filter(hivData, yeardiagnosis <= analysisYear)
  annDiagsOrig <- hivSetAll %>% 
    group_by(yeardiagnosis) %>%
    summarise(notifications = n()) %>%
    mutate(totalnotifications = cumsum(notifications)) %>%
    ungroup() 
  
  if (excludeAborig) {
    # Set indigenous aside separately to add on later if required
    hivSetIndigenous <- filter(hivSetAll, aboriggroup == 'indigenous')
    hivSetAll <- filter(hivSetAll, aboriggroup == 'non_indigenous')
  }
  
  # Now select/calculate annual notifications for selected categories
  
  if (useImputed) {
    # Use the imputed notifications sets - loop through each set of 
    # notifications subsetting the required ones and at the end calculate 
    # the averages across the sets.
    
    hivDataImputed <- hivSetAll
    
    uniqueNotificationsSets <- tibble()
    uniqueNotificationsAllSets <- tibble()
    
    hivResultsSets <- tibble()
    hivResultsAgeSets <- tibble()
    hivResultsAllSets <- tibble()
    hivResultsAgeAllSets <- tibble()
    
    if (targetGender == "all") {
      propDiagsMaleSets <- tibble()
      propDiagsMaleAllSets <- tibble()
      if (doAge) {
        propDiagsAgeMaleSets <- tibble()
        propDiagsAgeMaleAllSets <- tibble()
      }
    }
    
    if (ecdcData) {
      imputeHivSet <- tibble()
    }
    
    for (ii in 1:nImputedSets) {
      
      tempHivSetAll <- hivDataImputed %>%
        filter(impute_set == ii) %>%
        select(-impute_set, -notification) 
      
      if (excludeAborig) {
        # Set indigenous aside separately to add on later if required
        tmepHivSetIndigenous <- filter(tempHivSetAll, 
          aboriggroup == 'indigenous')
        tempHivSetAll <- filter(tempHivSetAll, 
          aboriggroup == 'non_indigenous')
      }
      
      tempHivSetReturn <- SubHivSetImpute(tempHivSetAll, "all", 
        targetGender, targetExposure, targetCob, targetAtsi, targetState, 
        targetLocalRegion , targetGlobalRegion)
      
      tempHivSet <- tempHivSetReturn[[1]]
      tempHivSetExcluded <- tempHivSetReturn[[2]]
      tempHivSetUnknown <- tibble() # Empty
      
      if (ecdcData) {
        # Need to save all notifications 
        imputeHivSet <- bind_rows(imputeHivSet, tempHivSet)
      }
      
      # HivSetUnknown outputs should be empty
      if (nrow(tempHivSetUnknown) != 0) {
        stop("Unknowns detected in imputed data set.")
      }
      
      # For Interstate and Age we need overall estimates
      if (interState || doAge) {
        tempHivSetReturnAll <- SubHivSetImpute(tempHivSetAll, "all",
          targetGender, targetExposure, targetCob, targetAtsi, "all",
          "all", targetGlobalRegion)
        
        tempHivSetAll <- tempHivSetReturnAll[[1]]
        tempHivSetExcludedAll <- tempHivSetReturnAll[[2]]
        tempHivSetUnknownAll <- tibble() # Empty
        
        # HivSetUnknown outputs should be empty
        if (nrow(tempHivSetUnknownAll) != 0) {
          stop("Unknowns detected in imputed data set.")
        }
      }
      
      # Calculate unique notifications and duplicates for the known set.
      # Assume local region estimates match the state estimates
      if (doUnique) {
        tempHivSetUnique <- SubHivSetImpute(tempHivSetAll, "all", 
          targetGender, targetExposure, "all", "all", targetState, "all", 
          "all")
        
        tempUniqueNotifications <- GetUnique(tempHivSetUnique[[1]], 
          allYears, yearUnique = yearUnique)
        
        # Collate
        uniqueNotificationsSets <- bind_rows(uniqueNotificationsSets, 
          tempUniqueNotifications)
        
        # For Interstate and inter-regional Calculations we need overall
        # national estimates
        if (interState) {
          tempHivSetUniqueAll <- SubHivSetImpute(tempHivSetAll, "all",
            targetGender, targetExposure, "all", "all", "all", "all", 
            "all")
          
          tempUniqueNotificationsAll <- GetUnique(tempHivSetUniqueAll[[1]], 
            allYears, yearUnique = yearUnique) 
          
          # Collate
          uniqueNotificationsAllSets <- bind_rows(uniqueNotificationsAllSets,
            tempUniqueNotificationsAll)
        }
      } 
      
      # Calculation cumulative proportion male. This is used for gender
      # weighted migration rates
      if (targetGender == "all") {
        tempPropDiagsMale <- ProportionMale(tempHivSet, analysisYear, 
          FALSE, targetGender = targetGender)
        tempPropDiagsMaleAll <- ProportionMale(tempHivSetAll, analysisYear, 
          FALSE, targetGender = targetGender)
        
        tempPropDiagsMale <- as_tibble(tempPropDiagsMale) %>%
          add_column(year = allYears, .before = 1)
        tempPropDiagsMaleAll <- as_tibble(tempPropDiagsMaleAll) %>%
          add_column(year = allYears, .before = 1) 
        
        # Collate
        propDiagsMaleSets <- bind_rows(propDiagsMaleSets, 
          tempPropDiagsMale)
        propDiagsMaleAllSets <- bind_rows(propDiagsMaleAllSets,
          tempPropDiagsMaleAll)
        
        if (doAge) {
          tempPropDiagsAgeMale <- ProportionMale(tempHivSet, analysisYear, 
            TRUE, targetGender = targetGender)
          tempPropDiagsAgeMaleAll <- ProportionMale(tempHivSetAll, 
            analysisYear, TRUE, targetGender = targetGender)
          
          # Collate
          propDiagsAgeMaleSets <- bind_rows(propDiagsAgeMaleSets, 
            as_tibble(tempPropDiagsAgeMale))
          propDiagsAgeMaleAllSets <- bind_rows(propDiagsAgeMaleAllSets, 
            as_tibble(tempPropDiagsAgeMaleAll))
        }
      }
      
      # Annual notifications
      tempAnnualDiags <- AnnualDiagnoses(tempHivSet, tempHivSetExcluded, 
        tempHivSetUnknown, allYears, doAge)
      
      tempHivResults <- tempAnnualDiags[[1]]
      tempHivResultsAge <- tempAnnualDiags[[2]] %>%
        as.data.frame() %>%
        rownames_to_column(var = "agegroup")
      
      # Collate
      hivResultsSets <- bind_rows(hivResultsSets, tempHivResults)
      hivResultsAgeSets <- bind_rows(hivResultsAgeSets, tempHivResultsAge)
      
      if (interState || doAge) {
        tempAnnualDiagsAll <- AnnualDiagnoses(tempHivSetAll, 
          tempHivSetExcludedAll, tempHivSetUnknownAll, allYears, doAge)
        
        tempHivResultsAll <- tempAnnualDiagsAll[[1]]
        tempHivResultsAgeAll <- tempAnnualDiagsAll[[2]] %>%
          as.data.frame() %>%
          rownames_to_column(var = "agegroup")
        
        # Collate
        hivResultsAllSets <- bind_rows(hivResultsAllSets, 
          tempHivResultsAll)
        hivResultsAgeAllSets <- bind_rows(hivResultsAgeAllSets,
          tempHivResultsAgeAll)
      }
    }
    
    # Outputs we need - take mean value across all imputed data sets ------
    if (doUnique) {
      uniqueNotifications <- uniqueNotificationsSets %>%
        group_by(year) %>%
        summarise_all(list(mean)) %>%
        ungroup()
      if (interState) {
        uniqueNotificationsAll <- uniqueNotificationsAllSets %>%
          group_by(year) %>%
          summarise_all(list(mean)) %>%
          ungroup()
      }
    }
    
    # For proportion male convert back to vector
    if (targetGender == "all") {
      propDiagsMale <- propDiagsMaleSets %>%
        group_by(year) %>%
        summarise_all(list(mean)) %>%
        ungroup() %>%
        select(value) %>%
        as.matrix() %>%
        as.vector 
      propDiagsMaleAll <- propDiagsMaleAllSets %>%
        group_by(year) %>%
        summarise_all(list(mean)) %>%
        ungroup() %>%
        select(value) %>%
        as.matrix() %>%
        as.vector
      
      if (doAge) {
        propDiagsAgeMale <- propDiagsAgeMaleSets %>%
          group_by(year) %>%
          summarise_all(list(mean)) %>%
          ungroup() %>%
          as.matrix
        propDiagsAgeMaleAll <- propDiagsAgeMaleAllSets %>%
          group_by(year) %>%
          summarise_all(list(mean)) %>%
          ungroup() %>%
          as.matrix
      }
    } else {
      propDiagsMale <- NULL
      propDiagsMaleAll <- NULL
      
      propDiagsAgeMale <- NULL
      propDiagsAgeMaleAll <- NULL
    }

    # Annual notifications
    hivResultsRange <- hivResultsSets %>%
      group_by(year) %>%
      summarise(notifications_min = min(notifications),
        notifications_max = max(notifications)) %>%
      ungroup()
        
    hivResults <- hivResultsSets %>%
      group_by(year) %>%
      summarise_all(list(mean)) %>%
      ungroup() %>%
      left_join(hivResultsRange, by = "year") %>%
      select(year, notifications, notifications_min, notifications_max,
        everything())
    
    hivResultsAge <- hivResultsAgeSets %>%
      group_by(agegroup) %>%
      summarise_all(list(mean)) %>%
      as.data.frame() %>%
      column_to_rownames(var = "agegroup") %>%
      ungroup() %>% 
      as.matrix()
    
    hivResultsAgeMin <- hivResultsAgeSets %>%
      group_by(agegroup) %>%
      summarise_all(list(min)) %>%
      as.data.frame() %>%
      column_to_rownames(var = "agegroup") %>%
      ungroup() %>% 
      as.matrix()
    
    hivResultsAgeMax <- hivResultsAgeSets %>%
      group_by(agegroup) %>%
      summarise_all(list(min)) %>%
      as.data.frame() %>%
      column_to_rownames(var = "agegroup") %>%
      ungroup() %>% 
      as.matrix()
    
    if (interState || doAge) {
      hivResultsAllRange <- hivResultsAllSets %>%
      group_by(year) %>%
      summarise(notifications_min = min(notifications),
        notifications_max = max(notifications)) %>%
      ungroup()
      
      hivResultsAll <- hivResultsAllSets %>%
        group_by(year) %>%
        summarise_all(list(mean)) %>%
        ungroup() %>%
        left_join(hivResultsAllRange, by = "year") %>%
        select(year, notifications, notifications_min, notifications_max,
        everything())
      
      hivResultsAgeAll <- hivResultsAgeAllSets %>%
        group_by(agegroup) %>%
        summarise_all(list(mean)) %>%
        as.data.frame() %>%
        column_to_rownames(var = "agegroup") %>%
        ungroup() %>% 
        as.matrix()
      
      hivResultsAgeAllMin <- hivResultsAgeAllSets %>%
        group_by(agegroup) %>%
        summarise_all(list(min)) %>%
        as.data.frame() %>%
        column_to_rownames(var = "agegroup") %>%
        ungroup() %>% 
        as.matrix()
      
      hivResultsAgeAllMax <- hivResultsAgeAllSets %>%
        group_by(agegroup) %>%
        summarise_all(list(max)) %>%
        as.data.frame() %>%
        column_to_rownames(var = "agegroup") %>%
        ungroup() %>% 
        as.matrix()
    }
    
  } else {
    # Extract subset of notifications we want from data
    
    ##*******************************************************************##
    ## WARNING -- This following subsetting only works as required when 
    ## selecting a single input category for notififcations. If multiple 
    ## input categories are required you need to used the imputed 
    ## notifications version.  
    ##*******************************************************************## 
    if (excludeAborig) {
      # Set indigenous aside separately to add on later if required
      hivSetIndigenous <- filter(hivSetAll, aboriggroup == 'indigenous')
      hivSetAll <- filter(hivSetAll, aboriggroup == 'non_indigenous')
    }
    
    hivSetReturn <- SubHivSet(hivSetAll, "all", targetGender,
      targetExposure, targetCob, targetAtsi, targetState, 
      targetLocalRegion, targetGlobalRegion)
    
    hivSet <- hivSetReturn[[1]]
    hivSetExcluded <- hivSetReturn[[2]]
    hivSetUnknown <- hivSetReturn[[3]]
    
    if (interState || doAge) {
      hivSetReturnAll <- SubHivSet(hivSetAll, "all", targetGender,
        targetExposure, targetCob, targetAtsi, "all", targetLocalRegion, 
        targetGlobalRegion)
      
      hivSetAll <- hivSetReturnAll[[1]]
      hivSetExcludedAll <- hivSetReturnAll[[2]]
      hivSetUnknownAll <- hivSetReturnAll[[3]]
    }
    
    # TEMPORARY HACK for state cascades to fix subsetting issues for 
    # multiple selections with country of birth
    # hivSetExcluded <- filter(hivSetExcluded, state == targetState)
    
    # Calculate unique notifications and duplicates for the known set.
    # Assume local region estimates match the state estimates
    if (doUnique) {
      hivSetUnique <- SubHivSet(hivSet, "all", targetGender,
        targetExposure, "all", "all", targetState, "all", "all")
      
      uniqueNotifications <- GetUnique(hivSetUnique[[1]], allYears, 
        yearUnique = yearUnique)
      
      # For Interstate Calculations we need overall national estimates
      if (interState) {
        hivSetUniqueAll <- SubHivSet(hivSetAll, "all", targetGender,
          targetExposure, "all", "all", "all", "all", "all")
        
        uniqueNotificationsAll <- GetUnique(hivSetUniqueAll[[1]], 
          allYears, yearUnique = yearUnique)
      }
    } 
    
    # Calculation cumulative proportion male. This is used for gender weighted
    # migration rates
    if (targetGender == "all") {
      propDiagsMale <- ProportionMale(hivSet, analysisYear, FALSE)
      propDiagsMaleAll <- ProportionMale(hivSetAll, analysisYear, FALSE)
      
      if (doAge) {
        propDiagsAgeMale <- ProportionMale(hivSet, analysisYear, TRUE,
          targetGender = targetGender)
        propDiagsAgeMaleAll <- ProportionMale(hivSetAll, analysisYear, 
          TRUE, targetGender = targetGender)
      }
    }
    
    # Annual notifications and proportions
    if (targetAtsi == "indigenous") {
      # Assume all indigenous notifications are accounted for
      annualDiags <- AnnualDiagnoses(hivSet,  tibble(), 
        tibble(), allYears, doAge)
    } else {
      annualDiags <- AnnualDiagnoses(hivSet, hivSetExcluded, 
        hivSetUnknown, allYears, doAge)
    }
    
    
    hivResults <- annualDiags[[1]] 
    hivResultsAge <- annualDiags[[2]]
    
    # Assume no uncertainty as it is from a single set
    hivResults$notifications_min <- hivResults$notifications
    hivResults$notifications_max <- hivResults$notifications
    hivResultsAgeMin <- hivResultsAge
    hivResultsAgeMax <- hivResultsAge
    
    if (interState || doAge) {
      
      if (targetAtsi == "indigenous") { 
        # Assume all indigenous notifications are accounted for
        annualDiagsAll <- AnnualDiagnoses(hivSetAll, 
          tibble(), tibble(), allYears, doAge)
      } else {
        annualDiagsAll <- AnnualDiagnoses(hivSetAll, 
          hivSetExcludedAll, hivSetUnknownAll, allYears, doAge)
      }
      
      hivResultsAll <- annualDiagsAll[[1]]
      hivResultsAgeAll <- annualDiagsAll[[2]]
      
      # Assume no uncertainty as it is from a single set
      hivResultsAll$notifications_min <- hivResultsAll$notifications
      hivResultsAll$notifications_max <- hivResultsAll$notifications
      hivResultsAgeAllMin <- hivResultsAgeAll
      hivResultsAgeAllMax <- hivResultsAgeAll
    }
  }
  
  ## Generate base and adjustment estimates ------------------------------
  # Now get the death rates, base migration rates, and proportion stay 
  # rates. Migration rates are now calculated using GetMigrate. 
  # We assume the same deathrate, emigration rate and proportion stay for
  # all local regions based on state, so no targetLocalRegion in call. 

  subsetRates <- GetAdjustments(hivBase, hivAdjustments, 
    "all", targetGender, targetExposure, targetCob, targetAtsi, 
    targetLocalRegion, targetState, targetGlobalRegion) 
  
  # Get the annual proportion unique
  if (doUnique) {
    subsetRates$cumunique <- uniqueNotifications$cumunique 
    subsetRates$annunique <- uniqueNotifications$annunique
  } 
  
  # Get migration rate adjustments - targetLocalregion = "all"
  relMigration <- GetMigrate(analysisYear, cleanNom, "all",
    targetGender, targetExposure, targetCob, targetAtsi, "all",
    targetState, targetGlobalRegion, propMale = propDiagsMale)
  
  # Adjust migration rate for Indigenous population if required
  # Potentially move into GetMigrate
  if (targetAtsi == "indigenous" && length(targetCob) == 1 &&
      targetCob[1] == "Australia") {
    relMigration <- 0
  } else if ((targetAtsi == "non_indigenous" &&
      targetCob[1] == "Australia")|| targetGlobalRegion[1] != "all" ||
      (targetCob[1] != "all" && targetCob[1] != "Australia")) {
    relMigration <- relMigration *
      hivAdjustments$non_aborig_migration
  }
  
  # Final migration rates
  subsetRates$mrate <- relMigration * hivBase$migrationrate
  subsetRates$mrate_lower <- relMigration * hivBase$migrationrate_lower
  subsetRates$mrate_upper <- relMigration * hivBase$migrationrate_upper
  
  # Initialize interstate movement rates
  subsetRates$inter_arriverate <- 0
  subsetRates$inter_departrate <- 0
  
  if (interState) {
    # For interstate calculations also need overall population estimates
    subsetRatesAll <- GetAdjustments(hivBase, hivAdjustments,
      "all", targetGender, targetExposure, targetCob, targetAtsi, 
      "all", "all", targetGlobalRegion)  
    
    if (doUnique) {
      subsetRatesAll$cumunique <- uniqueNotificationsAll$cumunique 
      subsetRatesAll$annunique <- uniqueNotificationsAll$annunique
    }
    
    # Get migration rate adjustments 
    relMigrationAll <- GetMigrate(analysisYear, cleanNom, "all",
      targetGender, targetExposure, targetCob, targetAtsi,
      "all", "all", targetGlobalRegion, 
      propMale = propDiagsMale)
    
    # Adjust migration rate for Indigenous population if required
    # Potentially move into GetMigrate
    if (targetAtsi == "indigenous" && length(targetCob) == 1 &&
        targetCob[1] == "Australia") {
      relMigrationAll <- 0
    } else if ((targetAtsi == "non_indigenous" &&
        targetCob[1] == "Australia")|| targetGlobalRegion[1] != "all" ||
        (targetCob[1] != "all" && targetCob[1] != "Australia")) {
      relMigrationAll <- relMigrationAll *
        hivAdjustments$non_aborig_migration
    }
    
    # Final migration rates
    subsetRatesAll$mrate <- relMigrationAll * hivBase$migrationrate
    subsetRatesAll$mrate_lower <- relMigrationAll *
      hivBase$migrationrate_lower
    subsetRatesAll$mrate_upper <- relMigrationAll *
      hivBase$migrationrate_upper
    
    # Get interstate or inter-region rates - only have data since 1982 
    # assume zero rate for 1980-81. 
    interRegionRates <- GetInterRegion(analysisYear, cleanNom, 
      absInterstate, absInterRegion, "all", "all", targetState, 
      targetLocalRegion, assumeAdult = TRUE)
    
    subsetRates$inter_arriverate <- interRegionRates$arriverate
    subsetRates$inter_departrate <- interRegionRates$departrate
  }

  if (doAge) {
    # Now get relative migration and death rates by age. 
    # Death rates are assumed to be the same for overall and 
    # interstate/interegion
    
    # Relative death rates - based on overall data
    relAgeDeath <- hivAgeDeath %>%
      filter(population == targetGender, year <= analysisYear) %>%
      select(year, age, reldeathrate) %>%
      spread(year, reldeathrate) %>%
      select(-age)
    colnames(relAgeDeath) <- yearList
    rownames(relAgeDeath) <- ageList
    
    # Relative migration rates
    relAgeMigrate <- GetMigrateAge(analysisYear, cleanNom, targetGender, 
      targetExposure, targetCob, targetAtsi, "all", 
      targetState, targetGlobalRegion, propMale = propDiagsAgeMale)
    
    colnames(relAgeMigrate) <- yearList
    rownames(relAgeMigrate) <- ageList
    
    if (interState) {
      # Need overall relative migration rates as well
      relAgeMigrateAll <- GetMigrateAge(analysisYear, cleanNom, 
        targetGender, targetExposure, targetCob, targetAtsi, 
        "all", "all", targetGlobalRegion, 
        propMale = propDiagsAgeMaleAll)

      # Get inter-region rates by age
      interRegionAge <- GetInterRegionAge(analysisYear, cleanNom, 
        absInterstate, absInterRegion, "all", targetState, 
        targetLocalRegion)
      
      interDepartrateAge <- interRegionAge[[1]]
      interArriverateAge <- interRegionAge[[2]]
    }
  }
  
  ## Calculate PLDHIV ----------------------------------------------------
  # This chunk finally calculates the number of people living with 
  # diagnosed HIV
  
  if (interState) {
    # A bit more work to do if we do inter-regional migration
    if (doAge) {
      # Do age based interstate calculations
      # Calculate overall national PLDHIV estimates for interstate 
      # migration - min and max should be the same
      pldhivOverallAll <- LivingDiagnosed(hivResultsAll$notifications,
        subsetRatesAll$annunique, 
        subsetRatesAll$deathrate,
        subsetRatesAll$mrate,
        subsetRatesAll$propstay) %>%
        mutate(year = allYears) %>%
        select(year, everything()) %>%
        as_tibble()
      
      pldhivOverallAllMin <- LivingDiagnosed(hivResultsAll$notifications_min,
        subsetRatesAll$annunique,  
        subsetRatesAll$deathrate_upper,
        subsetRatesAll$mrate_upper,
        subsetRatesAll$propstay_lower) %>%
        mutate(year = allYears) %>%
        select(year, everything()) %>%
        as_tibble()
      
      pldhivOverallAllMax <- LivingDiagnosed(hivResultsAll$notifications_max,
        subsetRatesAll$annunique,  
        subsetRatesAll$deathrate_lower,
        subsetRatesAll$mrate_lower,
        subsetRatesAll$propstay_upper) %>%
        mutate(year = allYears) %>%
        select(year, everything()) %>%
        as_tibble()
      
      # Calculate National pldhiv by age
      pldhivAgeAll <- LivingDiagnosedAge(hivResultsAgeAll,
        subsetRatesAll$annunique, 
        subsetRatesAll$deathrate,
        hivBase$migrationrate, 
        subsetRatesAll$propstay,
        agedeath = relAgeDeath,
        agemigrate = relAgeMigrateAll,
        normalize = pldhivOverallAll$pldhiv)
      
      pldhivAgeAllMin <- LivingDiagnosedAge(hivResultsAgeAllMin,
        subsetRatesAll$annunique, 
        subsetRatesAll$deathrate_upper,
        hivBase$migrationrate_upper, 
        subsetRatesAll$propstay_lower,
        agedeath = relAgeDeath,
        agemigrate = relAgeMigrate,
        normalize = pldhivOverallAllMin$pldhiv)
      
      pldhivAgeAllMax <- LivingDiagnosedAge(hivResultsAgeAllMax,
        subsetRatesAll$annunique, 
        subsetRatesAll$deathrate_lower,
        hivBase$migrationrate_lower, 
        subsetRatesAll$propstay_upper,
        agedeath = relAgeDeath,
        agemigrate = relAgeMigrate,
        normalize = pldhivOverallAllMax$pldhiv)
      
      # Calculate overall (non-age) PLDHIV state estimates for normalization
      pldhivOverall <- LivingDiagnosed(hivResults$notifications,
        subsetRates$annunique, 
        subsetRates$deathrate,
        subsetRates$mrate,
        subsetRates$propstay,
        arrivals = subsetRates$inter_arriverate, 
        departs = subsetRates$inter_departrate,
        pldhiv = pldhivOverallAll$pldhiv) %>%
        mutate(year = allYears) %>%
        select(year, everything()) %>%
        as_tibble()
      
      pldhivOverallMin <- LivingDiagnosed(hivResults$notifications_min,
        subsetRates$annunique,  
        subsetRates$deathrate_upper,
        subsetRates$mrate_upper,
        subsetRates$propstay_lower,
        arrivals = subsetRates$inter_arriverate, 
        departs = subsetRates$inter_departrate,
        pldhiv = pldhivOverallAll$pldhiv) %>%
        mutate(year = allYears) %>%
        select(year, everything()) %>%
        as_tibble()
      
      pldhivOverallMax <- LivingDiagnosed(hivResults$notifications_max,
        subsetRates$annunique,  
        subsetRates$deathrate_lower,
        subsetRates$mrate_lower,
        subsetRates$propstay_upper,
        arrivals = subsetRates$inter_arriverate, 
        departs = subsetRates$inter_departrate,
        pldhiv = pldhivOverallAll$pldhiv) %>%
        mutate(year = allYears) %>%
        select(year, everything()) %>%
        as_tibble()
      
      # Calculate state estimates by age - use overall best estimates for 
      # min/max NOTE: use base migration rate as mrate is calculated 
      # assuming people are aged > 15 years
      pldhivAll <- LivingDiagnosedAge(hivResultsAge,
        subsetRates$annunique, 
        subsetRates$deathrate,
        hivBase$migrationrate, 
        subsetRates$propstay,
        agedeath = relAgeDeath,
        agemigrate = relAgeMigrate,
        arrivals = interArriverateAge,
        departs = interDepartrateAge,
        pldhiv =  pldhivAgeAll, 
        normalize = pldhivOverall$pldhiv)
      
      pldhivAllMin <- LivingDiagnosedAge(hivResultsAgeMin,
        subsetRates$annunique, 
        subsetRates$deathrate_upper,
        hivBase$migrationrate_upper, 
        subsetRates$propstay_lower,
        agedeath = relAgeDeath,
        agemigrate = relAgeMigrate,
        arrivals = interArriverateAge,
        departs = interDepartrateAge,
        pldhiv =  pldhivAgeAll,
        normalize = pldhivOverallMin$pldhiv)
      
      pldhivAllMax <- LivingDiagnosedAge(hivResultsAgeMax,
        subsetRates$annunique, 
        subsetRates$deathrate_lower,
        hivBase$migrationrate_lower, 
        subsetRates$propstay_upper,
        agedeath = relAgeDeath,
        agemigrate = relAgeMigrate,
        arrivals = interArriverateAge,
        departs = interDepartrateAge,
        pldhiv =  pldhivAgeAll,
        normalize = pldhivOverallMax$pldhiv)
      
    } else {
      # Calculate overall first for inter-regional migration
      pldhivOverall <- LivingDiagnosed(hivResultsAll$notifications,
        subsetRatesAll$annunique, 
        subsetRatesAll$deathrate,
        subsetRatesAll$mrate,
        subsetRatesAll$propstay) %>%
        mutate(year = allYears) %>% 
        select(year, everything()) %>%
        as_tibble()
      
      pldhivOverallMin <- LivingDiagnosed(hivResultsAll$notifications_min,
        subsetRatesAll$annunique,  
        subsetRatesAll$deathrate_upper,
        subsetRatesAll$mrate_upper,
        subsetRatesAll$propstay_lower) %>%
        mutate(year = allYears) %>%
        select(year, everything()) %>%
        as_tibble()
      
      pldhivOverallMax <- LivingDiagnosed(hivResultsAll$notifications_max,
        subsetRatesAll$annunique,  
        subsetRatesAll$deathrate_lower,
        subsetRatesAll$mrate_lower,
        subsetRatesAll$propstay_upper) %>%
        mutate(year = allYears) %>%
        select(year, everything()) %>%
        as_tibble()
      
      # Now do regional migration - use overall best estimates for min/max
      pldhivAll <- LivingDiagnosed(hivResults$notifications,
        subsetRates$annunique, 
        subsetRates$deathrate, 
        subsetRates$mrate,
        subsetRates$propstay,
        arrivals = subsetRates$inter_arriverate, 
        departs = subsetRates$inter_departrate, 
        pldhiv = pldhivOverall$pldhiv) %>%
        mutate(year = allYears) %>%
        select(year, everything()) %>%
        as_tibble()
      
      pldhivAllMin <- LivingDiagnosed(hivResults$notifications_min,
        subsetRates$annunique,  
        subsetRates$deathrate_upper,
        subsetRates$mrate_upper,
        subsetRates$propstay_lower,
        arrivals = subsetRates$inter_arriverate,
        departs = subsetRates$inter_departrate,
        pldhiv = pldhivOverall$pldhiv) %>%
        mutate(year = allYears) %>%
        select(year, everything()) %>%
        as_tibble()
      
      pldhivAllMax <- LivingDiagnosed(hivResults$notifications_max,
        subsetRates$annunique,  
        subsetRates$deathrate_lower,
        subsetRates$mrate_lower,
        subsetRates$propstay_upper,
        arrivals = subsetRates$inter_arriverate,
        departs = subsetRates$inter_departrate,
        pldhiv = pldhivOverall$pldhiv) %>%
        mutate(year = allYears) %>%
        select(year, everything()) %>%
        as_tibble()
      
    } 
  } else {
    
    # Do overall calculations only
    if (doAge) {
      # Do age based calculations - age group estimates should almost 
      # always be normalized to the overall estimates as differences in 
      # death rates and migration rates can produce inconsistenices across
      # the age groups. Normalization can be turned off for testing to make
      # sure the results are not too different (comapring 
      # pldhivOverall$pldhiv vs colSums(pldhivAll). 
      
      # Calculate overall (non-age) PLDHIV estimates for normalization
      pldhivOverall <- LivingDiagnosed(hivResults$notifications,
        subsetRates$annunique, 
        subsetRates$deathrate,
        subsetRates$mrate,
        subsetRates$propstay) %>%
        mutate(year = allYears) %>%
        select(year, everything()) %>%
        as_tibble()
      
      pldhivOverallMin <- LivingDiagnosed(hivResults$notifications_min,
        subsetRates$annunique,  
        subsetRates$deathrate_upper,
        subsetRates$mrate_upper,
        subsetRates$propstay_lower) %>%
        mutate(year = allYears) %>%
        select(year, everything()) %>%
        as_tibble()
      
      pldhivOverallMax <- LivingDiagnosed(hivResults$notifications_max,
        subsetRates$annunique,  
        subsetRates$deathrate_lower,
        subsetRates$mrate_lower,
        subsetRates$propstay_upper) %>%
        mutate(year = allYears) %>%
        select(year, everything()) %>%
        as_tibble()
      
      # Calculate age group estimates 
      # NOTE: use base migration rate as mrate is calculated assuming 
      # people are aged > 15 years
      
      pldhivAll <- LivingDiagnosedAge(hivResultsAge,
        subsetRates$annunique, 
        subsetRates$deathrate,
        hivBase$migrationrate, 
        subsetRates$propstay,
        agedeath = relAgeDeath,
        agemigrate = relAgeMigrate,
        normalize = pldhivOverall$pldhiv) 
      
      pldhivAllMin <- LivingDiagnosedAge(hivResultsAgeMin,
        subsetRates$annunique, 
        subsetRates$deathrate_upper,
        hivBase$migrationrate_upper, 
        subsetRates$propstay_lower,
        agedeath = relAgeDeath,
        agemigrate = relAgeMigrate,
        normalize = pldhivOverallMin$pldhiv)
      
      pldhivAllMax <- LivingDiagnosedAge(hivResultsAgeMax,
        subsetRates$annunique, 
        subsetRates$deathrate_lower,
        hivBase$migrationrate_lower, 
        subsetRates$propstay_upper,
        agedeath = relAgeDeath,
        agemigrate = relAgeMigrate,
        normalize = pldhivOverallMax$pldhiv)
      
    } else {
      pldhivAll <- LivingDiagnosed(hivResults$notifications,
        subsetRates$annunique, 
        subsetRates$deathrate,
        subsetRates$mrate,
        subsetRates$propstay) %>%
        mutate(year = allYears) %>%
        select(year, everything()) %>%
        as_tibble()
      
      pldhivAllMin <- LivingDiagnosed(hivResults$notifications_min,
        subsetRates$annunique,  
        subsetRates$deathrate_upper,
        subsetRates$mrate_upper,
        subsetRates$propstay_lower) %>%
        mutate(year = allYears) %>%
        select(year, everything()) %>%
        as_tibble()
      
      pldhivAllMax <- LivingDiagnosed(hivResults$notifications_max,
        subsetRates$annunique,  
        subsetRates$deathrate_lower,
        subsetRates$mrate_lower,
        subsetRates$propstay_upper) %>%
        mutate(year = allYears) %>%
        select(year, everything()) %>%
        as_tibble()
    }
  }
  
  # Store results
  if (doAge) {
    # Store estimates by year and age
    pldhivDf <- as.data.frame(pldhivAll) %>% 
      rownames_to_column(var = "agebin") %>%
      gather("year", "pldhiv", 2:ncol(.)) %>%
      select(year, everything()) %>%
      mutate(year = as.integer(str_sub(year, 2))) %>%
      as_tibble()
    
    pldhivDfMin <- as.data.frame(pldhivAllMin) %>% 
      rownames_to_column(var = "agebin") %>%
      gather("year", "pldhiv", 2:ncol(.)) %>%
      select(year, everything()) %>%
      mutate(year = as.integer(str_sub(year, 2))) %>%
      as_tibble()
    
    pldhivDfMax <- as.data.frame(pldhivAllMax) %>% 
      rownames_to_column(var = "agebin") %>%
      gather("year", "pldhiv", 2:ncol(.)) %>%
      select(year, everything()) %>%
      mutate(year = as.integer(str_sub(year, 2))) %>%
      as_tibble()
    
    hivDiagnosed <- data.frame(stage = "pldhiv",
      year = pldhivDf$year,
      agebin = pldhivDf$agebin,
      value = pldhivDf$pldhiv,
      lower = pldhivDfMin$pldhiv,
      upper = pldhivDfMax$pldhiv) %>%
      as_tibble()
    
    hivDiagnosedOverall <- data.frame(stage = "pldhiv",
      year = allYears,
      value = pldhivOverall$pldhiv,
      lower = pldhivOverallMin$pldhiv,
      upper = pldhivOverallMax$pldhiv)
    
    
  } else {
    # Don't need to store age
    hivDiagnosed <- data.frame(stage = "pldhiv",
      year = allYears,
      value = pldhivAll$pldhiv,
      lower = pldhivAllMin$pldhiv,
      upper = pldhivAllMax$pldhiv)
  }
  
  ## ECDC outputs ---------------------------------------------------------
  if (ecdcData) {
    if (useImputed) {
      normalizeFactor <- rep(1, length(allYears))
      ecdcImputeSets <- nImputedSets
    } else {
      normalizeFactor <-  annualDiags[[1]]$ecdc_normalization
      normalizeFactor[is.nan(normalizeFactor)] <- 1
      ecdcImputeSets <- 1
    }  
    
    # Folder for ECDC output
    ecdcFolder <- file.path(outputFolder, projectOutput)
    
    # ECDC data set
    if (useImputed) {
      hivSetEcdc <- imputeHivSet
    } else {
      hivSetEcdc <- hivSet %>%
        mutate(expgroup = ifelse(is.na(expgroup), "unknown", expgroup))
    }

    if (excludeOS) {
      # excluding OS diagnosis
      dataAll <- EcdcFolders(ecdcFolder, ecdcModel, 
        includeOS = FALSE)
      EcdcFiles(hivSetEcdc, dataAll, propUnique = subsetRates$cumunique,
        propKnown = normalizeFactor, nDataSets = ecdcImputeSets)
    } else {
      # including OS diagnosis
      dataAll <- EcdcFolders(ecdcFolder, ecdcModel)
      EcdcFiles(hivSetEcdc, dataAll, propUnique = subsetRates$cumunique,
        propKnown = normalizeFactor, nDataSets = ecdcImputeSets) 
    }
    
    # Create output directory
    resultsEcdcPath <- file.path(ecdcFolder, "ECDC_models", cascadeName)
    
    #save parameters 
    saveStringParams <- file.path(resultsEcdcPath, "Parameters")
    
    # Write to csv
    write_csv(hivParams, paste0(saveStringParams, ".csv"))
    
    # Save deaths and emigrants into ECDC
    
    # All deaths
    deaths <- pldhivAll %>%
      select(year, deaths) %>%
      rename(all = deaths)
    EcdcWrite(deaths, dataAll[[1]], "deaths")
    
    # All emigrants
    if (interState) {
      emigrants <- pldhivAll %>% 
        select(year, emigrants, diag_departs, inter_departs,
          inter_arrivals) %>%
        mutate(total = emigrants + diag_departs + inter_departs -
            inter_arrivals) %>%
        select(year, total) %>%
        rename(all = total)
    } else {
      emigrants <- pldhivAll %>% 
        select(year, emigrants, diag_departs) %>%
        mutate(total = emigrants + diag_departs) %>%
        select(year, total) %>%
        rename(all = total)
    }
    
    EcdcWrite(emigrants, dataAll[[1]], "emigrants")
    
    hivExpCum <- hivSetEcdc %>% 
      filter(is.na(previ_diag_overseas)) %>%
      group_by(expgroup, yeardiagnosis) %>% 
      summarise(notifications = n() / ecdcImputeSets) %>% 
      ungroup() %>% 
      group_by(expgroup) %>% 
      mutate(cumnotifications = cumsum(notifications)) %>%
      ungroup() %>% 
      select(-notifications) %>%
      spread(expgroup, cumnotifications) 

    if (!("hetero" %in% names(hivExpCum))) hivExpCum$hetero <- 0
    if (!("msm" %in% names(hivExpCum))) hivExpCum$msm <- 0
    if (!("otherexp" %in% names(hivExpCum))) hivExpCum$otherexp <- 0
    if (!("pwid" %in% names(hivExpCum))) hivExpCum$pwid <- 0
    if (!("unknown" %in% names(hivExpCum))) hivExpCum$unknown <- 0
    
    hivExpCum <- hivExpCum %>% 
      select(-unknown) %>%
      gather("expgroup", "cumnotifications", 2:5) %>%
      mutate(cumnotifications = ifelse(is.na(cumnotifications), 0, 
        cumnotifications)) %>%
      group_by(yeardiagnosis) %>% 
      mutate(propnotifications = cumnotifications /
          sum(cumnotifications)) %>%
      mutate(propnotifications = ifelse(is.na(propnotifications), 0, 
        propnotifications)) %>%
      select(-cumnotifications) %>%
      ungroup() %>% 
      spread(expgroup, propnotifications) %>%
      rename(year = yeardiagnosis) 
    if (targetGender != "female") {
      # Add an msm notification to 1981
      hivExpCum$msm[2] <- 1.0
    } 

    expDeaths <- FillDataFrame(1980:max(hivExpCum$year), hivExpCum, 
      cumulative = TRUE)
    expDeaths[, 2:5] <- expDeaths[, 2:5] * 
      matrix(rep(pldhivAll$deaths, 4), ncol = 4)
    
    EcdcWrite(expDeaths, dataAll[[2]], "deaths")
    
    expEmig <- FillDataFrame(1980:max(hivExpCum$year), hivExpCum, 
      cumulative = TRUE)
    expEmig[, 2:5] <- expEmig[, 2:5] * 
      matrix(rep(emigrants$all, 4), ncol = 4)
    
    EcdcWrite(expEmig, dataAll[[2]], "emigrants")
    
  }
  
  ## Append retained -----------------------------------------------------
  if (doRetained) {
    # Perform and append retained in care calculations
    source(file.path(HIVcode, "RetainedCare.R"))
    
    # Load retained in care data - Use McMahon et al data for 2013 onwards. 
    # Given by value, lower, upper. 
    # Initial estimates from study conducted in 2013. Lower (91.4%) and 
    # upper (98.8%) ranges correspond to the range for the percentage 
    # retained after follow-up in McMahon et al., Clinic Network 
    # Collaboration and Patient Tracing to Maximize Retention in HIV Care, 
    # PLOS One, May 26, 2015.
    # A repeat study in 2019 produced an estimate of 96.24% which is only 
    # slightly than the previous estimate. We assume a range of 93% to 99%.
    # 
    # We assume a linear change form the 2013 to the latest value 
    # (currently in 2019). 

    hivParameters <- read.csv(file.path(dataFolder, 
      "individualHIVparameters.csv"), as.is = 1)
    hivParameters <- select(hivParameters, parameter, value)
    
    for (ii in 1:nrow(hivParameters)) {
      assign(hivParameters$parameter[ii], hivParameters$value[ii])
    }

    retained <- seq(vicClinicRetained2013, vicClinicRetained, 
      length.out = length(retainedYears))
    retainedLower <- seq(vicClinicRetainedLower2013, vicClinicRetainedLower,
      length.out = length(retainedYears))
    retainedUpper <- seq(vicClinicRetainedUpper2013, vicClinicRetainedUpper,
      length.out = length(retainedYears))
    
    hivRetained <- RetainedCare(hivDiagnosed, retained, retainedLower,
      retainedUpper, retainedYears)
    
    # Merge with hivDiagnosed
    hivDiagnosed <- bind_rows(hivDiagnosed, hivRetained)
    
  }
  
  ## Save cascade results ------------------------------------------------
  if (saveResults) {
    # Create output directory
    resultsPath <- file.path(outputFolder, projectOutput, cascadeName)
    dir.create(resultsPath, showWarnings = FALSE, recursive = TRUE)
    
    # Save all estimates
    if (doAge) {
      saveStringDetails <- file.path(resultsPath, 
        paste0("pldhiv-", toString(analysisYear), "-age-"))
      write_csv(rownames_to_column(as.data.frame(pldhivAll), 
        var = "agebin"),
        paste0(saveStringDetails, "all.csv"))
      write_csv(rownames_to_column(as.data.frame(pldhivAllMin), 
        var = "agebin"), paste0(saveStringDetails, "min.csv"))
      write_csv(rownames_to_column(as.data.frame(pldhivAllMax), 
        var = "agebin"), paste0(saveStringDetails, "max.csv"))
      
      # Save overall as well
      saveStringDetails <- file.path(resultsPath, 
        paste0("pldhiv-", toString(analysisYear), "-"))
      write_csv(pldhivOverall, paste0(saveStringDetails, "all.csv"))
      write_csv(pldhivOverallMin, paste0(saveStringDetails, "min.csv"))
      write_csv(pldhivOverallMax, paste0(saveStringDetails, "max.csv"))
      
    } else {
      saveStringDetails <- file.path(resultsPath, 
        paste0("pldhiv-", toString(analysisYear), "-"))
      write_csv(pldhivAll, paste0(saveStringDetails, "all.csv"))
      write_csv(pldhivAllMin, paste0(saveStringDetails, "min.csv"))
      write_csv(pldhivAllMax, paste0(saveStringDetails, "max.csv"))
    }
    
    # Save main results
    saveStringPldhiv <- file.path(resultsPath, 
      paste0("HIVpldhivEstimates-", toString(analysisYear)))
    if (doAge) {
      write_csv(hivDiagnosedOverall, paste0(saveStringPldhiv, ".csv"))
      saveStringPldhiv <- paste0(saveStringPldhiv, "-age")
    }
    write_csv(hivDiagnosed, paste0(saveStringPldhiv, ".csv"))
    
    #save parameters 
    saveStringParams <- file.path(resultsPath, "PldhivParameters")
    
    # Write to csv
    write_csv(hivParams, paste0(saveStringParams, ".csv"))
    
    # Write uniqueNotifications to file
    if (doUnique) {
      saveStringUnique <- file.path(resultsPath, "UniqueNotifications")
      write_csv(uniqueNotifications, paste0(saveStringUnique, ".csv"))
    }
  }

  ## Projections ---------------------------------------------------------
  # This chunk is used for generating projections of the number of people 
  # living with diagnosed HIV. To do the projections we simply make
  # assumptions about the future changes to annual diagnoses, deathrates, 
  # migration rates into a specified future period.  
  # 
  # Any change in inputs to LivingDiagnosed function can be manually 
  # specified in the ProjectDiagnoses.R function the default is to keep
  # everything the same as the analysis year. Do not need different 
  # emigration rates for overall and age calculations because the overall 
  # rate assumes all PLDHIV are adults and to align with estimates above up 
  # to analysis year.
  
  if (projectPldhiv) { 
    
    saveProjResults <- saveResults 
    
    # Setup projection years
    projectYears <- (analysisYear + 1):projectYear
    nprojYears <- length(projectYears)
    # totalYears <- c(allYears, projectYears)
    
    # Set-up projection inputs
    # Use trend diagnoses as default - 
    
    # Now do the projections-BAD CODE as it is essentially a copy and paste 
    # of the PLDHIV calculations code above. Just easier to do for the
    # moment.
    if (interState) {
      if (doAge) {
        # Do age based interstate calculations

        # Set-up projection diagnoses - need regional and overall calculations
        diagnosesFuture <- ProjectDiagnoses(hivResults$notifications, 
          allYears, projectDecrease, projectYears, projectOption, 
          diagnosesAge = hivResultsAge)
        
        diagnosesFutureMin <- ProjectDiagnoses(hivResults$notifications_min, 
          allYears, projectDecrease, projectYears, projectOption, 
          diagnosesAge = hivResultsAgeMin)
        
        diagnosesFutureMax <- ProjectDiagnoses(hivResults$notifications_max, 
          allYears, projectDecrease, projectYears, projectOption, 
          diagnosesAge = hivResultsAgeMax)
        
        diagnosesFutureAll <- ProjectDiagnoses(hivResultsAll$notifications, 
          allYears, projectDecrease, projectYears, projectOption, 
          diagnosesAge = hivResultsAgeAll)
        
        diagnosesFutureAllMin <- ProjectDiagnoses(hivResultsAll$notifications_min, 
          allYears, projectDecrease, projectYears, projectOption, 
          diagnosesAge = hivResultsAgeAllMin)
        
        diagnosesFutureAllMax <- ProjectDiagnoses(hivResultsAll$notifications_max, 
          allYears, projectDecrease, projectYears, projectOption, 
          diagnosesAge = hivResultsAgeAllMax)
        
        # Set-up age based deaths and migration factors
        
        # Deaths is a bit tricky because the final row is zero
        relAgeDeathFuture <- cbind(relAgeDeath[, 1:(ncol(relAgeDeath)-1)], 
          matrix(rep(relAgeDeath[, (ncol(relAgeDeath)-1)], nprojYears), 
            ncol = nprojYears), relAgeDeath[, ncol(relAgeDeath)])
        colnames(relAgeDeathFuture) <- paste0("y", 
          as.character(c(allYears, projectYears)))
        
        # Population movement is straight forward
        relAgeMigrateFuture <- cbind(relAgeMigrate,
          matrix(rep(relAgeMigrate[,ncol(relAgeMigrate-1)], nprojYears), 
            ncol = nprojYears))
        colnames(relAgeMigrateFuture) <- paste0("y", 
          as.character(c(allYears, projectYears)))
        
        relAgeMigrateFutureAll <- cbind(relAgeMigrateAll,
          matrix(rep(relAgeMigrateAll[,ncol(relAgeMigrateAll-1)], nprojYears), 
            ncol = nprojYears))
        colnames(relAgeMigrateFutureAll) <- paste0("y", 
          as.character(c(allYears, projectYears)))
        
        interArriverateAgeFuture <- cbind(interArriverateAge,
          matrix(rep(interArriverateAge[,ncol(interArriverateAge-1)], 
            nprojYears), ncol = nprojYears))
        colnames(interArriverateAgeFuture) <- paste0("y", 
          as.character(c(allYears, projectYears)))
        
        interDepartrateAgeFuture <- cbind(interDepartrateAge,
          matrix(rep(interDepartrateAge[,ncol(interDepartrateAge-1)], 
            nprojYears), ncol = nprojYears))
        colnames(interDepartrateAgeFuture) <- paste0("y", 
          as.character(c(allYears, projectYears)))
      
        # Calculate overall national PLDHIV estimates for interstate 
        # migration
        pldhivOverallAllFuture <- LivingDiagnosed(diagnosesFutureAll[[1]],
          ProjVec(subsetRatesAll$annunique, nprojYears),
          ProjVec(subsetRatesAll$deathrate, nprojYears),
          ProjVec(subsetRatesAll$mrate, nprojYears),
          ProjVec(subsetRatesAll$propstay, nprojYears)) %>%
          mutate(year = c(allYears, projectYears)) %>%
          select(year, everything()) %>%
          as_tibble()
        
        pldhivOverallAllMinFuture <- LivingDiagnosed(diagnosesFutureAllMin[[1]],
          ProjVec(subsetRatesAll$annunique, nprojYears),
          ProjVec(subsetRatesAll$deathrate_upper, nprojYears),
          ProjVec(subsetRatesAll$mrate_upper, nprojYears),
          ProjVec(subsetRatesAll$propstay_lower, nprojYears)) %>%
          mutate(year = c(allYears, projectYears)) %>%
          select(year, everything()) %>%
          as_tibble()

        pldhivOverallAllMaxFuture <- LivingDiagnosed(diagnosesFutureAllMax[[1]],
          ProjVec(subsetRatesAll$annunique, nprojYears),
          ProjVec(subsetRatesAll$deathrate_lower, nprojYears),
          ProjVec(subsetRatesAll$mrate_lower, nprojYears),
          ProjVec(subsetRatesAll$propstay_upper, nprojYears)) %>%
          mutate(year = c(allYears, projectYears)) %>%
          select(year, everything()) %>%
          as_tibble()
        
        # Calculate National pldhiv by age
        pldhivAgeAllFuture <- LivingDiagnosedAge(diagnosesFutureAll[[2]],
          ProjVec(subsetRatesAll$annunique, nprojYears),
          ProjVec(subsetRatesAll$deathrate, nprojYears),
          ProjVec(hivBase$migrationrate, nprojYears),
          ProjVec(subsetRatesAll$propstay, nprojYears),
          agedeath = relAgeDeathFuture,
          agemigrate = relAgeMigrateFutureAll,
          normalize = pldhivOverallAllFuture$pldhiv)
        
        pldhivAgeAllMinFuture <- LivingDiagnosedAge(diagnosesFutureAllMin[[2]],
          ProjVec(subsetRatesAll$annunique, nprojYears),
          ProjVec(subsetRatesAll$deathrate_upper, nprojYears),
          ProjVec(hivBase$migrationrate_upper, nprojYears),
          ProjVec(subsetRatesAll$propstay_lower, nprojYears),
          agedeath = relAgeDeathFuture,
          agemigrate = relAgeMigrateFutureAll,
          normalize = pldhivOverallAllMinFuture$pldhiv)

        pldhivAgeAllMaxFuture <- LivingDiagnosedAge(diagnosesFutureAllMax[[2]],
          ProjVec(subsetRatesAll$annunique, nprojYears),
          ProjVec(subsetRatesAll$deathrate_lower, nprojYears),
          ProjVec(hivBase$migrationrate_lower, nprojYears),
          ProjVec(subsetRatesAll$propstay_upper, nprojYears),
          agedeath = relAgeDeathFuture,
          agemigrate = relAgeMigrateFutureAll,
          normalize = pldhivOverallAllMaxFuture$pldhiv)
        
        # Calculate overall (non-age) PLDHIV state estimates for 
        # normalization - use best estimates for min/max
        pldhivOverallFuture <- LivingDiagnosed(diagnosesFuture[[1]],
          ProjVec(subsetRates$annunique, nprojYears),
          ProjVec(subsetRates$deathrate, nprojYears),
          ProjVec(subsetRates$mrate, nprojYears),
          ProjVec(subsetRates$propstay, nprojYears),
          arrivals = ProjVec(subsetRates$inter_arriverate, nprojYears),
          departs = ProjVec(subsetRates$inter_departrate, nprojYears),
          pldhiv = pldhivOverallAllFuture$pldhiv) %>%
          mutate(year = c(allYears, projectYears)) %>%
          select(year, everything()) %>%
          as_tibble()
        
        pldhivOverallMinFuture <- LivingDiagnosed(diagnosesFutureMin[[1]],
          ProjVec(subsetRates$annunique, nprojYears),
          ProjVec(subsetRates$deathrate_upper, nprojYears),
          ProjVec(subsetRates$mrate_upper, nprojYears),
          ProjVec(subsetRates$propstay_lower, nprojYears),
          arrivals = ProjVec(subsetRates$inter_arriverate, nprojYears),
          departs = ProjVec(subsetRates$inter_departrate, nprojYears),
          pldhiv = pldhivOverallAllFuture$pldhiv) %>%
          mutate(year = c(allYears, projectYears)) %>%
          select(year, everything()) %>%
          as_tibble()

        pldhivOverallMaxFuture <- LivingDiagnosed(diagnosesFutureMax[[1]],
          ProjVec(subsetRates$annunique, nprojYears),
          ProjVec(subsetRates$deathrate_lower, nprojYears),
          ProjVec(subsetRates$mrate_lower, nprojYears),
          ProjVec(subsetRates$propstay_upper, nprojYears),
          arrivals = ProjVec(subsetRates$inter_arriverate, nprojYears),
          departs = ProjVec(subsetRates$inter_departrate, nprojYears),
          pldhiv = pldhivOverallAllFuture$pldhiv) %>%
          mutate(year = c(allYears, projectYears)) %>%
          select(year, everything()) %>%
          as_tibble()
        
        # Calculate state estimates by age - use best estimates for 
        # min/max
        pldhivAllFuture <- LivingDiagnosedAge(diagnosesFuture[[2]],
          ProjVec(subsetRates$annunique, nprojYears),
          ProjVec(subsetRates$deathrate, nprojYears),
          ProjVec(hivBase$migrationrate, nprojYears),
          ProjVec(subsetRates$propstay, nprojYears),
          agedeath = relAgeDeathFuture,
          agemigrate = relAgeMigrateFuture,
          arrivals = interArriverateAgeFuture,
          departs = interDepartrateAgeFuture,
          pldhiv =  pldhivAgeAllFuture, 
          normalize = pldhivOverallFuture$pldhiv)
        
        pldhivAllMinFuture <- LivingDiagnosedAge(diagnosesFutureMin[[2]],
          ProjVec(subsetRates$annunique, nprojYears),
          ProjVec(subsetRates$deathrate_upper, nprojYears),
          ProjVec(hivBase$migrationrate_upper, nprojYears),
          ProjVec(subsetRates$propstay_lower, nprojYears),
          agedeath = relAgeDeathFuture,
          agemigrate = relAgeMigrateFuture,
          arrivals = interArriverateAgeFuture,
          departs = interDepartrateAgeFuture,
          pldhiv =  pldhivAgeAllFuture,
          normalize = pldhivOverallMinFuture$pldhiv)

        pldhivAllMaxFuture <- LivingDiagnosedAge(diagnosesFutureMax[[2]],
          ProjVec(subsetRates$annunique, nprojYears),
          ProjVec(subsetRates$deathrate_lower, nprojYears),
          ProjVec(hivBase$migrationrate_lower, nprojYears),
          ProjVec(subsetRates$propstay_upper, nprojYears),
          agedeath = relAgeDeathFuture,
          agemigrate = relAgeMigrateFuture,
          arrivals = interArriverateAgeFuture,
          departs = interDepartrateAgeFuture,
          pldhiv =  pldhivAgeAllFuture,
          normalize = pldhivOverallMaxFuture$pldhiv)
        
      } else {
        
        # Set-up projection diagnoses - need regional and overall calculations
        diagnosesFuture <- ProjectDiagnoses(hivResults$notifications, 
          allYears, projectDecrease, projectYears, projectOption)
        
        diagnosesFutureMin <- ProjectDiagnoses(hivResults$notifications_min, 
          allYears, projectDecrease, projectYears, projectOption)
        
        diagnosesFutureMax <- ProjectDiagnoses(hivResults$notifications_max, 
          allYears, projectDecrease, projectYears, projectOption)
        
        diagnosesFutureAll <- ProjectDiagnoses(hivResultsAll$notifications, 
          allYears, projectDecrease, projectYears, projectOption)
        
        diagnosesFutureAllMin <- ProjectDiagnoses(hivResultsAll$notifications_min, 
          allYears, projectDecrease, projectYears, projectOption)
        
        diagnosesFutureAllMax <- ProjectDiagnoses(hivResultsAll$notifications_max, 
          allYears, projectDecrease, projectYears, projectOption)
        
        # Calculate overall first for inter-regional migration
        pldhivOverallFuture <- LivingDiagnosed(diagnosesFutureAll[[1]],
          ProjVec(subsetRatesAll$annunique, nprojYears), 
          ProjVec(subsetRatesAll$deathrate, nprojYears),
          ProjVec(subsetRatesAll$mrate, nprojYears),
          ProjVec(subsetRatesAll$propstay, nprojYears)) %>%
          mutate(year = c(allYears, projectYears)) %>% 
          select(year, everything()) %>%
          as_tibble()
        
        pldhivOverallMinFuture <- LivingDiagnosed(diagnosesFutureAllMin[[1]],
          ProjVec(subsetRatesAll$annunique, nprojYears),
          ProjVec(subsetRatesAll$deathrate_upper, nprojYears),
          ProjVec(subsetRatesAll$mrate_upper, nprojYears),
          ProjVec(subsetRatesAll$propstay_lower, nprojYears)) %>%
          mutate(year = c(allYears, projectYears)) %>%
          select(year, everything()) %>%
          as_tibble()

        pldhivOverallMaxFuture <- LivingDiagnosed(diagnosesFutureAllMax[[1]],
          ProjVec(subsetRatesAll$annunique, nprojYears),
          ProjVec(subsetRatesAll$deathrate_lower, nprojYears),
          ProjVec(subsetRatesAll$mrate_lower, nprojYears),
          ProjVec(subsetRatesAll$propstay_upper, nprojYears)) %>%
          mutate(year = c(allYears, projectYears)) %>%
          select(year, everything()) %>%
          as_tibble()
        
        # Now do regional migration - use best estimates for min/max
        pldhivAllFuture <- LivingDiagnosed(diagnosesFuture[[1]],
          ProjVec(subsetRates$annunique, nprojYears), 
          ProjVec(subsetRates$deathrate, nprojYears), 
          ProjVec(subsetRates$mrate, nprojYears),
          ProjVec(subsetRates$propstay, nprojYears),
          arrivals = ProjVec(subsetRates$inter_arriverate, nprojYears), 
          departs = ProjVec(subsetRates$inter_departrate, nprojYears),
          pldhiv = pldhivOverallFuture$pldhiv) %>%
          mutate(year = c(allYears, projectYears)) %>%
          select(year, everything()) %>%
          as_tibble()
        
        pldhivAllMinFuture <- LivingDiagnosed(diagnosesFutureMin[[1]],
          ProjVec(subsetRates$annunique, nprojYears),
          ProjVec(subsetRates$deathrate_upper, nprojYears),
          ProjVec(subsetRates$mrate_upper, nprojYears),
          ProjVec(subsetRates$propstay_lower, nprojYears),
          arrivals = ProjVec(subsetRates$inter_arriverate, nprojYears),
          departs = ProjVec(subsetRates$inter_departrate, nprojYears),
          pldhiv = pldhivOverallFuture$pldhiv) %>%
          mutate(year = c(allYears, projectYears)) %>%
          select(year, everything()) %>%
          as_tibble()

        pldhivAllMaxFuture <- LivingDiagnosed(diagnosesFutureMax[[1]],
          ProjVec(subsetRates$annunique, nprojYears),
          ProjVec(subsetRates$deathrate_lower, nprojYears),
          ProjVec(subsetRates$mrate_lower, nprojYears),
          ProjVec(subsetRates$propstay_upper, nprojYears),
          arrivals = ProjVec(subsetRates$inter_arriverate, nprojYears),
          departs = ProjVec(subsetRates$inter_departrate, nprojYears),
          pldhiv = pldhivOverallFuture$pldhiv) %>%
          mutate(year = c(allYears, projectYears)) %>%
          select(year, everything()) %>%
          as_tibble()
        
      } 
    } else {
      # Do overall calculations only
      if (doAge) {
        # Do age based calculations 
        diagnosesFuture <- ProjectDiagnoses(hivResults$notifications, 
          allYears, projectDecrease, projectYears, projectOption, 
          diagnosesAge = hivResultsAge)
        
        diagnosesFutureMin <- ProjectDiagnoses(hivResults$notifications_min, 
          allYears, projectDecrease, projectYears, projectOption, 
          diagnosesAge = hivResultsAgeMin)
          
        diagnosesFutureMax <- ProjectDiagnoses(hivResults$notifications_max, 
          allYears, projectDecrease, projectYears, projectOption, 
          diagnosesAge = hivResultsAgeMax)
        
        # Set-up age based deaths and migration factors
        
        # Deaths is a bit tricky because the final row is zero
        relAgeDeathFuture <- cbind(relAgeDeath[, 1:(ncol(relAgeDeath)-1)], 
          matrix(rep(relAgeDeath[, (ncol(relAgeDeath)-1)], nprojYears), 
            ncol = nprojYears), relAgeDeath[, ncol(relAgeDeath)])
        colnames(relAgeDeathFuture) <- paste0("y", 
          as.character(c(allYears, projectYears)))
        
        # Population movement is straight forward
        relAgeMigrateFuture <- cbind(relAgeMigrate,
          matrix(rep(relAgeMigrate[,ncol(relAgeMigrate-1)], nprojYears), 
            ncol = nprojYears))
        colnames(relAgeMigrateFuture) <- paste0("y", 
          as.character(c(allYears, projectYears)))
      
        # Calculate overall (non-age) PLDHIV estimates for normalization
        pldhivOverallFuture <- LivingDiagnosed(diagnosesFuture[[1]],
          ProjVec(subsetRates$annunique, nprojYears),
          ProjVec(subsetRates$deathrate, nprojYears),
          ProjVec(subsetRates$mrate, nprojYears),
          ProjVec(subsetRates$propstay, nprojYears)) %>%
          mutate(year = c(allYears, projectYears)) %>%
          select(year, everything()) %>%
          as_tibble()
        
        pldhivOverallMinFuture <- LivingDiagnosed(diagnosesFutureMin[[1]],
          ProjVec(subsetRates$annunique, nprojYears),
          ProjVec(subsetRates$deathrate_upper, nprojYears),
          ProjVec(subsetRates$mrate_upper, nprojYears),
          ProjVec(subsetRates$propstay_lower, nprojYears)) %>%
          mutate(year = c(allYears, projectYears)) %>%
          select(year, everything()) %>%
          as_tibble()

        pldhivOverallMaxFuture <- LivingDiagnosed(diagnosesFutureMax[[1]],
          ProjVec(subsetRates$annunique, nprojYears),
          ProjVec(subsetRates$deathrate_lower, nprojYears),
          ProjVec(subsetRates$mrate_lower, nprojYears),
          ProjVec(subsetRates$propstay_upper, nprojYears)) %>%
          mutate(year = c(allYears, projectYears)) %>%
          select(year, everything()) %>%
          as_tibble()
        
        # Calculate age group estimates 
        pldhivAllFuture <- LivingDiagnosedAge(diagnosesFuture[[2]],
          ProjVec(subsetRates$annunique, nprojYears),
          ProjVec(subsetRates$deathrate, nprojYears),
          ProjVec(hivBase$migrationrate, nprojYears),
          ProjVec(subsetRates$propstay, nprojYears),
          agedeath = relAgeDeathFuture,
          agemigrate = relAgeMigrateFuture,
          normalize = pldhivOverallFuture$pldhiv) 
        
        pldhivAllMinFuture <- LivingDiagnosedAge(diagnosesFutureMin[[2]],
          ProjVec(subsetRates$annunique, nprojYears),
          ProjVec(subsetRates$deathrate_upper, nprojYears),
          ProjVec(hivBase$migrationrate_upper, nprojYears),
          ProjVec(subsetRates$propstay_lower, nprojYears),
          agedeath = relAgeDeathFuture,
          agemigrate = relAgeMigrateFuture,
          normalize = pldhivOverallMinFuture$pldhiv)

        pldhivAllMaxFuture <- LivingDiagnosedAge(diagnosesFutureMax[[2]],
          ProjVec(subsetRates$annunique, nprojYears),
          ProjVec(subsetRates$deathrate_lower, nprojYears),
          ProjVec(hivBase$migrationrate_lower, nprojYears),
          ProjVec(subsetRates$propstay_upper, nprojYears),
          agedeath = relAgeDeathFuture,
          agemigrate = relAgeMigrateFuture,
          normalize = pldhivOverallMaxFuture$pldhiv)
        
      } else {
        
        diagnosesFuture <- ProjectDiagnoses(hivResults$notifications, 
          allYears, projectDecrease, projectYears, projectOption)
        
        diagnosesFutureMin <- ProjectDiagnoses(hivResults$notifications, 
          allYears, projectDecrease, projectYears, projectOption)
        
        diagnosesFutureMax <- ProjectDiagnoses(hivResults$notifications, 
          allYears, projectDecrease, projectYears, projectOption)
        
        pldhivAllFuture <- LivingDiagnosed(diagnosesFuture[[1]],
          ProjVec(subsetRates$annunique, nprojYears),
          ProjVec(subsetRates$deathrate, nprojYears),
          ProjVec(subsetRates$mrate, nprojYears),
          ProjVec(subsetRates$propstay, nprojYears)) %>%
          mutate(year = c(allYears, projectYears)) %>%
          select(year, everything()) %>%
          as_tibble()
        
        pldhivAllMin <- LivingDiagnosed(diagnosesFutureMin[[1]],
          ProjVec(subsetRates$annunique, nprojYears),
          ProjVec(subsetRates$deathrate_upper, nprojYears),
          ProjVec(subsetRates$mrate_upper, nprojYears),
          ProjVec(subsetRates$propstay_lower, nprojYears)) %>%
          mutate(year = c(allYears, projectYears)) %>%
          select(year, everything()) %>%
          as_tibble()

        pldhivAllMax <- LivingDiagnosed(diagnosesFutureMax[[1]],
          ProjVec(subsetRates$annunique, nprojYears),
          ProjVec(subsetRates$deathrate_lower, nprojYears),
          ProjVec(subsetRates$mrate_lower, nprojYears),
          ProjVec(subsetRates$propstay_upper, nprojYears)) %>%
          mutate(year = c(allYears, projectYears)) %>%
          select(year, everything()) %>%
          as_tibble()
      }
    }
    
    # Store projection results
  if (doAge) {
    # Store estimates by year and age
    pldhivDfFuture <- as.data.frame(pldhivAllFuture) %>% 
      rownames_to_column(var = "agebin") %>%
      gather("year", "pldhiv", 2:ncol(.)) %>%
      select(year, everything()) %>%
      mutate(year = as.integer(str_sub(year, 2))) %>%
      as_tibble()
    
    pldhivDfMinFuture <- as.data.frame(pldhivAllMinFuture) %>%
      rownames_to_column(var = "agebin") %>%
      gather("year", "pldhiv", 2:ncol(.)) %>%
      select(year, everything()) %>%
      mutate(year = as.integer(str_sub(year, 2))) %>%
      as_tibble()

    pldhivDfMaxFuture <- as.data.frame(pldhivAllMaxFuture) %>%
      rownames_to_column(var = "agebin") %>%
      gather("year", "pldhiv", 2:ncol(.)) %>%
      select(year, everything()) %>%
      mutate(year = as.integer(str_sub(year, 2))) %>%
      as_tibble()
    
    hivDiagnosedFuture <- data.frame(stage = "pldhiv",
      year = pldhivDfFuture$year,
      agebin = pldhivDfFuture$agebin,
      value = pldhivDfFuture$pldhiv,
      lower = pldhivDfMinFuture$pldhiv,
      upper = pldhivDfMaxFuture$pldhiv) %>%
      as_tibble()
    
  } else {
    # Don't need to store age
    hivDiagnosedFuture <- data.frame(stage = "pldhiv",
      year = c(allYears, projectYears),
      value = pldhivAllFuture$pldhiv,
      lower = pldhivAllMinFuture$pldhiv,
      upper = pldhivAllMaxFuture$pldhiv)
  }
    
    # Save projection results --------------------------------------------
    if (saveResults) {
      # Create output directory
      resultsPath <- file.path(outputFolder, projectOutput, cascadeName)
      dir.create(resultsPath, showWarnings = FALSE, recursive = TRUE)
      
      # Set up files
      if (doAge) {
        saveStringDetails <- file.path(resultsPath, 
          paste0("pldhiv-", toString(analysisYear), "-age-"))
      } else {
        saveStringDetails <- file.path(resultsPath, 
          paste0("pldhiv-", toString(analysisYear), "-"))
      }
      
      # Save main results
      saveStringPldhiv <- file.path(resultsPath, 
        paste0("HIVpldhivEstimates-", toString(analysisYear)))
      if (doAge) {
        saveStringPldhiv <- paste0(saveStringPldhiv, "-age")
      }
      
      # Save results
      if (doAge) {
        write_csv(rownames_to_column(as.data.frame(pldhivAllFuture), 
          var = "agebin"), paste0(saveStringDetails, projectName,
            ".csv")) 
        write_csv(rownames_to_column(as.data.frame(pldhivAllMinFuture), 
          var = "agebin"), paste0(saveStringDetails, projectName,
            "-min.csv"))
        write_csv(rownames_to_column(as.data.frame(pldhivAllMaxFuture), 
          var = "agebin"), paste0(saveStringDetails, projectName,
            "-max.csv"))
        
        write_csv(pldhivOverallFuture, paste0(saveStringDetails, 
          projectName, "_overall.csv"))
        write_csv(pldhivOverallMinFuture, paste0(saveStringDetails, 
          projectName, "_overall-min.csv"))
        write_csv(pldhivOverallMaxFuture, paste0(saveStringDetails, 
          projectName, "_overall-max.csv"))
        
        write_csv(hivDiagnosedFuture, paste0(saveStringPldhiv, "-",
          projectName, ".csv"))
      } else {
        write_csv(pldhivAllFuture, paste0(saveStringDetails, projectName,
          ".csv"))
        write_csv(pldhivAllMinFuture, paste0(saveStringDetails, projectName,
          "-max.csv"))
        write_csv(pldhivAllMaxFuture, paste0(saveStringDetails, projectName,
          "-min.csv"))
      }
    }
  }
  
  ## Return outputs we want ----------------------------------------------
  pldhivCalculations <- list("pldhivAll" = pldhivAll, 
    "pldhivAllMin" = pldhivAllMin, 
    "pldhivAllMax" = pldhivAllMax,
    "hivDiagnosed" = hivDiagnosed, 
    "hivParams" = hivParams,
    "uniqueNotifications" = uniqueNotifications,
    "pldhivAllFuture" = pldhivAllFuture, 
    "pldhivAllMinFuture" = pldhivAllMinFuture,
    "pldhivAllMaxFuture" = pldhivAllMaxFuture,
    "hivDiagnosedFuture" = hivDiagnosedFuture, 
    "hivResults" = hivResults,
    "hivResultsAge" = hivResultsAge)
  
  # Return
  return(pldhivCalculations)
}
