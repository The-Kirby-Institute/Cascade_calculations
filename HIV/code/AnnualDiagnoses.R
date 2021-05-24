#' Calculate the annual number of notifications/diagnoses
#' 
#' This function calculates the number of annual notifications each year for
#' a specific set of criteria. Adjusting to account for missing 
#' information. 
#' 
#' @param hivSet Set of notifications known to meet the criteria and are 
#' included in the analysis. 
#' @param hivSetExcluded Set of notifications known to not meet the 
#' criteria and are excluded from the analysis. 
#' @param hivSetUnknown 
#' @param allYears Vector specifying the years for calculations. 
#' @param doAge Logical specifying if proportion males is calculated for 
#' each age group.
#' 
#' @return A list containing up to two data frames. 
#' 
#' @author Richard T Gray, \email{Rgray@kirby.unsw.edu.au}
#' 
#' @export
#' @import tidyverse
#'  
AnnualDiagnoses <- function(hivSet, hivSetExcluded, hivSetUnknown, 
  allYears, doAge) {
  
  # analysisYear <- tail(allYears, 1)
  if (nrow(hivSet) == 0) {
    includeDiags <- tibble(year = allYears,
      included = 0)
  } else {
    includeDiags <- hivSet %>% 
      group_by(yeardiagnosis) %>%
      summarise(included = n()) %>%
      ungroup() %>%
      rename(year = yeardiagnosis)
    includeDiags <- FillDataFrame(allYears, includeDiags)
  }
  
  # If excluded and unknown is empty replace with zeros
  if (nrow(hivSetExcluded) == 0) {
    excludedDiags <- tibble(year = allYears,
      excluded = 0)
  } else {
    excludedDiags <- hivSetExcluded %>%
      group_by(yeardiagnosis) %>% 
      summarise(excluded = n()) %>%
      rename(year = yeardiagnosis)
    
  }
  excludedDiags <- FillDataFrame(allYears, excludedDiags)
  
  if (nrow(hivSetUnknown) == 0) {
    unknownDiags <- tibble(year = allYears,
      unknown = 0)
  } else {
    unknownDiags <- hivSetUnknown %>%
      group_by(yeardiagnosis) %>% 
      summarise(unknown = n()) %>%
      rename(year = yeardiagnosis)
  }
  unknownDiags <- FillDataFrame(allYears, unknownDiags)
  
  # Adjust included and excluded annual diagnoses propotionally to replace
  # unknowns
  
  ##*********************************************************************##
  ## WARNING -- The following calculation to adjust included notifications 
  ## can be incorrect if multiple selections are used which include 
  ## selections with lots of missing data unless appropriate adjustments are
  ## made. 
  ##*********************************************************************##
  
  adjustDiags <- includeDiags %>% 
    left_join(excludedDiags, by = "year") %>%
    left_join(unknownDiags, by = "year") %>%
    mutate(all = included + excluded + unknown) %>%
    mutate(included = ifelse(is.na(included), 0, included),
      excluded = ifelse(is.na(excluded), 0, excluded),
      unknown = ifelse(is.na(unknown), 0, unknown)) %>%
    mutate(prop_included = included / (included + excluded),
      prop_excluded = excluded / (included + excluded)) %>%
    mutate(prop_included = ifelse(is.nan(prop_included), 0, prop_included), 
      prop_excluded = ifelse(is.nan(prop_excluded), 0, 
        prop_excluded)) %>%
    mutate(adjusted_included = included  + unknown * prop_included,
      adjusted_excluded = excluded + unknown * prop_excluded)
  
  hivResults <- tibble(year = adjustDiags$year,
    notifications = adjustDiags$adjusted_included,
    cumnotifications = cumsum(adjustDiags$adjusted_included),
    all_notifications = adjustDiags$all,
    all_cumnotifications = cumsum(adjustDiags$all),
    ecdc_normalization = adjustDiags$included / 
      adjustDiags$adjusted_included) %>%
    mutate(ecdc_normalization = ifelse(is.nan(ecdc_normalization), 1, 
      ecdc_normalization))
  
  # Proportion in each age group--------------------------------------------
  # For the known diagnoses calculate proportion by age bin, exposure group,
  # cd4 count category and diagnosis type. This will be used to distribute 
  # annual notifications for aging PLDHIV.  
  
  hivResultsAge <- NULL #initialize
  if (doAge) {
    # Age and year specs
    nyears <- tail(allYears, 1) - allYears[1] + 1
    nages <- 18
    
    ageList <- c("a00_04", "a05_09","a10_14", "a15_19", "a20_24", "a25_29", 
      "a30_34", "a35_39", "a40_44", "a45_49", "a50_54", "a55_59", 
      "a60_64", "a65_69", "a70_74", "a75_79", "a80_84", "a85+")
    
    yearList <- paste0("y", as.character(allYears))
    
    if (nrow(hivSet) == 0) {
      hivResultsAge <- matrix(0, ncol = length(yearList), 
        nrow = length(ageList))
      
      colnames(hivResultsAge) <- yearList
      rownames(hivResultsAge) <- ageList
    } else {
      
      agedDiags <- hivSet %>%
        group_by(yeardiagnosis, agebin) %>%
        summarise(diags = n()) %>%
        ungroup() %>%
        spread(agebin, diags) %>%
        rename(year = yeardiagnosis)
      agedDiags[is.na(agedDiags)] <- 0
      
      
      
      # Fill missing years and ages
      agedDiags <- FillDataFrame(allYears, agedDiags) %>%
        FillMissingAge(ageList, .) %>%
        select(c("year", ageList)) %>%
        gather("agebin", "diagnoses", 2:ncol(.)) %>%
        arrange(year) %>%
        filter(agebin != "not_reported") %>%
        group_by(year) %>%
        mutate(propdiags = diagnoses / sum(diagnoses)) %>%
        mutate(propdiags = ifelse(is.nan(propdiags), 0 , propdiags)) %>%
        ungroup()
      
      # Adjust_included notifications by age and year and convert to matrix
      hivResultsAge <- agedDiags %>% 
        left_join(., hivResults, by = "year") %>% 
        select(-diagnoses, -cumnotifications, -all_notifications,
          -all_cumnotifications, -ecdc_normalization) %>%
        group_by(year, agebin) %>%
        mutate(diags = propdiags * notifications) %>%
        select(-propdiags, -notifications) %>%
        spread(year, diags) %>%
        ungroup() %>%
        select(-agebin) %>%
        as.matrix()
      
      colnames(hivResultsAge) <- yearList
      rownames(hivResultsAge) <- ageList
    }
  }
  # Return list of hiv results
  return(list(hivResults, hivResultsAge))
  
}

