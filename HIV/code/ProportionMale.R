#' Calculate proportion of diagnoses that are male
#' 
#' This function calculates the proportion of annual diagnoses
#' that are male.
#' 
#' @param hivSet Data frame with selected notifications
#' @param analysisYear Year the calculation goes up to
#' @param doAge Logical specifying if proportion males is calculated for 
#' each age group.  
#' @param targetGender string specifying target gender for calculations. 
#' Almost always "all" for this calculation so set to "all" as default and 
#' function returns NULL if it has a different value. 
#' 
#' @return A vector of the proportion of notifications that are male or 
#' NULL. 
#' 
#' @author Richard T Gray, \email{Rgray@kirby.unsw.edu.au}
#' 
#' @export
#' @import tidyverse
#' 
ProportionMale <- function(hivSet, analysisYear, doAge, 
  targetGender = "all") {
  
  hivSetGender <- hivSet %>% 
    group_by(yeardiagnosis, sex) %>% 
    summarise(notifications = n()) %>% 
    ungroup() %>%
    spread(sex, notifications) %>%
    rename(year = yeardiagnosis)
  hivSetGender[is.na(hivSetGender)] <- 0
  
  # Fill in missing years with zeros
  hivSetGender <- FillDataFrame(1980:analysisYear, hivSetGender) %>%
    gather("sex", "notifications", 2:ncol(.)) %>%
    arrange(year)
  
  if (doAge) {
    
    ageList <- c("a0_4", "a5_9","a10_14", "a15_19", "a20_24", "a25_29", 
      "a30_34", "a35_39", "a40_44", "a45_49", "a50_54", "a55_59", 
      "a60_64", "a65_69", "a70_74", "a75_79", "a80_84", "a85+")
    
    # Calculation cumulative proportion male for each age group.
    if (targetGender == "all") {
      hivSetGenderAge <- hivSet %>%
        group_by(yeardiagnosis, sex, agebin) %>%
        summarise(notifications = n()) %>%
        ungroup() %>%
        spread(sex, notifications)
      hivSetGenderAge[is.na(hivSetGenderAge)] <- 0
      hivSetGenderAge <- hivSetGenderAge %>%
        mutate(other = female+transgender+unknown) %>%
        rename(year = yeardiagnosis) %>%
        select(year, agebin, male, other)
      hivSetGenderAge[is.na(hivSetGenderAge)] <- 0
      
      # Fill in missing years with zeros - have to do males and others and
      # then rebind seperately
      hivSetMaleAge <- hivSetGenderAge %>%
        select(-other) %>%
        spread(agebin, male)
      hivSetMaleAge[is.na(hivSetMaleAge)] <- 0
      hivSetMaleAge <- FillDataFrame(1980:analysisYear, hivSetMaleAge) %>%
        FillMissingAge(ageList, .) %>%
        select(c("year", ageList)) %>%
        gather("agebin", "male", 2:ncol(.)) %>%
        arrange(year)
      
      hivSetOtherAge <- hivSetGenderAge %>%
        select(-male) %>%
        spread(agebin, other)
      hivSetOtherAge[is.na(hivSetOtherAge)] <- 0
      hivSetOtherAge <- FillDataFrame(1980:analysisYear, hivSetOtherAge) %>%
        FillMissingAge(ageList, .) %>%
        select(c("year", ageList)) %>%
        gather("agebin", "other", 2:ncol(.)) %>%
        arrange(year)
      
      hivGenderAgeCum <- hivSetMaleAge %>%
        mutate(other = hivSetOtherAge$other) %>%
        group_by(agebin) %>%
        mutate(cummale = cumsum(male),
          cumother = cumsum(other)) %>%
        ungroup() %>%
        select(-male, -other) %>%
        group_by(year, agebin) %>%
        mutate(propmale = cummale / (cummale + cumother)) %>%
        mutate(propmale = ifelse(is.nan(propmale), 1, propmale)) %>%
        select(-cummale, -cumother) %>%
        spread(agebin, propmale)
      
      propDiagsMale <- as.matrix(hivGenderAgeCum)
      
    } else {
      propDiagsMale <- NULL
    } 
  } else {
    # Not doing age calculations
    if (targetGender == "all") {
      
      if ("transgender" %in% unique(hivSetGender$sex)) {
        hivGenderCum <- hivSetGender %>% 
          group_by(sex) %>% 
          mutate(cumnotifications = cumsum(notifications)) %>%
          ungroup() %>%
          select(-notifications) %>%
          spread(sex, cumnotifications) %>%
          select(-starts_with("unknown")) %>%
          gather("sex", "cumnotifications", 2:ncol(.)) %>%
          mutate(cumnotifications = ifelse(is.na(cumnotifications), 0, 
            cumnotifications)) %>%
          group_by(year) %>% 
          mutate(propnotifications = cumnotifications / sum(cumnotifications)) %>%
          select(-cumnotifications) %>%
          spread(sex, propnotifications) %>%
          mutate(female = ifelse(is.na(female), 0, female),
            male = ifelse(is.na(male), 0, male),
            transgender = ifelse(is.na(transgender), 0, transgender)) 
      } else {
        hivGenderCum <- hivSetGender %>% 
          group_by(sex) %>% 
          mutate(cumnotifications = cumsum(notifications)) %>%
          ungroup() %>%
          select(-notifications) %>%
          spread(sex, cumnotifications) %>%
          select(-starts_with("unknown")) %>%
          gather("sex", "cumnotifications", 2:ncol(.)) %>%
          mutate(cumnotifications = ifelse(is.na(cumnotifications), 0, 
            cumnotifications)) %>%
          group_by(year) %>% 
          mutate(propnotifications = cumnotifications / sum(cumnotifications)) %>%
          select(-cumnotifications) %>%
          spread(sex, propnotifications) %>%
          mutate(female = ifelse(is.na(female), 0, female),
            male = ifelse(is.na(male), 0, male))
      }
      propDiagsMale <- hivGenderCum$male
    } else {
      propDiagsMale <- NULL
    } 
  }
  
  # Return final value
  return(propDiagsMale)
} 
