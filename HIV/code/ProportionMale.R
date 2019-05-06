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
  
  if (doAge) {
    
    ageList <- c("a0_4", "a5_9","a10_14", "a15_19", "a20_24", "a25_29", 
      "a30_34", "a35_39", "a40_44", "a45_49", "a50_54", "a55_59", 
      "a60_64", "a65_69", "a70_74", "a75_79", "a80_84", "a85+")
    
    # Calculation cumulative proportion male for each age group.
    if (targetGender == "all" && nrow(hivSet) != 0) {
      hivSetGenderAge <- hivSet %>%
        group_by(yeardiagnosis, sex, agebin) %>%
        summarise(notifications = n()) %>%
        ungroup() %>%
        spread(sex, notifications) 
      
      # Make sure at least male and female columns exist
      if (!("male" %in% colnames(hivSetGenderAge))) {
        hivSetGenderAge <- hivSetGenderAge %>%
          mutate(male = 0)
      }
      
      if (!("female" %in% colnames(hivSetGenderAge))) {
        hivSetGenderAge <- hivSetGenderAge %>%
          mutate(female = 0)
      }
      
      hivSetGenderAge <- hivSetGenderAge %>%
        mutate(other = Reduce("+", 
          select(., -yeardiagnosis, -agebin, -male))) %>%
        rename(year = yeardiagnosis) %>%
        select(year, agebin, male, other)
      hivSetGenderAge[is.na(hivSetGenderAge)] <- 0
      
      # hivSetGenderAge <- hivSetGenderAge %>%
      #     mutate(other = Reduce("+", 
      #       select(., -yeardiagnosis, -agebin, -male))) %>%
      #     rename(year = yeardiagnosis) %>%
      #     select(year, agebin, male, other)
      # 
      # if ("unknown" %in% names(hivSetGenderAge)) {
      #   hivSetGenderAge <- hivSetGenderAge %>%
      #     mutate(other = female+transgender+unknown) %>%
      #     rename(year = yeardiagnosis) %>%
      #     select(year, agebin, male, other)
      # } else {
      #   hivSetGenderAge <- hivSetGenderAge %>%
      #     mutate(other = Reduce("+", select(., -yeardiagnosis, -agebin, -male))) %>%
      #     rename(year = yeardiagnosis) %>%
      #     select(year, agebin, male, other)
      # }
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
      propDiagsMale <- matrix(1, nrow = length(1980:analysisYear),
        ncol = length(ageList)) 
      colnames(propDiagsMale) <- ageList
      propDiagsMale <- as_tibble(propDiagsMale) %>%
        mutate(year = 1980:analysisYear) %>%
        select(year, everything()) %>%
        as.matrix()
    } 
  } else {
    # Not doing age calculations
    
    if (targetGender == "all" && nrow(hivSet) != 0) {
      hivSetGender <- hivSet %>% 
        group_by(yeardiagnosis, sex) %>% 
        summarise(notifications = n()) %>% 
        ungroup() %>%
        spread(sex, notifications) 
      
      # Make sure at least male and female columns exist
      if (!("male" %in% colnames(hivSetGender))) {
        hivSetGender <- hivSetGender %>%
          mutate(male = 0)
      }
      
      if (!("female" %in% colnames(hivSetGender))) {
        hivSetGender <- hivSetGender %>%
          mutate(female = 0)
      }
      
      hivSetGender <- hivSetGender %>%
        mutate(other = Reduce("+", 
          select(., -yeardiagnosis, -male))) %>%
        rename(year = yeardiagnosis) %>%
        select(year, male, other)
      
      hivSetGender[is.na(hivSetGender)] <- 0
      
      # Fill in missing years with zeros
      hivSetGender <- FillDataFrame(1980:analysisYear, hivSetGender) %>%
        arrange(year)
      
      # Now calculate proportion male
      hivGenderCum <- hivSetGender %>%
        group_by(year) %>%
        mutate(cummale = cumsum(male),
          cumother = cumsum(other)) %>%
        ungroup() %>%
        select(-male, -other) %>%
        group_by(year) %>%
        mutate(propmale = cummale / (cummale + cumother)) %>%
        mutate(propmale = ifelse(is.nan(propmale), 1, propmale)) %>%
        select(-cummale, -cumother) 
      
      propDiagsMale <- hivGenderCum$propmale
      
    } else {
      propDiagsMale <- rep(1, length(1980:analysisYear))
    } 
  }
  
  # Return final value
  return(propDiagsMale)
} 
