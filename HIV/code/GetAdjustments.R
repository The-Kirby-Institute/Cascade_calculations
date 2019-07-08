## GetAdjustments.R

# Richard T. Gray

# R function to extract subset of interest from adjustments

GetAdjustments <- function(hivBase, hivAdjustments,
  targetAge, targetGender,targetExposure, targetCob, targetAtsi,
  targetLocalRegion, targetState, targetGlobalRegion) {
  
  adjustments <- data.frame(year = hivBase$year)
  
  # Create list of variables
  # subsetVar <- c(targetAge, targetExposure, targetCob, targetAtsi,
  #                targetLocalRegion, targetGlobalRegion)
  
  # Now extract and calculate the variables we want
  
  # Unique notifications------------------------------------------------
  
  # Adjusted only for sex (msm included) and indigenous status for the 
  # national cascade. Any combination of exposure assumed to have base 
  # proportion. For other sub-populations need to run this calculation
  # separately. 
  
  # Extract proportion notifications unique - if more than 
  # one target population drop to the base estimates
  if (targetGender == "male" || 
      (length(targetExposure) == 1 && targetExposure[1] == "msm")) {
    # Use male proportion 
    adjustments$cumunique <-  hivBase$cumunique_male
    adjustments$annunique <-  hivBase$annunique_male
  } else if (targetGender == "female") {
    # Use female proportion
    adjustments$cumunique <-  hivBase$cumunique_female
    adjustments$annunique <-  hivBase$annunique_female
  } else if (targetAtsi == "indigenous") {
    # Assume no duplicates for Australian born indigenous 
    adjustments$cumunique <-  1
    adjustments$annunique <-  1
  } else {
    # Any other combination used base
    adjustments$cumunique <-  hivBase$cumunique_all
    adjustments$annunique <-  hivBase$annunique_all
  }
  
  # Death rates -----------------------------------------------------------
  
  # Adjusted only for indigenous status, and country of birth. 
  # Those born outside Australia assumed to be non-indigenous
  
  # Extract deathrate and propstay for indigenous and non-indigenous 
  if (targetAtsi == "indigenous" && length(targetCob) == 1 && 
      targetCob[1] == "Australia") {
    # Australian born indigenous
    adjustments$deathrate <- hivBase$deathrate * 
      hivAdjustments$drate_indigenous
    adjustments$deathrate_lower <- hivBase$deathrate_lower * 
      hivAdjustments$drate_indigenous_lower
    adjustments$deathrate_upper <- hivBase$deathrate_upper * 
      hivAdjustments$drate_indigenous_upper
    
    adjustments$propstay <- hivAdjustments$pstay_indigenous
    adjustments$propstay_lower <- hivAdjustments$pstay_indigenous
    adjustments$propstay_upper <- hivAdjustments$pstay_indigenous
  } else if ((targetAtsi == "non_indigenous" &&  
      targetCob[1] == "Australia")|| targetGlobalRegion[1] != "all" || 
      (targetCob[1] != "all" && targetCob[1] != "Australia")) {
    # Australian born non-indiegnous or those born overseas
    adjustments$deathrate <- hivBase$deathrate * 
      hivAdjustments$drate_non_indigenous
    adjustments$deathrate_lower <- hivBase$deathrate_lower * 
      hivAdjustments$drate_non_indigenous_lower
    adjustments$deathrate_upper <- hivBase$deathrate_upper * 
      hivAdjustments$drate_non_indigenous_upper
    
    adjustments$propstay <- hivBase$propstay * 
      hivAdjustments$pstay_non_indigenous
    adjustments$propstay_lower <- hivBase$propstay_lower * 
      hivAdjustments$pstay_non_indigenous
    adjustments$propstay_upper <- hivBase$propstay_upper *
      hivAdjustments$pstay_non_indigenous
    
  } else {
    # Any other combination
    adjustments$deathrate <- hivBase$deathrate
    adjustments$deathrate_lower <- hivBase$deathrate_lower
    adjustments$deathrate_upper <- hivBase$deathrate_upper
    
    adjustments$propstay <- hivBase$propstay
    adjustments$propstay_lower <- hivBase$propstay_lower
    adjustments$propstay_upper <- hivBase$propstay_upper
  }
  
  # Adjust deathrates for males and females
  if (targetGender == "male" || 
      (length(targetExposure) == 1 && targetExposure[1] == "msm")) {
    adjustments$deathrate <- hivBase$deathrate * 
      hivAdjustments$drate_male
    adjustments$deathrate_lower <- hivBase$deathrate_lower * 
      hivAdjustments$drate_male
    adjustments$deathrate_upper <- hivBase$deathrate_upper * 
      hivAdjustments$drate_male
  } 
  
  if (targetGender == "female") {
    adjustments$deathrate <- hivBase$deathrate * 
      hivAdjustments$drate_female
    adjustments$deathrate_lower <- hivBase$deathrate_lower * 
      hivAdjustments$drate_female
    adjustments$deathrate_upper <- hivBase$deathrate_upper * 
      hivAdjustments$drate_female
  }
  
  # TODO: Fix below to handle combinations of states
  # Treat as a separate function using the migration ABS data
  
  # Migration rates -------------------------------------------------------
  
  # Extract migration rate
  # First set up base
  if (targetGender == "male" ||
      (length(targetExposure) == 1 && targetExposure[1] == "msm")) {
    adjustments$mrate <- hivBase$migrationrate *
      hivAdjustments$mrate_male_adults
    adjustments$mrate_lower <- hivBase$migrationrate_lower *
      hivAdjustments$mrate_male_adults
    adjustments$mrate_upper <- hivBase$migrationrate_upper *
      hivAdjustments$mrate_male_adults
  } else if (targetGender == "female") {
    adjustments$mrate <- hivBase$migrationrate *
      hivAdjustments$mrate_female_adults
    adjustments$mrate_lower <- hivBase$migrationrate_lower *
      hivAdjustments$mrate_female_adults
    adjustments$mrate_upper <- hivBase$migrationrate_upper *
      hivAdjustments$mrate_female_adults
  } else {
    # all
    adjustments$mrate <- hivBase$migrationrate *
      hivAdjustments$mrate_all_adults
    adjustments$mrate_lower <- hivBase$migrationrate_lower *
      hivAdjustments$mrate_all_adults
    adjustments$mrate_upper <- hivBase$migrationrate_upper *
      hivAdjustments$mrate_all_adults
  }

  # Adjust  migration rate for location
  if (targetState[1] != "all" && length(targetState) == 1) {
    mrate <- switch(targetState[1],
      "nsw" = hivAdjustments$mrate_nsw,
      "vic" = hivAdjustments$mrate_vic,
      "qld" = hivAdjustments$mrate_qld,
      "nt" = hivAdjustments$mrate_nt,
      "wa" = hivAdjustments$mrate_wa,
      "sa" = hivAdjustments$mrate_sa,
      "tas" = hivAdjustments$mrate_tas,
      "act" = hivAdjustments$mrate_act)

    adjustments$mrate <- adjustments$mrate * mrate
    adjustments$mrate_lower <- adjustments$mrate_lower * mrate
    adjustments$mrate_upper <- adjustments$mrate_upper * mrate
  }

  # Adjust migration rate for Indigenous population
  if (targetAtsi == "indigenous" && length(targetCob) == 1 &&
      targetCob[1] == "Australia") {
    adjustments$mrate <- 0
    adjustments$mrate_lower <- 0
    adjustments$mrate_upper <- 0
  } else if ((targetAtsi == "non_indigenous" &&
      targetCob[1] == "Australia")|| targetGlobalRegion[1] != "all" ||
      (targetCob[1] != "all" && targetCob[1] != "Australia")) {
    adjustments$mrate <- adjustments$mrate *
      hivAdjustments$non_aborig_migration
    adjustments$mrate_lower <- adjustments$mrate_lower *
      hivAdjustments$non_aborig_migration
    adjustments$mrate_upper <- adjustments$mrate_upper *
      hivAdjustments$non_aborig_migration
  }
  
  # adjustments$mrate <- hivBase$migrationrate 
  # adjustments$mrate_lower <- hivBase$migrationrate_lower 
  # adjustments$mrate_upper <- hivBase$migrationrate_upper 
  
  # Interstate migration rate ---------------------------------------------
  # Adjust interstate migration rate for location
  # TODO: create a function to do this adjustment for 
  # combinations of states
  # if (targetState != "all" && length(targetState) == 1) {
  #   arriverate <- switch(targetState[1],
  #     "nsw" = filter(hivInterstate, 
  #       state == "nsw")$arriverate,
  #     "vic" = filter(hivInterstate, 
  #       state == "vic")$arriverate,
  #     "qld" = filter(hivInterstate, 
  #       state == "qld")$arriverate,
  #     "nt" = filter(hivInterstate, 
  #       state == "nt")$arriverate,
  #     "wa" = filter(hivInterstate, 
  #       state == "wa")$arriverate,
  #     "sa" = filter(hivInterstate, 
  #       state == "sa")$arriverate,
  #     "tas" = filter(hivInterstate, 
  #       state == "tas")$arriverate,
  #     "act" = filter(hivInterstate, 
  #       state == "act")$arriverate)
  #   
  #   adjustments$inter_arriverate <- arriverate
  #   
  #   departrate <- switch(targetState[1],
  #     "nsw" = filter(hivInterstate, 
  #       state == "nsw")$departrate,
  #     "vic" = filter(hivInterstate, 
  #       state == "vic")$departrate,
  #     "qld" = filter(hivInterstate, 
  #       state == "qld")$departrate,
  #     "nt" = filter(hivInterstate, 
  #       state == "nt")$departrate,
  #     "wa" = filter(hivInterstate, 
  #       state == "wa")$departrate,
  #     "sa" = filter(hivInterstate, 
  #       state == "sa")$departrate,
  #     "tas" = filter(hivInterstate, 
  #       state == "tas")$departrate,
  #     "act" = filter(hivInterstate, 
  #       state == "act")$departrate)
  #   
  #   adjustments$inter_departrate <- departrate
  #   
  # } else {
  #   adjustments$inter_arriverate <- 0
  #   adjustments$inter_departrate <- 0
  # }
  

  # Further adjust propstay and deathrate by location only have this for 
  # NSW and Victoria. For other states assume no post-diagnosis movement
  # and the national deathrate
  if (targetState[1] != "all" && length(targetState) == 1) {
    
    if (targetState[1] == "nsw") {
      
      # Adjust propstay in NSW by country of birth
      if (targetCob[1] != "all" && length(targetCob) == 1) {
        if (targetCob[1] %in% c("Australia", "New Zealand")  && 
            length(targetState) == 1) {
          adjustments$propstay <- adjustments$propstay * 
            hivAdjustments$pstay_aus
          adjustments$propstay_lower <- adjustments$propstay_lower * 
            hivAdjustments$pstay_aus_lower
          adjustments$propstay_upper <- adjustments$propstay_upper * 
            hivAdjustments$pstay_aus_upper
        } else {
          adjustments$propstay <- adjustments$propstay * 
            hivAdjustments$pstay_os
          adjustments$propstay_lower <- adjustments$propstay_lower * 
            hivAdjustments$pstay_os_lower
          adjustments$propstay_upper <- adjustments$propstay_upper * 
            hivAdjustments$pstay_os_upper
        }
      }
      
      adjustments$propstay <- adjustments$propstay * 
        hivAdjustments$pstay_nsw
      adjustments$propstay_lower <- adjustments$propstay_lower * 
        hivAdjustments$pstay_nsw_lower
      adjustments$propstay_upper <- adjustments$propstay_upper * 
        hivAdjustments$pstay_nsw_upper
      
      adjustments$deathrate <- adjustments$deathrate * 
        hivAdjustments$drate_nsw
      adjustments$deathrate_lower <- adjustments$deathrate_lower * 
        hivAdjustments$drate_nsw_lower
      adjustments$deathrate_upper <- adjustments$deathrate_upper * 
        hivAdjustments$drate_nsw_upper
      
    } else if (targetState[1] == "vic") {
      adjustments$propstay <- adjustments$propstay *
        hivAdjustments$pstay_vic
      adjustments$propstay_lower <- adjustments$propstay_lower *
        hivAdjustments$pstay_vic_lower
      adjustments$propstay_upper <- adjustments$propstay_upper *
        hivAdjustments$pstay_vic_upper
      
      adjustments$deathrate <- adjustments$deathrate *
        hivAdjustments$drate_vic
      adjustments$deathrate_lower <- adjustments$deathrate_lower *
        hivAdjustments$drate_vic_lower
      adjustments$deathrate_upper <- adjustments$deathrate_upper *
        hivAdjustments$drate_vic_upper
    } else {
      # Same as Vic because assuming no movement outside NSW
      adjustments$propstay <- adjustments$propstay *
        hivAdjustments$pstay_vic
      adjustments$propstay_lower <- adjustments$propstay_lower *
        hivAdjustments$pstay_vic_lower
      adjustments$propstay_upper <- adjustments$propstay_upper *
        hivAdjustments$pstay_vic_upper
    }
  } 
  
  # if (targetAtsi == "indigenous") {
  #   adjustments$propstay <- 1
  #   adjustments$propstay_lower <- 1
  #   adjustments$propstay_upper <- 1
  # }
  
  return(adjustments)
  
}
