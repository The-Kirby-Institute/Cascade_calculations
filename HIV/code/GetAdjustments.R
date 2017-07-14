## GetAdjustments.R

# Richard T. Gray

# R function to extract subset of interest from notifications

GetAdjustments <- function(hivBase, hivAdjustments, hivInterstate, targetAge, 
                           targetExposure, targetCob, targetAtsi,
                           targetLocalRegion, targetGlobalRegion) {
  
  adjustments <- data.frame(year = hivBase$year)
  
  # Create list of variables
  # subsetVar <- c(targetAge, targetExposure, targetCob, targetAtsi,
  #                targetLocalRegion, targetGlobalRegion)
  
  # Now extract and calculate the variables we want
  
  # Extract migration rate
  # First set up base
  if (targetGender == "male" | targetExposure == "msm") {
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
  if (targetLocalRegion != "all") {
    mrate <- switch(targetLocalRegion,
                    "nsw" = hivAdjustments$mrate_nsw,
                    "vic" = hivAdjustments$mrate_nsw,
                    "qld" = hivAdjustments$mrate_nsw,
                    "nt" = hivAdjustments$mrate_nsw,
                    "wa" = hivAdjustments$mrate_nsw,
                    "sa" = hivAdjustments$mrate_nsw,
                    "tas" = hivAdjustments$mrate_nsw,
                    "act" = hivAdjustments$mrate_act)
    
    adjustments$mrate <- adjustments$mrate * mrate
    adjustments$mrate_lower <- adjustments$mrate_lower * mrate
    adjustments$mrate_upper <- adjustments$mrate_upper * mrate
  }

  # Adjust interstate migration rate for location
  if (targetLocalRegion != "all") {
    arriverate <- switch(targetLocalRegion,
                    "nsw" = filter(hivInterstatestate == "nsw")$arriverate,
                    "vic" = filter(hivInterstatestate == "vic")$arriverate,
                    "qld" = filter(hivInterstatestate == "qld")$arriverate,
                    "nt" = filter(hivInterstatestate == "nt")$arriverate,
                    "wa" = filter(hivInterstatestate == "wa")$arriverate,
                    "sa" = filter(hivInterstatestate == "sa")$arriverate,
                    "tas" = filter(hivInterstatestate == "tas")$arriverate,
                    "act" = filter(hivInterstatestate == "act")$arriverate)
    
    adjustments$inter_arriverate <- arriverate
    
    departrate <- switch(targetLocalRegion,
                         "nsw" = filter(hivInterstatestate == "nsw")$departrate,
                         "vic" = filter(hivInterstatestate == "vic")$departrate,
                         "qld" = filter(hivInterstatestate == "qld")$departrate,
                         "nt" = filter(hivInterstatestate == "nt")$departrate,
                         "wa" = filter(hivInterstatestate == "wa")$departrate,
                         "sa" = filter(hivInterstatestate == "sa")$departrate,
                         "tas" = filter(hivInterstatestate == "tas")$departrate,
                         "act" = filter(hivInterstatestate == "act")$departrate)
    
    adjustments$inter_departrate <- departrate
    
  } else {
    adjustments$inter_arriverate <- 0
    adjustments$inter_departrate <- 0
  }
  
  # Extract deathrate and propstay for indigenous and non-indigenous 
  if (targetAtsi == "all") {
    adjustments$deathrate <- hivBase$deathrate
    adjustments$deathrate_lower <- hivBase$deathrate_lower
    adjustments$deathrate_upper <- hivBase$deathrate_upper
    
    adjustments$propstay <- hivBase$propstay
    adjustments$propstay_lower <- hivBase$propstay_lower
    adjustments$propstay_upper <- hivBase$propstay_upper
    
  } else if (targetAtsi == "non_indigenous") {
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
    adjustments$deathrate <- hivBase$deathrate * 
      hivAdjustments$drate_indigenous
    adjustments$deathrate_lower <- hivBase$deathrate_lower * 
      hivAdjustments$drate_indigenous_lower
    adjustments$deathrate_upper <- hivBase$deathrate_upper * 
      hivAdjustments$drate_indigenous_upper
    
    adjustments$propstay <- hivBase$propstay * 
      hivAdjustments$pstay_indigenous
    adjustments$propstay_lower <- hivBase$propstay_lower * 
      hivAdjustments$pstay_indigenous
    adjustments$propstay_upper <- hivBase$propstay_upper *
      hivAdjustments$pstay_indigenous
  }
  
  # Further adjust propstay by location
  # Now adjust for location only have this for nsw and vic
  if (targetLocalRegion == "nsw") {
    adjustments$propstay <- adjustments$propstay * 
      hivAdjustments$pstay_nsw
    adjustments$propstay_lower <- adjustments$propstay_lower * 
      hivAdjustments$pstay_nsw_lower
    adjustments$propstay_upper <- adjustments$propstay_upper * 
      hivAdjustments$pstay_nsw_upper
  } 
  
  if (targetLocalRegion == "vic") {
    adjustments$propstay <- adjustments$propstay * 
      hivAdjustments$pstay_vic
    adjustments$propstay_lower <- adjustments$propstay_lower * 
      hivAdjustments$pstay_vic_lower
    adjustments$propstay_upper <- adjustments$propstay_upper * 
      hivAdjustments$pstay_vic_upper
  }
  
  return(adjustments)
  
}
