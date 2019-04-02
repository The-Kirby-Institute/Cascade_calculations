## R function to extract subset of interest from imputed notifications

# R. T. Gray

SubHivSetImpute <- function(hivdataframe, fAge, fGender, fExposure, fCob, 
  fAtsi, fState, fLocalRegion, fGlobalRegion){
  # Extract the notifications data based on the input variables from the
  # imputed notifications data set which means there are no unknowns
  # 
  # Args:
  #   hivdataframe: Data frame of cleaned notifications data
  #   fAge: age group at diagnosis: all (includes NA) or groups a0_4, a5_9,
  #     a10_14,...,a85+
  #   fGender: sex of notifications: all (includes NA), male, female
  #   fExposure: exposure category #(includes NA), msm, hetero, pwid, 
  #     otherexp
  #   fCob: country of birth of notifications: all (includes NA), 
  #     non-australia (group), non-aus-nz (group), Australia, New Zealand, 
  #     Thailand, etc. Country names begin with capitals.
  #   fAtsi: Indigenous status of notifications: all (includes NA) 
  #     indigenous or non_indigenous (includes NA). Only used if country of
  #     birth is Australia
  #   fState: jurisdiction of residence at diagnosis: all (includes NA), 
  #     nsw, sa, nt, qld, vic, wa, act, tas 
  #   fGlobalRegion: WHO global region of birth of notifications: 
  #     all (includes NA), South-East Asia, Sub-Saharan Africa, Oceania, 
  #     South American, Other cob, etc
  # Returns:
  #   A list consisting of the following data frames:
  #     includeframe: Data frame of notifications which match all the input 
  #       criteria
  #     excludeframe: Data frame of notifications which do not match one of
  #      the input criteria
  #     
  # -----------------------------------------------------------------------
  
  # Initialize data frames for storing results
  # subframe <- hivdataframe
  includeframe <- hivdataframe
  excludeframe <- data_frame()
  
  # Start collating the notifications 
  if (fAge[1] != 'all') {
    # Exclude ones we don't want and keep ones we want
    excludeframe <- bind_rows(excludeframe, 
      filter(includeframe, !(agebin %in% fAge)))
    includeframe <- filter(includeframe, agebin %in% fAge)
  }
  
  if (fGender[1] != 'all') {
    # Exclude ones we don't want and keep ones we want
    excludeframe <- bind_rows(excludeframe, 
      filter(includeframe, !(sex %in% fGender)))
    includeframe <- filter(includeframe, sex %in% fGender) 
  }
  
  if (fExposure[1] != 'all') {
    # Exclude ones we don't want and keep ones we want
    excludeframe <- bind_rows(excludeframe, 
      filter(includeframe, !(expgroup %in% fExposure)))
    includeframe <- filter(includeframe, expgroup %in% fExposure) 
  }
  
  if (fCob[1] != 'all') {
    # Exclude ones we don't want and keep ones we want
    if (fCob[1] == 'non-australia') {
      # Special case - not born in Australia/born overseas
      excludeframe <- bind_rows(excludeframe,
        filter(includeframe, cob == 'Australia'))
      includeframe <- filter(includeframe, cob != 'Australia')
    } else if (fCob[1] =='non-aus-nz') {
      # Special case - not born in Australia or NZ
      excludeframe <- bind_rows(excludeframe, filter(includeframe, 
        cob %in% c('Australia', 'New Zealand')))
      includeframe <- filter(includeframe, 
        !(cob %in% c('Australia', 'New Zealand')))
    } else {
      excludeframe <- bind_rows(excludeframe,
        filter(includeframe, !(cob %in% fCob)))
      includeframe <- filter(includeframe, cob %in% fCob)
    }
  }
  
  if(fAtsi[1] != 'all' && fCob[1] == "Australia" && length(fCob) == 1){
    # Exclude ones we don't want and keep ones we want
    excludeframe <- bind_rows(excludeframe, 
      filter(includeframe, !(aboriggroup %in% fAtsi)))
    includeframe <- filter(includeframe, aboriggroup %in% fAtsi)
  }
  
  if(fState[1] != 'all'){
    # Exclude ones we don't want and keep ones we want
    excludeframe <- bind_rows(excludeframe, 
      filter(includeframe, !(state %in% fState)))
    includeframe <- filter(includeframe, state %in% fState)
  }
  
  if(fLocalRegion[1] != 'all'){
    # Exclude ones we don't want and keep ones we want
    excludeframe <- bind_rows(excludeframe, 
      filter(includeframe, !(diag_region %in% fLocalRegion)))
    includeframe <- filter(includeframe, diag_region %in% fLocalRegion)
  }
  
  if(fGlobalRegion[1] != "all"){
    # Exclude ones we don't want and keep ones we want - note a number 
    # of special cases 
    if(fGlobalRegion == "Other cob"){
      # Category for ASR for everyone not Australian, South-East Asia,
      # or Sub-Saharan Africa born 
      
      excludeframe <- bind_rows(excludeframe, 
        filter(includeframe, globalregion %in% c("South-East Asia", 
          "Sub-Saharan Africa")))
      excludeframe <- bind_rows(excludeframe,
        filter(includeframe, cob == "Australia"))  
      
      includeframe <- filter(includeframe, 
        !(cob %in% c("Australia", "Overseas", "Not Reported")))    
      includeframe <- filter(includeframe, 
        !(globalregion %in% c("South-East Asia", "Sub-Saharan Africa")))
      
    }else{
      excludeframe <- bind_rows(excludeframe, filter(includeframe, 
        !(globalregion %in% fGlobalRegion)))
      includeframe <- filter(includeframe, globalregion %in% fGlobalRegion)
    }
  }
  
  # Return resulting dataframes
  return(list(includeframe, excludeframe))
}
