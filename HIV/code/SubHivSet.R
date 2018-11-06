## R function to extract subset of interest from notifications

# N.A. Bretana and R. T. Gray

SubHivSet <- function(hivdataframe, fAge, fGender, fExposure, fCob, fAtsi, 
                      fState, fGlobalRegion){
  # Extract the notifications data based on the input variables 
  # Need to add local target region (LHD SLA etc)
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
  #     unknownframe: Data frame of notifications which have a missing or 
  #       unknown value for one of the input criteria
  #     
  # -----------------------------------------------------------------------

  ##*********************************************************************##
  ## WARNING -- This function doesn't work as required for selecting a 
  ## combination of multiple input categories for notififcations when a 
  ## category with lots of missing data is included. Most likely need to 
  ## perform full imputation for multiple selections. 
  ##*********************************************************************## 
  
  # Initialize data frames for storing results
  # subframe <- hivdataframe
  includeframe <- hivdataframe
  unknownframe <- data_frame()
  excludeframe <- data_frame()
  
 
  # Start collating the notifications 
  if (fAge[1] != 'all') {
    # Store unkowns
    unknownframe <- distinct(bind_rows(unknownframe, filter(includeframe,
      agebin == 'not_reported')), .keep_all = TRUE)
    unknownframe <- distinct(bind_rows(unknownframe, filter(includeframe,
      is.na(agebin))), .keep_all = TRUE)

    # Remove missing so not double countered in excluded and included -
    # need so excluded so excluded doesn't pick up "not_reported"
    includeframe <- filter(includeframe, agebin != 'not_reported') 
    includeframe <- filter(includeframe, !is.na(agebin)) 
    
    # Exclude ones we don't want and keep ones we want
    excludeframe <- distinct(bind_rows(excludeframe,
      filter(includeframe, !(agebin %in% fAge))), .keep_all = TRUE)
    includeframe <- filter(includeframe, agebin %in% fAge) 
  }
  
  if (fGender[1] != 'all') {
    # Store unkowns
    unknownframe <- distinct(bind_rows(unknownframe, filter(includeframe, 
      sex == 'unknown')), .keep_all = TRUE)
    unknownframe <- distinct(bind_rows(unknownframe, filter(includeframe, 
      is.na(sex))), .keep_all = TRUE)
    
    # Remove missing so not double countered in excluded and included -
    # need so excluded doesn't pick up "unknown"
    includeframe <- filter(includeframe, sex!='unknown') 
    includeframe <- filter(includeframe, !is.na(sex))
    
    # Exclude ones we don't want and keep ones we want
    excludeframe <- distinct(bind_rows(excludeframe, 
      filter(includeframe, !(sex %in% fGender))), .keep_all = TRUE)
    includeframe <- filter(includeframe, sex %in% fGender)     
  }
  
  if (fExposure[1] != 'all') {
    # Store unkowns
    unknownframe <- distinct(bind_rows(unknownframe, filter(includeframe, 
      expgroup == 'unknown')), .keep_all = TRUE)
    unknownframe <- distinct(bind_rows(unknownframe, filter(includeframe, 
      is.na(expgroup))), .keep_all = TRUE)
    
    # Remove missing so not double countered in excluded and included -
    # need so excluded doesn't pick up "unknown"
    includeframe <- filter(includeframe, expgroup != 'unknown') 
    includeframe <- filter(includeframe, !is.na(expgroup))
    
    # Exclude ones we don't want and keep ones we want
    excludeframe <- distinct(bind_rows(excludeframe, 
      filter(includeframe, !(expgroup %in% fExposure))), .keep_all = TRUE)
    includeframe <- filter(includeframe, expgroup %in% fExposure) 
  }
  
  if (fCob[1] != 'all') {
    # Store unkowns
    unknownframe <- distinct(bind_rows(unknownframe, 
      filter(includeframe, cob == 'Not Reported')), .keep_all = TRUE)
    unknownframe <- distinct(bind_rows(unknownframe, 
      filter(includeframe, is.na(cob))), .keep_all = TRUE)
    
    # Remove missing so not double countered in excluded and included -
    # need so excluded doesn't pick up "Not Reported"
    includeframe <- filter(includeframe, cob!='Not Reported') 
    includeframe <- filter(includeframe, !is.na(cob))
    
    # Exclude ones we don't want and keep ones we want - note a number 
    # of special cases
    if (fCob[1] == 'non-australia') {
      # Special case - not born in Australia/born overseas
      excludeframe <- distinct(bind_rows(excludeframe,
        filter(includeframe, cob == 'Australia')), .keep_all = TRUE)
      includeframe <- filter(includeframe, cob != 'Australia') 
    } else if (fCob[1] =='non-aus-nz') {
      # Special case - not born in Australia or NZ
      excludeframe <- distinct(bind_rows(excludeframe, filter(includeframe, 
        cob %in% c('Australia', 'New Zealand'))), .keep_all = TRUE)
      includeframe <- filter(includeframe, 
        !(cob %in% c('Australia', 'New Zealand')))
    } else {
      excludeframe <- distinct(bind_rows(excludeframe,
        filter(includeframe, !(cob %in% fCob))), .keep_all = TRUE)
      includeframe <- filter(includeframe, cob %in% fCob)
    }
  }
  
  if(fAtsi[1] != 'all' && fCob[1] == "Australia" && length(fCob) == 1){
    # Store unkowns
    unknownframe <- distinct(bind_rows(unknownframe, filter(includeframe, 
      aboriggroup == 'Not Reported')), .keep_all = TRUE)
    unknownframe <- distinct(bind_rows(unknownframe, filter(includeframe, 
      is.na(aboriggroup))), .keep_all = TRUE)
    
    # Remove missing so not double countered in excluded and included -
    # need so excluded doesn't pick up "Not Reported"
    includeframe <- filter(includeframe, aboriggroup != 'Not Reported') 
    includeframe <- filter(includeframe, !is.na(aboriggroup))
    
    # Exclude ones we don't want and keep ones we want
    excludeframe <- distinct(bind_rows(excludeframe, 
      filter(includeframe, !(aboriggroup %in% fAtsi))), .keep_all = TRUE)
    includeframe <- filter(includeframe, aboriggroup %in% fAtsi)  
  }
  
  if(fState[1] != 'all'){
    # Store unkowns
    unknownframe <- distinct(bind_rows(unknownframe, filter(includeframe, 
      state == 'Not Reported')), .keep_all = TRUE)
    unknownframe <- distinct(bind_rows(unknownframe, filter(includeframe, 
      is.na(state))), .keep_all = TRUE)
    
    
    # Remove missing so not double countered in excluded and included -
    # need so excluded doesn't pick up "Not Reported"
    includeframe <- filter(includeframe, state != 'Not Reported',
      !is.na(state)) 
    
    # Exclude ones we don't want and keep ones we want
    excludeframe <- distinct(bind_rows(excludeframe, 
      filter(includeframe, !(state %in% fState)),
      filter(unknownframe, !(state %in% fState))), .keep_all = TRUE)
    includeframe <- filter(includeframe, state %in% fState)
    unknownframe <- filter(unknownframe, state %in% fState)
  }
  
  if(fGlobalRegion[1] != "all"){
    # Store unkowns
    unknownframe <- distinct(bind_rows(unknownframe, filter(includeframe, 
      globalregion == "NR")), .keep_all = TRUE)
    unknownframe <- distinct(bind_rows(unknownframe, filter(includeframe, 
      is.na(globalregion))), .keep_all = TRUE)
    
    
    # Remove missing so not double countered in excluded and included -
    # need so excluded so excluded doesn't pick up "Not Reported"
    includeframe <- filter(includeframe,
      globalregion != "NR")
    includeframe <- filter(includeframe, !is.na(countrygroup))
    
    # Exclude ones we don't want and keep ones we want - note a number 
    # of special cases 
    if(fGlobalRegion == "Other cob"){
      # Category for ASR for everyone not Australian, South-East Asia,
      # or Sub-Saharan Africa born 
      unknownframe <- distinct(bind_rows(unknownframe, filter(includeframe, 
      cob %in% c("Overseas" ,"Not Reported"))), .keep_all = TRUE)
      
      excludeframe <- distinct(bind_rows(excludeframe, 
        filter(includeframe, globalregion %in% c("South-East Asia", 
          "Sub-Saharan Africa"))), .keep_all = TRUE)
      excludeframe <- distinct(bind_rows(excludeframe,
        filter(includeframe, cob == "Australia")), .keep_all = TRUE)
      
      includeframe <- filter(includeframe, 
        !(cob %in% c("Australia", "Overseas", "Not Reported")))    
      includeframe <- filter(includeframe, 
        !(globalregion %in% c("South-East Asia", "Sub-Saharan Africa")))
      
    }else{
      excludeframe <- distinct(bind_rows(excludeframe, filter(includeframe, 
        !(globalregion %in% fGlobalRegion))), .keep_all = TRUE)
      includeframe <- filter(includeframe, globalregion %in% fGlobalRegion)
    }
  }
  
  # Return resulting dataframes
  return(list(includeframe, excludeframe, unknownframe))
}
