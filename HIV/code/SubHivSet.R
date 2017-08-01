## R function to extract subset of interest from notifications

# N.A. Bretana

SubHivSet <- function(hivdataframe, fAge, fGender, fExposure, fCob, fAtsi, fState, fGlobalRegion){
  
  subframe <- hivdataframe
  includeframe <- subframe
  unknownframe <- data_frame()
  excludeframe <- data_frame()
  
  if(fAge=='all'&&fGender=='all'&&fExposure=='all'&&fCob=='all'&&fAtsi=='all'&&fState=='all'&&fGlobalRegion=='all'){
    unknownframe <- data_frame()
  }else{
    unknownframe <- subframe
  }
  
  if(fAge!='all'){
    unknownframe <- filter(unknownframe, agebin == 'not_reported')
    unknownframe <- bind_rows(unknownframe, filter(subframe, is.na(agebin)))
    includeframe <- filter(includeframe, agebin!='Not Reported') 
    includeframe <- filter(includeframe, !is.na(agebin))
    excludeframe <- filter(includeframe, agebin != fAge)
    includeframe <- filter(includeframe, agebin == fAge) 
  }
  
  if(fGender!='all'){
    unknownframe <- filter(unknownframe, sex == 'unknown')
    unknownframe <- bind_rows(unknownframe, filter(subframe, is.na(sex)))
    includeframe <- filter(includeframe, sex!='Not Reported') 
    includeframe <- filter(includeframe, !is.na(sex))
    excludeframe <- filter(includeframe, sex != fGender)
    includeframe <- filter(includeframe, sex == fGender)     
  }
  
  if(fExposure!='all'){
    unknownframe <- filter(unknownframe, expgroup == 'unknown')
    unknownframe <- bind_rows(unknownframe, filter(subframe, is.na(expgroup)))
    includeframe <- filter(includeframe, expgroup!='Not Reported') 
    includeframe <- filter(includeframe, !is.na(expgroup))
    excludeframe <- filter(includeframe, expgroup != fExposure)
    includeframe <- filter(includeframe, expgroup == fExposure) 
  }
  
  if(fCob!='all'){
    unknownframe <- filter(unknownframe, cob == 'Not Reported')
    unknownframe <- bind_rows(unknownframe, filter(subframe, is.na(cob)))
    if(fCob=='non-australia'){
      excludeframe <- filter(includeframe, cob == 'Australia')
      includeframe <- filter(includeframe, cob!='Not Reported') #remove all missings
      includeframe <- filter(includeframe, !is.na(cob)) #remove all missings
      includeframe <- filter(includeframe, cob != 'Australia') #get only non-Australians
    }else{
      excludeframe <- filter(includeframe, cob != fCob)
      excludeframe <- filter(excludeframe, cob != 'Not Reported')
      excludeframe <- filter(excludeframe, !is.na(cob))
      includeframe <- filter(includeframe, cob == fCob)
    }
  }
  
  if(fAtsi!='all'){
    unknownframe <- filter(unknownframe, aboriggroup == 'Not Reported')
    unknownframe <- bind_rows(unknownframe, filter(subframe, is.na(aboriggroup)))
    includeframe <- filter(includeframe, aboriggroup!='Not Reported') 
    includeframe <- filter(includeframe, !is.na(aboriggroup))
    excludeframe <- filter(includeframe, aboriggroup != fAtsi)
    includeframe <- filter(includeframe, aboriggroup == fAtsi)  
  }
  
  if(fState != 'all'){
    unknownframe <- bind_rows(unknownframe, filter(includeframe, is.na(state)))
    excludeframe <- bind_rows(excludeframe, filter(includeframe, state != fState))
    includeframe <- filter(includeframe, state == fState)
  }
  
  if(fGlobalRegion != 'all'){
    unknownframe <- filter(unknownframe, globalregion == 'Not Reported')
    unknownframe <- bind_rows(unknownframe, filter(includeframe, is.na(globalregion)))
    if(fGlobalRegion=="Other cob"){
      excludeframe <- bind_rows(excludeframe, filter(includeframe, globalregion == "South-East Asia"))
      excludeframe <- bind_rows(excludeframe, filter(includeframe, globalregion == "Sub-Saharan Africa"))
      includeframe <- filter(includeframe, globalregion != "South-East Asia")
      includeframe <- filter(includeframe, globalregion != "Sub-Saharan Africa")
    }else{
      excludeframe <- bind_rows(excludeframe, filter(includeframe, globalregion != fGlobalRegion))
      includeframe <- filter(includeframe, globalregion == fGlobalRegion)
    }
  }
  
  return(list(includeframe, excludeframe, unknownframe))
}
