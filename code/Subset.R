## R function to extract subset of interest from notifications

# N.A. Bretana

subhivset <- function(hivdataframe, fAge, fExposure, fCob, fAtsi, fLocalRegion, fGlobalRegion){
  
  subframe <- hivdataframe
  includeframe <- subframe
  unknownframe <- data_frame()
  excludeframe <- data_frame()
  
  if(fAge!='all'){
    unknownframe <- filter(subframe, agebin == 'Not Reported')
    unknownframe <- bind_rows(unknownframe, filter(subframe, is.na(agebin)))
    includeframe <- filter(includeframe, agebin!='Not Reported') 
    includeframe <- filter(includeframe, !is.na(agebin))
    excludeframe <- filter(includeframe, agebin != fAge)
    includeframe <- filter(includeframe, agebin == fAge) 
  }
  if(fExposure!='all'){
    unknownframe <- filter(unknownframe, expgroup == 'Not Reported')
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
  
  if(fLocalRegion != 'all'){
    unknownframe <- bind_rows(unknownframe, filter(includeframe, is.na(localregion)))
    excludeframe <- bind_rows(excludeframe, filter(includeframe, localregion != fLocalRegion))
    includeframe <- filter(includeframe, localregion == fLocalRegion)
  }
  
  if(fGlobalRegion != 'all'){
    unknownframe <- bind_rows(unknownframe, filter(includeframe, is.na(localregion)))
    excludeframe <- bind_rows(excludeframe, filter(includeframe, localregion != globalregion))
    includeframe <- filter(includeframe, globalregion == fGlobalRegion)
  }
  
  return(list(includeframe, excludeframe, unknownframe))
}
