## R function to extract subset of interest from notifications

# N.A. Bretana

subhivset <- function(hivdataframe, fAge, fExposure, fCob, fAtsi, fLocalRegion, fGlobalRegion){
  
  subframe <- hivdataframe
  includeframe <- subframe
  # excludeframe <- 
  
  if(fAge!='all'){
    #to be filled later
  }
  if(fExposure!='all'){
  }
  
  if(fCob!='all'){
    unknownframe <- filter(subframe, cob == 'Not Reported')
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
    # if(fAtsi=='non-australia'){
    #   #subframe <- filter(subframe, aboriggroup == 'othercob')
    # }else{
    if (fCob =='australia') {
      if(fAtsi=='non-atsi'){
        includeframe <- filter(includeframe, aboriggroup == 'non-indigenous') 
      }else if(fAtsi=='atsi'){
        includeframe <- filter(includeframe, aboriggroup == 'indigenous') 
      }
    }
    #what to do with NA's? EXCLUDE INDIGENOUS 
  }
  if(fLocalRegion!='all'){
    unknownframe <- bind_rows(unknownframe, filter(includeframe, is.na(localregion)))
    excludeframe <- bind_rows(excludeframe, filter(includeframe, localregion != fLocalRegion))
    includeframe <- filter(includeframe, localregion == fLocalRegion)
    
  }
  if(fGlobalRegion!='all'){
    unknownframe <- bind_rows(unknownframe, filter(includeframe, is.na(localregion)))
    excludeframe <- bind_rows(excludeframe, filter(includeframe, localregion != globalregion))
    includeframe <- filter(includeframe, globalregion == fGlobalRegion)
  }
  
                         
  
  return(list(includeframe, excludeframe, unknownframe))
}
