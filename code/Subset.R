## R function to extract subset of interest from notifications

# N.A. Bretana

subhivset <- function(hivdataframe, fAge, fExposure, fCob, fAtsi, fLocalRegion, fGlobalRegion){
  
  subframe <- hivdataframe
  
  if(fAge!='all'){
    #to be filled later
  }
  if(fExposure!='all'){
    subframe <- filter(subframe, exposure == fExposure)
  }
  if(fCob!='all'){
    if(fCob=='non-australia'){
      subframe <- filter(subframe, cob!='Not Reported') #remove all missings
      subframe <- filter(subframe, !is.na(cob)) #remove all missings
      subframe <- filter(subframe, cob != 'Australia') #get only non-Australians
    }else{
      subframe <- filter(subframe, cob == fCob)
    }
  }
  if(fAtsi!='all'){
    if(fAtsi=='non-australia'){
      #subframe <- filter(subframe, aboriggroup == 'othercob')
    }else{
      if(fAtsi=='non-atsi'){
        subframe <- filter(subframe, aboriggroup == 'non-indigenous') 
      }else if(fAtsi=='atsi'){
        subframe <- filter(subframe, aboriggroup == 'indigenous') 
      }
    }
    #what to do with NA's?
  }
  if(fLocalRegion!='all'){
    subframe <- filter(subframe, localregion == fLocalRegion)
  }
  if(fGlobalRegion!='all'){
    subframe <- filter(subframe, globalregion == fGlobalRegion)
  }
  
  return(subframe)
}
