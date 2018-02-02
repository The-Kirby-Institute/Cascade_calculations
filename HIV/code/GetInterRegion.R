## R function to calculate interregional migration rates within Australia

# Richard T. Gray

GetInterRegion <- function(finalYear, erpData, interstateData, 
  interRegionData, targetState, targetRegion) {
  
  # First need to sort out if it is regional or state estimate
  if (targetState[1] != "all" && targetRegion[1] != "all") {
    stop("Cannot do state and regional estimates at same time")
  }
  
  allYears <- 1980:finalYear
  hivInterRegion <- tibble(year = allYears)
  
  # Now do calculations
  if (length(targetRegion) == 1 && targetRegion[1] == "all") {
    # We are doing state calculations 
    # First estimate erp for each state
    states <- c("nsw", "vic", "qld", "nt", "wa", "sa", "tas", "act", "all")
    
    # Estimate ERP from ABS migration data - 2004 to 2014
    erpState <- erpData %>%
      filter(state %in% targetState) %>%
      select(year, state, erp) %>%
      group_by(year) %>%
      summarise(erp = sum(erp))
    
    erpStateAll <- erpData %>%
      filter(state == "all") %>%
      select(year, state, erp) %>%
      group_by(year) %>%
      summarise(erp = sum(erp))
    
    # Extrapolate to years of epidemic assuming exponential/poisson fit
    gmErp <- glm(erp ~ year, erpState, family = "poisson")
    erpEstimate <- predict(gmErp, data.frame(year = allYears), 
      type = "response")
    
    gmErpAll <- glm(erp ~ year, erpStateAll, family = "poisson")
    erpEstimateAll <- predict(gmErpAll, data.frame(year = allYears), 
      type = "response")
    
    hivInterRegion <- hivInterRegion %>%
      mutate(erp = erpEstimate,
        erpall = erpEstimateAll) %>%
      mutate(outererp = erpall - erp)

    # Extract arrivals and departures
    interstateData %>% 
      filter(state %in% targetState) %>%
      group_by(year) %>%
      summarise(arrivals = sum(arrivals),
        departures = sum(departures))

    lmDepart <- lm(departures ~ year, data = interstateData)
    departState <- predict(lmDepart, data.frame(year = allYears))
    
    lmArrive <- lm(arrivals ~ year, data = interstateData)
    arriveState <- predict(lmArrive, data.frame(year = allYears))
    
    hivInterRegion <- hivInterRegion %>%
      mutate(departs = departState,
        arrives = arriveState) %>%
      mutate(departrate = departs/ erp,
        arriverate = arrives / outererp)
    
  } else {
    # We are doing regional SA4 level calculations 
  }
  
  return(hivInterRegion)
}
