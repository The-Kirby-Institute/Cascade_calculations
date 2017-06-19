## R function to estimate estimate migration rates

#R. T. Gray and N.A. Bretana

# Load all the data sets we need - the ABS data is stored in the main 
# Cascade_calculations respository data/ directory

mainDataFolder <- file.path(dirname(getwd()), "data")

absMigration <- read.csv(file.path(mainDataFolder, 
                                   paste0("ABS_migration_clean-", toString(analysisYear), ".csv")), 
                         as.is = 1)
absDeparts <- read.csv(file.path(mainDataFolder, 
                                 paste0("ABS_departures_clean-", toString(analysisYear), ".csv")),
                       as.is = 1)

absInterstate <- read.csv(file.path(mainDataFolder, 
                                    paste0("ABS_interstate_clean-", toString(analysisYear), ".csv")), 
                          as.is = 1)

# Depending on data used calculate departure rate for each state ---------

states <- c("nsw", "vic", "qld", "nt", "wa", "sa", "tas", "act", "all")
# nstates <- length(states)

allPredicts <- data.frame(year = numeric(), 
                          state = character(),
                          rate = numeric(),
                          lower = numeric(),
                          upper = numeric())

if (permDeparts) {
  # Use permanent departures nationally
  absDeparts <- mutate(absDeparts, migrate = permanent / erp)
  mrate <- absDeparts$migrate
  
  if (analysisYear <= 2014) {
    # Keep methdology for 2014 cascade for replication purposes
    
    # mrate is missing a value for 1980 assume same value as 1981
    mrate <- c(mrate[1], mrate)
    lmrate <- 0
    umrate <- mrate
    mrate <-  mrate / 2
    
  } else{
    # Change in methodlogy for migration rates from 2015.
    
    # Hard coded adjustment factor to account for age > 15 years and 
    # sex. These harded coded estimates for 1981-2015 are determined in 
    # the file project_care_cascades/data/Migration_options-2016-10-12.xlsx
    relativeRate <- c(1.2499, 1.2499,	1.2499,	1.240169381, 1.235807895,
                      1.235391781, 1.236048319,	1.236375068, 1.236578904,
                      1.235863902, 1.23556672, 1.235166515,	1.234926588,
                      1.234456001, 1.234217118, 1.233938271, 1.233473343,
                      1.232885687, 1.23528249, 1.234803523, 1.234243908,
                      1.231400608, 1.231146313, 1.230567577, 1.22257888,
                      1.211940692, 1.20153294, 1.191640652, 1.180852205,
                      1.170562151, 1.160456052,	1.150374872, 1.140323439,
                      1.130276173,	1.12031362)
    
    mrate <- relativeRate * mrate
    
    # mrate is missing a value for 1980 assume same value as 1981
    mrate <- c(mrate[1], mrate)
    lmrate <- 0
    umrate <- 2 * mrate
  }
  
  # Fill in the state values - produce states based estimates 
  # using NOM departure rates to adjust the national rate
  absMigration <- mutate(absMigration, migrate = departures / erp)
  
  nonmrateAll <- filter(absMigration, state == "all")
  lmrateAll <- lm(migrate ~ year, data = nonmrateAll)
  expmrateAll <- predict(lmrateAll, data.frame(year = allYears),
                         level = 0.9, interval = "confidence")
  
  for (region in states) {
    # Adjust national rate to reflect difference at state level using 
    # national NOM departures data
    
    nonmrate <- filter(absMigration, state == region)
    
    # Now we need to fit and extrapolate for other years
    lmrate <- lm(migrate ~ year, data = nonmrate)
    expmrate <- predict(lmrate, data.frame(year = allYears),
                        level = 0.9, interval = "confidence")
    
    relexpmrate <- expmrate[, 1] / expmrateAll[, 1]
    
    
    allPredicts <- rbind(allPredicts, data.frame(year = allYears, 
                                                 state = region,
                                                 rate = mrate * relexpmrate, 
                                                 lower = 0, 
                                                 upper = 2 * mrate * relexpmrate))
  }
  
  # Store results in final data frame
  hivMigrate <- allPredicts
  
} else {
  # Use state based NOM departures
  
  # Calculate migration rate overall and for each state
  absMigration <- mutate(absMigration, migrate = departures / erp)
  
  for (region in states) {
    mrate <- filter(absMigration, state == region)
    
    # Now we need to fit and extrapolate for other years
    lmrate <- lm(migrate ~ year, data = mrate)
    expmrate <- predict(lmrate, data.frame(year = allYears),
                        level = 0.9, interval = "confidence")
    
    # Store extrapolated rate
    allPredicts <- rbind(allPredicts, data.frame(year = allYears, 
                                                 state = region,
                                                 rate = expmrate[, 3]/2, 
                                                 lower = 0, 
                                                 upper = expmrate[, 3]))
  }
  
  # Store results in final data frame
  hivMigrate <- allPredicts
}