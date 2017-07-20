## R function to estimate estimate mortality rates

#R. T. Gray and N.A. Bretana

hivParams <- read.csv(file.path(dataFolder,
                                "individualHIVparameters.csv"), 
                      as.is = 1)
hivParams <- select(hivParams, parameter, value)

for (ii in 1:nrow(hivParams)) {
  assign(hivParams$parameter[ii], hivParams$value[ii])
}

# Load AHOD data to calculate mortality and do some simple cleaning
ahodData <- read.csv(file.path(dataFolder, 
                               paste("ahod", toString(analysisYear), 
                                     ".csv", sep = "")))

# Extract number of known deaths and store in a central data frame
#hivDeaths <- hivData %>% 
#  filter(! is.na(yeardeath)) %>%
#  group_by(yeardeath) %>% 
#  summarise(number_deaths = n()) %>% 
#  filter(yeardeath <= analysisYear)  
hivDeaths <- hivSet %>% 
  filter(! is.na(yeardeath)) %>%
  group_by(yeardeath) %>% 
  summarise(number_deaths = n()) %>% 
  filter(yeardeath <= analysisYear) 

#Fill up missing years
fillMissing <- as.data.frame(allYears) 
colnames(fillMissing) <- c("yeardeath")
hivDeaths <- merge(fillMissing, hivDeaths, by="yeardeath", all.x=TRUE)
hivDeaths[is.na(hivDeaths)] <- 0

# Calculate sensitivity of recorded deaths based on linkage results
linkageFactor <- (linkedDeaths + notifiedDeaths) / notifiedDeaths
linkageFactorLower <- linkageFactor * (1 - linkageSensitivity)
linkageFactorUpper <- linkageFactor * (1 + linkageSensitivity)  

#  We don't need to adjust deaths for number of unique diagnoses as
# different dates of death should result in different people

# Set up linked deaths
hivDeaths$inflated_deaths <- hivDeaths$number_deaths * linkageFactor
hivDeaths$inflated_deaths_lower <- hivDeaths$number_deaths * 
  linkageFactorLower
hivDeaths$inflated_deaths_upper <- hivDeaths$number_deaths * 
  linkageFactorUpper

# No deaths in 1980 and 1982 so add this data manually to ensure 
# everything lines up
#hivDeaths <- rbind(hivDeaths, c(1980, 0, 0, 0, 0))
#hivDeaths <- rbind(hivDeaths, c(1982, 0, 0, 0, 0))
hivDeaths <- arrange(hivDeaths, yeardeath)

# Now calculate mortality rate for linkage data --------------------------
cumDiagnoses <- hivResults$uniquecases # totalnotifications #uniquecases
deaths <- hivDeaths$inflated_deaths
deaths_min <- hivDeaths$inflated_deaths_lower
deaths_max <- hivDeaths$inflated_deaths_upper
annDiags <- c(cumDiagnoses[1], diff(cumDiagnoses))  # annual diagnoses

# Calculate number living each year 
numLiving <- rep(NA, length(annDiags))
numLiving_min <- rep(NA, length(annDiags))
numLiving_max <- rep(NA, length(annDiags))

numLiving[1] <- annDiags[1]
numLiving_min[1] <- annDiags[1]
numLiving_max[1] <- annDiags[1]

# Overall migration rate for deaths estimate
allMigrate <- filter(hivMigrate, state == "all") 

for (ii in 2:length(annDiags)) {
  numLiving[ii] <- numLiving[ii-1] + annDiags[ii] - deaths[ii-1] 
  - numLiving[ii-1] * allMigrate$rate[ii-1]
  numLiving_min[ii] <- numLiving_min[ii-1] + annDiags[ii] -
    deaths_max[ii-1] - numLiving_min[ii-1] * allMigrate$upper[ii-1]
  numLiving_max[ii] <- numLiving_max[ii-1] + annDiags[ii] -
    deaths_min[ii-1] - numLiving_max[ii-1] * allMigrate$lower[ii-1]
}

# Calculate mortality
linkageMortality <- deaths / numLiving
linkageMortalityMin <- deaths_min/numLiving_max
linkageMortalityMax <- deaths_max/numLiving_min

#Append AHOD mortality ---------------------------------------

# Calculate crude death rate for each state and year - use 95% CI as the upper and lower bound

ahodDeaths <- ahodData %>% 
  filter(population == "ALL") %>%
  group_by(state, year) %>%
  summarise(n = sum(n_id), 
            deaths = sum(n_death), 
            deathrate = unname(prop.test(sum(n_death), 
                                         sum(n_id))$estimate), 
            lower = unname(prop.test(sum(n_death), sum(n_id))$conf.int[1]),
            upper = unname(prop.test(sum(n_death), sum(n_id))$conf.int[2])) %>%
  filter(year > 2003)

# First find index corresponding to year of linkage 
# indexLinkage <- max(seq(along = min(hivResults$yeardiagnosis):linkageYear))
indexLinkage <- max(seq(along = 1980:linkageYear))

# Save in deathrate
deathRate <- rep(NA, length(allYears))
deathRate_min <- rep(NA, length(allYears))
deathRate_max <- rep(NA, length(allYears))

deathRate[1:indexLinkage] <- linkageMortality[1:indexLinkage]
deathRate[(indexLinkage + 1):length(deathRate)] <- filter(ahodDeaths, 
                                                          state == "all")$deathrate # 

deathRate_min[1:indexLinkage] <- linkageMortalityMin[1:indexLinkage]
deathRate_min[(indexLinkage + 1):length(deathRate)] <- filter(ahodDeaths, 
                                                              state == "all")$lower # 

deathRate_max[1:indexLinkage] <- linkageMortalityMax[1:indexLinkage]
deathRate_max[(indexLinkage+1):length(deathRate)] <- filter(ahodDeaths, 
                                                            state == "all")$upper # 

deathRate[is.na(deathRate)] <- 0
deathRate_min[is.na(deathRate_min)] <- 0
deathRate_max[is.na(deathRate_max)] <- 0



