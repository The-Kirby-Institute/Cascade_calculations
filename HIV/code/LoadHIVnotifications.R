## LoadHIVnotifications.R
#
# Self-contained function to quickly load the HIV notifications data 
# in the national HIV registry.

require(tidyverse)

LoadHIVnotifications <- function(analysisYear) {
  
  # Set-up directories
  basePath <- "C:/Users/rgray/UNSW/Australian Care Cascades - Documents/"
  Rcode <- file.path(basePath, "code") 
  HIVcode <- file.path(basePath, "HIV", "code") 
  dataFolder <- file.path(basePath, "HIV", "data")
  
  # Notifications folder (a private folder needing VPN access)
  notificationsFolder <- file.path("/", "SVR-NAS", "Public", "SERP", "Data",
    "National HIV Registry", "Cascades") 
  
  # Functions 
  source(file.path(Rcode, "AgeCat.R"))
  source(file.path(HIVcode, "TidyNotifications.R"))
  
  # Load cleaned notifications data
  rawNotifications <- read.csv(file.path(notificationsFolder,
    paste0("cascadeHIVnotifications-clean-", toString(analysisYear),
      ".csv"))) 
  
  # Read in country code data
  countryCodes <- read.csv(file.path(dataFolder, "countryRegionCodes.csv"))
  
  # Read in location of diagnosis coding
  regionCodes <- read.csv(file.path(dataFolder, "postcodeRegionCodes.csv"))
  
  # Tidy up notifications for exploration
  hivData <- TidyNotifications(rawNotifications, analysisYear, countryCodes,
    regionCodes)
  
  return(hivData)
  
}
