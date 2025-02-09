## TidyNotifications.R 

# This function further cleans notifications data and adds 
# additional variables used for subsetting and calculation purposes

# Richard T. Gray

TidyNotifications <- function(notificationsData, analysisYear, crCodes,
  regCodes, appendExtra = TRUE, removeExcess = TRUE) {
  # This function is used to produce a tidier data set for the notiifcations
  # by cleaning up the key variable for the HIV cascade calculations. 
  # Requires:dplyr
  # 
  # Needs AgeCat.R to be loaded 
  #
  # Args:
  #   hivData: Data frame of raw semi-process notications data.
  #   analysisYear: Year of notifications we are runnning. Required because
  #     variables have changed over time.
  #   crCodes: Data frame holding the country codes.
  #   regCodes: Data frame holding the postcode to region codes.
  #   appendExtra (optional): Append extra columns for age-bins and CD4 
  #     category. Default TRUE
  #   removeExcess(optional): Remove columns that are not needed for 
  #     cascade calculations. Default TRUE
  # 
  # Returns: 
  #   A data frame of cleaned notifications with additional variables if 
  #   requested. 
  #   
  #-----------------------------------------------------------------------
  
  # Fix up country codes for AUstralia from 2018-- Australia to be 1101 
  # with old 1101 (Christmas Island, Cocos (Keeling) Island) becoming 
  # Australia
  if (analysisYear >= 2018) {
    crCodes <- crCodes %>%
      filter(COUNTRY_CODE != 1101) %>%
      mutate(COUNTRY_CODE = ifelse(COUNTRY_NAME == "Australia", 1101, 
        COUNTRY_CODE))
    auscode <- 1101
  }
  
  hivData <- notificationsData
  
  # Setup standard extra columns for analysis
  hivData$yeardiagnosis <- as.numeric(substr(hivData$datediagnosis, 1, 4))
  hivData$yeardeath <- as.numeric(substr(hivData$datedeath, 1, 4))
  
  # Aboriginal and Torres Strait Islanders--------------------------------
  # We are assuming indigenous means Australian born AND indigenous 
  # only. May need to reconsider because there are some notifications which
  # have no country of birth but are classified as indigenous. 
  
  #clean aboriggroup
  hivData$aboriggroup <- rep(NA, nrow(hivData))
  
  # Aboriginal group
  if (analysisYear < 2015) {
    hivData$aboriggroup[hivData$cob != auscode|hivData$rob!=7] <- "othercob"
    hivData$aboriggroup[hivData$cob == 0] <- NA
    hivData$aboriggroup[hivData$cob == auscode & 
        hivData$indigenous == "Aboriginal"] <- "indigenous"
    hivData$aboriggroup[hivData$cob == auscode & 
        hivData$indigenous == "Non indigenous"] <- "non_indigenous" 
  } else if (analysisYear == 2015) {
    
    hivData$aboriggroup[hivData$cob == auscode & 
        hivData$indigenous == "Aboriginal"] <- "indigenous"
    hivData$aboriggroup[hivData$indigenous != "Aboriginal"] <- "non_indigenous"
    hivData$aboriggroup[is.na(hivData$aboriggroup)] <- "non_indigenous"
  } else {
    hivData$aboriggroup[hivData$cob == auscode & 
        hivData$indig == 1] <- "indigenous"
    hivData$aboriggroup[hivData$indig != 1] <- "non_indigenous"
    hivData$aboriggroup[is.na(hivData$aboriggroup)] <- "non_indigenous"
  }
  
  # Country and region of birth--------------------------------------------
  
  # Transfer cob to cobCode
  hivData$cobcode <- hivData$cob
  
  # Load cob and region codes
  # crCodes <- read.csv(file.path(dataFolder, "countryRegionCodes.csv"))
  
  # If cob is not in crCodes/NA, change to Not Reported
  # hivData$cob[!(hivData$cobcode %in% crCodes$COUNTRY_CODE)] <- "Not Reported" 
  
  # Change cob from code to String
  hivData$cob <- crCodes$COUNTRY_NAME[match(hivData$cobcode,
    crCodes$COUNTRY_CODE)]
  
  # Add region
  hivData$globalregion <- as.character(crCodes$REGION[match(hivData$cobcode,
    crCodes$COUNTRY_CODE)])
  
  # Change NAs to not reported (only 22 missing cobs in the data set)
  hivData$cob[is.na(hivData$cob)] <- "Not Reported" 
  hivData$cob[hivData$cob == "Not Known"] <- "Not Reported" 
  hivData$globalregion[hivData$globalregion == "Not Known"] <- "Not Reported" 
  hivData$globalregion[is.na(hivData$globalregion)] <- "Not Reported" 
  
  # Convert some of the not reporteds to "overseas" if country of birth is
  # missing but region of birth is available.
  hivData$cob <- as.character(hivData$cob)
  
  if (analysisYear >= 2017) {
    hivData$cob[hivData$cob == "Not Reported"  &
        !(hivData$rob %in%  c("NR", "Oceania/Anatartica"))] <- "Overseas"
  } else {
    hivData$globalregion[hivData$globalregion == "Not Reported"] <- "NR"
    hivData$cob[hivData$cob == "Not Reported"  &
        !(hivData$rob %in%  c(0, 7))] <- "Overseas"
  }
  
  # Postcode of diagnosis to SA3 code--------------------------------------
  hivData$diag_region <- NA
  
  # Change cob from code to String
  hivData$diag_region <- as.character(regCodes$SA3_name_2016[match(hivData$postcode,
    regCodes$postcode)])
  
  # If region is not in regionCodes/NAs, change to unknown/not reported
  hivData$diag_region[!(hivData$postcode %in% regCodes$postcode)] <- "Unknown"
  hivData$diag_region[is.na(hivData$postcode)] <- "Not Reported"
  
  # Additional variables---------------------------------------------------
  if (appendExtra) {
    # Convert NAs to other values for ease of filtering
    
    # Add agebin variable for age at diagnosis
    # Age groups: <15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 
    #   50-54, 55-59, 60-64, 65-69, 70-74, 75-79, 80-84, 
    #   85+ (18 age groups)

    hivData$agebin <- AgeCat(hivData$agehiv, lower = 0, upper = 85, by = 5)
    
    hivData$agebin <- factor(hivData$agebin, 
      levels = levels(addNA(hivData$agebin)), 
      labels = c(levels(hivData$agebin), 
        "not_reported"), exclude = NULL)
    
    # Add CD4 count variable
    cd4Labels <- c("cl200", "c200_349", "c350_499", "cg500")
    hivData$cd4bin <- cut(floor(hivData$cd4count), 
      breaks = c(0, 200, 350, 500, Inf), 
      right = FALSE, 
      labels = cd4Labels)
    
    hivData$cd4bin <- factor(hivData$cd4bin, 
      levels = levels(addNA(hivData$cd4bin)), 
      labels = c(levels(hivData$cd4bin), 
        "not_reported"), exclude = NULL)
    
    # Add CD4 count London Method
    cd4LMlabels <- c("cl20", "c20_49", "c50_99", "c100_149", "c150_199", 
      "c200_249", "c250_299", "c300_349", "cg350")
    hivData$cd4London <- cut(floor(hivData$cd4count), 
      breaks = c(0, 20, 50, 100, 150, 200,
        250, 300, 350, Inf), 
      right = FALSE, 
      labels = cd4LMlabels)
    
    hivData$cd4London <- factor(hivData$cd4London, 
      levels = levels(addNA(hivData$cd4London)), 
      labels = c(levels(hivData$cd4London), 
        "not_reported"), exclude = NULL)
    
    # Diagnosed with AIDS
    hivData$yearaids <- as.numeric(format(as.Date(hivData$dateaids), 
      "%Y"))
    hivData$yearaids[is.na(hivData$yearaids)] <- NA
    
    # Add column with hiv, hivaids or aids
    hivData$typediagnosis <- "hiv"
    hivData$typediagnosis[!is.na(hivData$yearaids)] <- "aids"
    
    indices <- hivData$yearhiv == hivData$yearaids
    hivData$typediagnosis[indices] <- "hivaids"
    
  }
  
  if (removeExcess) {
    # Remove unwanted columns
    hivData <- dplyr::select(hivData, -rob, -dateaids, -partnercob, -dateneg, 
      -dateindet, -datedeath, -causeofdeath, -indigenous, 
      -cobcode) 
  }
  
  hivData <- hivData %>% as_tibble()
  
  return(hivData)
}
