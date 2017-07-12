## Functions for ECDC HIV Modeling Tool data

# Richard T. Gray

# This script contains a number of functions used to produce the data 
# for the ECDC annual HIV notifications.

# Create output folders ---------------------------------------------------
EcdcFolders <- function(outputFolder, model, includeOS = TRUE,
                        time = FALSE) {
  
  if (includeOS) {
      folderAll <- "all"
      folderExp <- "exposure"
  } else {
      folderAll <- "all_exclude"
      folderExp <- "exposure_exclude"
  }
  
  if (time) {
    currTime <- format(Sys.time(), "%y-%m-%d")
    folderAll <- paste0(folderAll, "_", currTime)
    folderExp <- paste0(folderExp, "_", currTime)
  }
  
  # All ---------------------------------------------------------------------
  modelPathAll <- file.path(outputFolder, "ECDC_models", model, folderAll)
  dir.create(modelPathAll, showWarnings = FALSE, recursive = TRUE)
  
  # Create other directories
  dataFolderAll <- file.path(modelPathAll, "data")
  resultsFolderAll <- file.path(modelPathAll, "results")
  
  dir.create(dataFolderAll, showWarnings = FALSE, recursive = TRUE)
  dir.create(resultsFolderAll, showWarnings = FALSE, recursive = TRUE)
  
  # By at-risk category -----------------------------------------------------
  modelPathRisk <- file.path(outputFolder, "ECDC_models", model, folderExp)
  dir.create(modelPathRisk, showWarnings = FALSE, recursive = TRUE)
  
  # Create other directories
  dataFolderRisk <- file.path(modelPathRisk, "data")
  resultsFolderRisk <- file.path(modelPathRisk, "results")
  
  dir.create(dataFolderRisk, showWarnings = FALSE, recursive = TRUE)
  dir.create(resultsFolderRisk, showWarnings = FALSE, recursive = TRUE)
  
  # Return data folders for writing data
  return(list(dataFolderAll, dataFolderRisk))
  
}

# Function for writing ECDC files------------------------------------------
EcdcWrite <- function(results, folder, category) {
 # Categories: hiv, hivaids, aids, <200, 200-349, 350-499, >500
  
  fileTag <- switch(category,
         "hiv" = "HIV",
         "hivaids" = "HIVAIDS",
         "aids" = "AIDS",
         ">500" = "HIV_CD4_1", 
         "350-499" = "HIV_CD4_2", 
         "200-349" = "HIV_CD4_3", 
         "<200" = "HIV_CD4_4")
  
  fileName <- file.path(folder, paste0(fileTag, ".csv"))
  
  write_csv(results, fileName)
  
  return(fileName)
  
}

# Diagnosis type-----------------------------------------------------------
typeDiag <- function(hivData, type, minYear = 1980,
                     exposure = TRUE, adjustUnique = NULL) {
  
  if (exposure) {
    # Extract notifications
    if (type == "hiv") {
      diagType <- hivData %>%
        filter(typediagnosis %in% c("hiv", "hivaids")) %>%
        group_by(yeardiagnosis,  expgroup) %>%
        summarise(diags = n()) %>%
        ungroup() %>%
        spread(expgroup, diags) %>%
        rename(year = yeardiagnosis)
    } else if (type == "aids") {
      diagType <- hivData %>%
        filter(typediagnosis %in% c("aids", "hivaids")) %>%
        group_by(yeardiagnosis, expgroup) %>%
        summarise(diags = n()) %>%
        ungroup() %>%
        spread(expgroup, diags) %>%
        rename(year = yeardiagnosis)
      
      # allYears <- minYear:max(hivData$yeardiagnosis)
      # requiredYears <- allYears[!(allYears %in% diagType$year)]
      # diagType <- FillDataFrame(requiredYears, diagType)
      # diagType <- arrange(diagType, year)
      # 
      # diagType2 <- hivData %>%
      #   filter(typediagnosis == "hivaids") %>%
      #   group_by(yeardiagnosis,  expgroup) %>%
      #   summarise(diags = n()) %>%
      #   ungroup() %>%
      #   spread(expgroup, diags) %>%
      #   rename(year = yeardiagnosis)
      # 
      # allYears <- minYear:max(hivData$yeardiagnosis)
      # requiredYears <- allYears[!(allYears %in% diagType2$year)]
      # diagType2 <- FillDataFrame(requiredYears, diagType2)
      # diagType2 <- arrange(diagType2, year)
      # 
      # diagType[, 2:6] <- diagType[, 2:6] + diagType2[, 2:6]
      
    } else {
      diagType <- hivData %>%
        filter(typediagnosis == "hivaids") %>%
        group_by(yeardiagnosis,  expgroup) %>%
        summarise(diags = n()) %>%
        ungroup() %>%
        spread(expgroup, diags) %>%
        rename(year = yeardiagnosis)
    } 
    diagType[is.na(diagType)] <- 0
    
    diagType <- diagType %>%
      mutate(known = hetero+msm+otherexp+pwid) %>%
      mutate(hetero = hetero + unknown * hetero/known,
             msm = msm + unknown * msm/known,
             pwid = pwid + unknown * pwid/known,
             otherexp = otherexp + unknown * otherexp/known) %>%
      mutate(hetero = ifelse(is.nan(hetero), 0, hetero), 
             msm = ifelse(is.nan(msm), 0, msm),
             pwid = ifelse(is.nan(pwid), 0, pwid),
             otherexp = ifelse(is.nan(otherexp), 0, otherexp)) %>%
      select(-known, -unknown)
      
    
    # Fill in missing years
    allYears <- minYear:max(hivData$yeardiagnosis)
    requiredYears <- allYears[!(allYears %in% diagType$year)]
    diagType <- FillDataFrame(requiredYears, diagType)
    diagType <- arrange(diagType, year)
    
    if (!is.null(adjustUnique)) {
      diagType$hetero <- AnnualUnique(diagType$hetero , adjustUnique)
      diagType$msm <- AnnualUnique(diagType$msm , adjustUnique)
      diagType$pwid <- AnnualUnique(diagType$pwid , adjustUnique)
      diagType$otherexp <- AnnualUnique(diagType$otherexp , adjustUnique)
    } else {
      warning("Notifications not adjusted for duplicates")
    }
    
  } else {
    # Don't split by exposure group 
    if (type == "hiv") {
      diagType <- hivData %>%
        filter(typediagnosis %in% c("hiv", "hivaids")) %>%
        group_by(yeardiagnosis) %>%
        summarise(all = n()) %>%
        ungroup() %>%
        rename(year = yeardiagnosis)
    } else if (type == "aids") {
      diagType <- hivData %>%
        filter(typediagnosis %in% c("aids", "hivaids")) %>%
        group_by(yeardiagnosis) %>%
        summarise(all = n()) %>%
        ungroup() %>%
        rename(year = yeardiagnosis)
    } else {
      diagType <- hivData %>%
        filter(typediagnosis == "hivaids") %>%
        group_by(yeardiagnosis) %>%
        summarise(all = n()) %>%
        ungroup() %>%
        rename(year = yeardiagnosis)
    } 
    diagType[is.na(diagType)] <- 0
    
    # Fill in missing years
    allYears <- minYear:max(hivData$yeardiagnosis)
    requiredYears <- allYears[!(allYears %in% diagType$year)]
    diagType <- FillDataFrame(requiredYears, diagType)
    diagType <- arrange(diagType, year)
    
    if (!is.null(adjustUnique)) {
      diagType$all <- AnnualUnique(diagType$all, adjustUnique)
    } else {
      warning("Notifications not adjusted for duplicates")
    }
    
  }
  
  return(diagType)
}

# Number in each CD4 count all---------------------------------------------
cd4All <- function(hivData, cd4binGroup, minYear = 1980, useprop = FALSE,
                   adjustUnique = NULL) {
  # First filer out aids cases
  hivData <- hivData %>%
    filter(typediagnosis == "hiv")
  
  # Organize all the data
  cd4DiagsAll <- hivData %>%
    group_by(yeardiagnosis, cd4bin) %>%
    summarise(diags = n()) %>%
    ungroup() %>%
    spread(cd4bin, diags) %>%
    mutate(total = apply(select(., 2:6), 1, sum)) %>%
    rename(year = yeardiagnosis)
  cd4DiagsAll[is.na(cd4DiagsAll)] <- 0
  
  # Fix cd4 string so it can be read by Select
  cd4binSelect <- paste0("`", cd4binGroup, "`")
  
  # Select what we want
  cd4Diags <- cd4DiagsAll %>%
    select_("year", cd4binSelect)
  colnames(cd4Diags)[2] <- c("all")
  
  if (useprop) {
    
    propCD4all <- t(apply(select(cd4DiagsAll, -year, -total, 
                                 -`Not Reported`), 1,
                          function(row) row/sum(row)))
    
    propCD4all[is.nan(propCD4all)] <- 0
    propCD4all <- as_data_frame(propCD4all)
    
    cd4Diags$all <- cd4DiagsAll$total * propCD4all[[cd4binGroup]]
  }
  
  # Fill in missing years
  allYears <- minYear:max(cd4Diags$year)
  requiredYears <- allYears[!(allYears %in% cd4Diags$year)]
  cd4Diags <- FillDataFrame(requiredYears, cd4Diags) %>%
    arrange(year)
  
  if (!is.null(adjustUnique)) {
    cd4Diags$all <- AnnualUnique(cd4Diags$all, adjustUnique)
  }
  
  return(cd4Diags)
  
}

# Number in each CD4 count by exposure------------------------------------
cd4Exposure <- function(hivData, cd4binGroup,
                        minYear = 1980, adjustUnique = NULL) {
  
  # First filer out aids cases
  hivData <- hivData %>%
    filter(typediagnosis == "hiv")
  
  cd4DiagsExp <- hivData %>%
    group_by(yeardiagnosis, cd4bin, expgroup) %>%
    summarise(diags = n()) %>%
    ungroup() %>%
    spread(expgroup, diags) 
  cd4DiagsExp[is.na(cd4DiagsExp)] <- 0  
  
  cd4ExpBin <- cd4DiagsExp %>%
    filter(cd4bin == cd4binGroup) %>%
    # mutate(known = hetero+msm+otherexp+pwid) %>%
    # mutate(hetero = hetero + unknown * hetero/known,
    #        msm = msm + unknown * msm/known,
    #        pwid = pwid + unknown * pwid/known,
    #        otherexp = otherexp + unknown * otherexp/known) %>%
    # mutate(hetero = ifelse(is.nan(hetero), 0, hetero), 
    #        msm = ifelse(is.nan(msm), 0, msm),
    #        pwid = ifelse(is.nan(pwid), 0, pwid),
    #        otherexp = ifelse(is.nan(otherexp), 0, otherexp)) %>%
    # select(-cd4bin, -known, -unknown) %>%
    select(-cd4bin, -unknown) %>%
    rename(year = yeardiagnosis)
  
  # Fill in missing years
  allYears <- minYear:max(cd4ExpBin$year)
  requiredYears <- allYears[!(allYears %in% cd4ExpBin$year)]
  cd4ExpBin <- FillDataFrame(requiredYears, cd4ExpBin) %>%
    arrange(year)
  
  if (!is.null(adjustUnique)) {
    cd4ExpBin$hetero <- AnnualUnique(cd4ExpBin$hetero , adjustUnique)
    cd4ExpBin$msm <- AnnualUnique(cd4ExpBin$msm , adjustUnique)
    cd4ExpBin$pwid <- AnnualUnique(cd4ExpBin$pwid , adjustUnique)
    cd4ExpBin$otherexp <- AnnualUnique(cd4ExpBin$otherexp , adjustUnique)
  }
  
  return(cd4ExpBin)
  
}  
