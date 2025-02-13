## Functions for ECDC HIV Modeling Tool data

# Richard T. Gray

# This script contains a number of functions used to produce the data 
# for the ECDC annual HIV notifications. It is used by the EcdcFiles 
# function.

# Create output folders --------------------------------------------------
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
  
  # All -------------------------------------------------------------------
  modelPathAll <- file.path(outputFolder, "ECDC_models", model, folderAll)
  dir.create(modelPathAll, showWarnings = FALSE, recursive = TRUE)
  
  # Create other directories
  dataFolderAll <- file.path(modelPathAll, "data")
  resultsFolderAll <- file.path(modelPathAll, "results")
  
  dir.create(dataFolderAll, showWarnings = FALSE, recursive = TRUE)
  dir.create(resultsFolderAll, showWarnings = FALSE, recursive = TRUE)
  
  # By at-risk category ---------------------------------------------------
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
    "cg500" = "HIV_CD4_1", 
    "c350_499" = "HIV_CD4_2", 
    "c200_349" = "HIV_CD4_3", 
    "cl200" = "HIV_CD4_4",
    "not_reported_1" = "HIV_CD4_LM_1_0",
    "cl20_1" = "HIV_CD4_LM_1_1",
    "c20_49_1" = "HIV_CD4_LM_1_2",
    "c50_99_1" = "HIV_CD4_LM_1_3",
    "c100_149_1" = "HIV_CD4_LM_1_4",
    "c150_199_1" = "HIV_CD4_LM_1_5",
    "c200_249_1" = "HIV_CD4_LM_1_6",
    "c250_299_1" = "HIV_CD4_LM_1_7",
    "c300_349_1" = "HIV_CD4_LM_1_8",
    "cg350_1" = "HIV_CD4_LM_1_9",
    "not_reported_2" = "HIV_CD4_LM_2_0",
    "cl20_2" = "HIV_CD4_LM_2_1",
    "c20_49_2" = "HIV_CD4_LM_2_2",
    "c50_99_2" = "HIV_CD4_LM_2_3",
    "c100_149_2" = "HIV_CD4_LM_2_4",
    "c150_199_2" = "HIV_CD4_LM_2_5",
    "c200_249_2" = "HIV_CD4_LM_2_6",
    "c250_299_2" = "HIV_CD4_LM_2_7",
    "c300_349_2" = "HIV_CD4_LM_2_8",
    "cg350_2" = "HIV_CD4_LM_2_9",
    "deaths" = "Dead",
    "emigrants" = "Emig")
  
  fileName <- file.path(folder, paste0(fileTag, ".csv"))
  
  write_csv(results, fileName)
  
  return(fileName)
  
}

# Diagnosis type-----------------------------------------------------------
typeDiag <- function(hivData, type, minYear = 1980,
  exposure = TRUE, adjustUnique = NULL,
  normalize = NULL, dataSets = 1) {
  
  if (exposure) {
    # Extract notifications
    if (type == "hiv") {
      diagType <- hivData %>%
        group_by(yeardiagnosis,  expgroup) %>%
        summarise(diags = n() / dataSets) %>%
        ungroup() %>%
        spread(expgroup, diags) %>%
        rename(year = yeardiagnosis)
    } else if (type == "aids") {
      diagType <- hivData %>%
        filter(typediagnosis %in% c("aids", "hivaids")) %>%
        # filter(typediagnosis %in% c("hivaids")) %>%
        group_by(yearaids, expgroup) %>%
        summarise(diags = n() / dataSets) %>%
        ungroup() %>%
        spread(expgroup, diags) %>%
        rename(year = yearaids) %>%
        filter(year >= minYear) # remove 1956 and 1963 AIDS cases about 4 in each set. 
      
    } else {
      diagType <- hivData %>%
        filter(typediagnosis == "hivaids") %>%
        group_by(yeardiagnosis,  expgroup) %>%
        summarise(diags = n() / dataSets) %>%
        ungroup() %>%
        spread(expgroup, diags) %>%
        rename(year = yeardiagnosis)
    } 
    
    diagType[is.na(diagType)] <- 0
    
    if (!("hetero" %in% names(diagType))) diagType$hetero <- 0
    if (!("msm" %in% names(diagType))) diagType$msm <- 0
    if (!("otherexp" %in% names(diagType))) diagType$otherexp <- 0
    if (!("pwid" %in% names(diagType))) diagType$pwid <- 0
    if (!("unknown" %in% names(diagType))) diagType$unknown <- 0

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
    
    allYears <- minYear:max(hivData$yeardiagnosis)
    # requiredYears <- allYears[!(allYears %in% diagType$year)]
    diagType <- FillDataFrame(allYears, diagType)
    # diagType <- arrange(diagType, year)
    
    if (!is.null(adjustUnique)) {
      diagType$hetero <- diagType$hetero * adjustUnique[1:length(diagType$hetero)]
      diagType$msm <- diagType$msm * adjustUnique[1:length(diagType$msm)]
      diagType$pwid <- diagType$pwid * adjustUnique[1:length(diagType$pwid)]
      diagType$otherexp <- diagType$otherexp * adjustUnique[1:length(diagType$otherexp)]
    } else {
      warning("Notifications not adjusted for duplicates")
    }
    
    # Replace any negatives with zero
    diagType[diagType < 0] <- 0
    
    # Adjust HIV notifications to account for unknowns
    # Assume AIDS cases are complete
    
    if (!is.null(normalize) && (type == "hiv")) {
      diagType$hetero <- diagType$hetero/normalize[1:length(diagType$hetero)]
      diagType$msm <- diagType$msm/normalize[1:length(diagType$msm)]
      diagType$pwid <- diagType$pwid/normalize[1:length(diagType$pwid)]
      diagType$otherexp <- diagType$otherexp/normalize[1:length(diagType$otherexp)]
    } 
    
    if (is.null(normalize)) {
      warning("HIV notifications not adjusted for unknowns")
    }  
    
    
  } else {
    # Don't split by exposure group 
    if (type == "hiv") {
      diagType <- hivData %>%
        group_by(yeardiagnosis) %>%
        summarise(all = n() / dataSets) %>%
        ungroup() %>%
        rename(year = yeardiagnosis)
    } else if (type == "aids") {
      diagType <- hivData %>%
        filter(typediagnosis %in% c("aids", "hivaids")) %>%
        group_by(yearaids) %>%
        summarise(all = n() / dataSets) %>%
        ungroup() %>%
        rename(year = yearaids) %>%
        filter(year != 1956) # remove 1956 AIDS cases about 23.
      
    } else {
      diagType <- hivData %>%
        filter(typediagnosis == "hivaids") %>%
        group_by(yeardiagnosis) %>%
        summarise(all = n() / dataSets) %>%
        ungroup() %>%
        rename(year = yeardiagnosis)
      
    } 
    
    diagType[is.na(diagType)] <- 0
    
    diagType <- diagType %>%
      mutate(all = ifelse(is.nan(all), 0, all))
    
    # Fill in missing years
    allYears <- minYear:max(hivData$yeardiagnosis)
    # requiredYears <- allYears[!(allYears %in% diagType$year)]
    diagType <- FillDataFrame(allYears, diagType)
    # diagType <- arrange(diagType, year)
    
    if (!is.null(adjustUnique)) {
      diagType$all <- diagType$all * adjustUnique[1:length(diagType$all)]
      
    } else {
      warning("Notifications not adjusted for duplicates")
    }
    
    # Replace any negatives with zero
    diagType[diagType < 0] <- 0
    
    # Adjust HIV notifications to account for unknowns
    # Assume AIDS cases are complete
    if (!is.null(normalize) && (type == "hiv")) {
      diagType$all <- diagType$all/normalize[1:length(diagType$all)]
    }
    
    if (is.null(normalize)) {
      warning("HIV notifications not adjusted for unknowns")
    }  
  }
  
  return(diagType)
}

# Number in each CD4 count all---------------------------------------------
cd4All <- function(hivData, cd4binGroup, minYear = 1980, useprop = FALSE,
  adjustUnique = NULL, london = FALSE, lmset = FALSE, dataSets = 1) {
  
  # Organize all the data
  if (london) {
    
    if (lmset) {
      # HIV/AIDS only
      hivData <- hivData %>%
        filter(typediagnosis %in% c("hivaids", "aids"))
    } else {
      # HIV/AIDS + those with HIV symptoms
      hivData <- hivData %>%
        filter(typediagnosis %in% c("hivaids", "aids") |
            !is.na(dateill))
    }
    
    cd4DiagsAll <- hivData %>%
      group_by(yeardiagnosis, cd4London) %>%
      summarise(diags = n() / dataSets) %>%
      ungroup() %>%
      spread(cd4London, diags)
    
    if (!("cl20" %in% names(cd4DiagsAll))) cd4DiagsAll$cl20 <- 0
    if (!("c20_49" %in% names(cd4DiagsAll))) cd4DiagsAll$c20_49 <- 0
    if (!("c50_99" %in% names(cd4DiagsAll))) cd4DiagsAll$c50_99 <- 0
    if (!("c100_149" %in% names(cd4DiagsAll))) cd4DiagsAll$c100_149 <- 0
    if (!("c150_199" %in% names(cd4DiagsAll))) cd4DiagsAll$c150_199 <- 0
    if (!("c200_249" %in% names(cd4DiagsAll))) cd4DiagsAll$c200_249 <- 0
    if (!("c250_299" %in% names(cd4DiagsAll))) cd4DiagsAll$c250_299 <- 0
    if (!("c300_349" %in% names(cd4DiagsAll))) cd4DiagsAll$c300_349 <- 0
    if (!("cg350" %in% names(cd4DiagsAll))) cd4DiagsAll$cg350 <- 0
    if (!("not_reported" %in% names(cd4DiagsAll))) cd4DiagsAll$not_reported <- 0
    
    cd4DiagsAll <- cd4DiagsAll %>% 
      mutate(total = apply(select(., 2:10), 1, sum)) %>%
      rename(year = yeardiagnosis)
  } else {
    # First filter out concurrent aids cases
    hivData <- hivData %>%
      filter(typediagnosis %in% c("hiv", "aids"))
    
    cd4DiagsAll <- hivData %>%
      group_by(yeardiagnosis, cd4bin) %>%
      summarise(diags = n() / dataSets) %>%
      ungroup() %>%
      spread(cd4bin, diags) %>%
      mutate(total = apply(select(., 2:6), 1, sum)) %>%
      rename(year = yeardiagnosis)
  }
  cd4DiagsAll[is.na(cd4DiagsAll)] <- 0
  
  # Fix cd4 string so it can be read by Select
  cd4binSelect <- cd4binGroup # paste0("`", cd4binGroup, "`")
  
  # add missing columns
  if (!(cd4binSelect %in% names(cd4DiagsAll))){
    cd4DiagsAll$cd4binSelect <- 0
    names(cd4DiagsAll)[ncol(cd4DiagsAll)] <- cd4binSelect
  } 
  
  # Select what we want
  cd4Diags <- cd4DiagsAll %>%
    select("year", all_of(cd4binSelect))
  colnames(cd4Diags)[2] <- c("all")
  
  if (useprop) {
    
    propCD4all <- t(apply(select(cd4DiagsAll, -year, -total, 
      -not_reported), 1, function(row) row/sum(row)))
    
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
    # cd4Diags$all <- AnnualUnique(cd4Diags$all, adjustUnique)
    cd4Diags$all <- cd4Diags$all * adjustUnique
  }
  
  #remove rows with all NAs
  cd4Diags <- cd4Diags[rowSums(is.na(cd4Diags)) != ncol(cd4Diags),]  #remove NA rows
  
  return(cd4Diags)
  
}

# Number in each CD4 count by exposure------------------------------------
cd4Exposure <- function(hivData, cd4binGroup,
  minYear = 1980, adjustUnique = NULL, london = FALSE, lmset = FALSE,
  dataSets = 1) {
  
  # Organize the data
  if (london) {
    
    if (lmset) {
      # First filter hiv/aids
      hivData <- hivData %>%
        filter(typediagnosis %in% c("hivaids", "aids"))
    } else {
      # HIV/AIDS + those with HIV symptoms
      hivData <- hivData %>%
        filter(typediagnosis %in% c("hivaids", "aids") |
            !is.na(dateill))
    }
    
    cd4DiagsExp <- hivData %>%
      group_by(yeardiagnosis, cd4London, expgroup) %>%
      summarise(diags = n() / dataSets) %>%
      ungroup() %>%
      spread(expgroup, diags)
    
    cd4DiagsExp[is.na(cd4DiagsExp)] <- 0 
    
    if (!("hetero" %in% names(cd4DiagsExp))) cd4DiagsExp$hetero <- 0
    if (!("msm" %in% names(cd4DiagsExp))) cd4DiagsExp$msm <- 0
    if (!("otherexp" %in% names(cd4DiagsExp))) cd4DiagsExp$otherexp <- 0
    if (!("pwid" %in% names(cd4DiagsExp))) cd4DiagsExp$pwid <- 0
    if (!("unknown" %in% names(cd4DiagsExp))) cd4DiagsExp$unknown <- 0
    
    cd4ExpBin <- cd4DiagsExp %>%
      filter(cd4London == cd4binGroup) %>%
      select(-cd4London, -unknown) %>%
      rename(year = yeardiagnosis)
    
  } else {
    # First filter out concurrent aids cases
    hivData <- hivData %>%
      filter(typediagnosis %in% c("hiv", "aids"))
    
    cd4DiagsExp <- hivData %>%
      group_by(yeardiagnosis, cd4bin, expgroup) %>%
      summarise(diags = n() / dataSets) %>%
      ungroup() %>%
      spread(expgroup, diags)
    
    cd4DiagsExp[is.na(cd4DiagsExp)] <- 0  
    
    if (!("hetero" %in% names(cd4DiagsExp))) cd4DiagsExp$hetero <- 0
    if (!("msm" %in% names(cd4DiagsExp))) cd4DiagsExp$msm <- 0
    if (!("otherexp" %in% names(cd4DiagsExp))) cd4DiagsExp$otherexp <- 0
    if (!("pwid" %in% names(cd4DiagsExp))) cd4DiagsExp$pwid <- 0
    if (!("unknown" %in% names(cd4DiagsExp))) cd4DiagsExp$unknown <- 0
    
    cd4ExpBin <- cd4DiagsExp %>%
      filter(cd4bin == cd4binGroup) %>%
      select(-cd4bin, -unknown) %>%
      rename(year = yeardiagnosis)
    
  }
  
  #remove rows with NAs
  cd4ExpBin <- cd4ExpBin[!apply(is.na(cd4ExpBin), 1, all),]
  
  # Fill in missing years
  if(nrow(cd4ExpBin)!=0){
    allYears <- minYear:max(cd4ExpBin$year)
    requiredYears <- allYears[!(allYears %in% cd4ExpBin$year)]
    cd4ExpBin <- FillDataFrame(requiredYears, cd4ExpBin) %>%
      arrange(year)
  } else { 
    cd4ExpBin <- tibble(year = minYear,
      hetero = 0,
      msm = 0,
      otherexp = 0,
      pwid = 0)
  }
  
  if (!is.null(adjustUnique)) {
    cd4ExpBin$hetero <- cd4ExpBin$hetero * adjustUnique
    cd4ExpBin$msm <- cd4ExpBin$msm * adjustUnique
    cd4ExpBin$pwid <- cd4ExpBin$pwid * adjustUnique
    cd4ExpBin$otherexp <- cd4ExpBin$otherexp * adjustUnique
  }
  
  #remove rows with all NAs
  cd4ExpBin <- cd4ExpBin[rowSums(is.na(cd4ExpBin)) != ncol(cd4ExpBin),]
  
  return(cd4ExpBin)
  
}  
