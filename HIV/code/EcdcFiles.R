## EcdcFIles.R

# Richard T. Gray

# This functions produces all the ECDC model data files required for 
# the proportion of PLHIV undiagnosed. It relies on functions from
# EcdcCalculations to be loaded. 

EcdcFiles <- function(hivFrame, dataFolder) {
  
  # All diagnoses----------------------------------------------------------
  
  # Incidence method
  hivcsvAll <- typeDiag(hivFrame, "hiv", exposure = FALSE, 
                        adjustUnique = overallPropUnique)
  hivaidscsvAll <- typeDiag(hivFrame, "hivaids", exposure = FALSE, 
                            adjustUnique = overallPropUnique)
  aidscsvAll <- typeDiag(hivFrame, "aids", exposure = FALSE, 
                         adjustUnique = overallPropUnique)
  
  EcdcWrite(hivcsvAll, dataFolder[[1]], "hiv")
  EcdcWrite(hivaidscsvAll, dataFolder[[1]], "hivaids")
  EcdcWrite(aidscsvAll, dataFolder[[1]], "aids")
  
  cd4All1 <- cd4All(hivFrame, ">500")
  cd4All2 <- cd4All(hivFrame, "350-499")
  cd4All3 <- cd4All(hivFrame, "200-349")
  cd4All4 <- cd4All(hivFrame, "<200")
  
  EcdcWrite(cd4All1, dataFolder[[1]], ">500")
  EcdcWrite(cd4All2, dataFolder[[1]], "350-499")
  EcdcWrite(cd4All3, dataFolder[[1]], "200-349")
  EcdcWrite(cd4All4, dataFolder[[1]], "<200")
  
  # London method
  cd4LM10 <- cd4All(hivFrame, "Not Reported", london = TRUE, lmset = TRUE)
  cd4LM11 <- cd4All(hivFrame, "<20", london = TRUE, lmset = TRUE)
  cd4LM12 <- cd4All(hivFrame, "20-49", london = TRUE, lmset = TRUE)
  cd4LM13 <- cd4All(hivFrame, "50-99", london = TRUE, lmset = TRUE)
  cd4LM14 <- cd4All(hivFrame, "100-149", london = TRUE, lmset = TRUE)
  cd4LM15 <- cd4All(hivFrame, "150-199", london = TRUE, lmset = TRUE)
  cd4LM16 <- cd4All(hivFrame, "200-249", london = TRUE, lmset = TRUE)
  cd4LM17 <- cd4All(hivFrame, "250-299", london = TRUE, lmset = TRUE)
  cd4LM18 <- cd4All(hivFrame, "300-349", london = TRUE, lmset = TRUE)
  cd4LM19 <- cd4All(hivFrame, ">350", london = TRUE, lmset = TRUE)
  
  EcdcWrite(cd4LM10, dataFolder[[1]], "Not Reported_1")
  EcdcWrite(cd4LM11, dataFolder[[1]], "<20_1")
  EcdcWrite(cd4LM12, dataFolder[[1]], "20-49_1")
  EcdcWrite(cd4LM13, dataFolder[[1]], "50-99_1")
  EcdcWrite(cd4LM14, dataFolder[[1]], "100-149_1")
  EcdcWrite(cd4LM15, dataFolder[[1]], "150-199_1")
  EcdcWrite(cd4LM16, dataFolder[[1]], "200-249_1")
  EcdcWrite(cd4LM17, dataFolder[[1]], "250-299_1")
  EcdcWrite(cd4LM18, dataFolder[[1]], "300-349_1")
  EcdcWrite(cd4LM19, dataFolder[[1]], ">350_1")
  
  cd4LM20 <- cd4All(hivFrame, "Not Reported", london = TRUE)
  cd4LM21 <- cd4All(hivFrame, "<20", london = TRUE)
  cd4LM22 <- cd4All(hivFrame, "20-49", london = TRUE)
  cd4LM23 <- cd4All(hivFrame, "50-99", london = TRUE)
  cd4LM24 <- cd4All(hivFrame, "100-149", london = TRUE)
  cd4LM25 <- cd4All(hivFrame, "150-199", london = TRUE)
  cd4LM26 <- cd4All(hivFrame, "200-249", london = TRUE)
  cd4LM27 <- cd4All(hivFrame, "250-299", london = TRUE)
  cd4LM28 <- cd4All(hivFrame, "300-349", london = TRUE)
  cd4LM29 <- cd4All(hivFrame, ">350", london = TRUE)
  
  EcdcWrite(cd4LM20, dataFolder[[1]], "Not Reported_2")
  EcdcWrite(cd4LM21, dataFolder[[1]], "<20_2")
  EcdcWrite(cd4LM22, dataFolder[[1]], "20-49_2")
  EcdcWrite(cd4LM23, dataFolder[[1]], "50-99_2")
  EcdcWrite(cd4LM24, dataFolder[[1]], "100-149_2")
  EcdcWrite(cd4LM25, dataFolder[[1]], "150-199_2")
  EcdcWrite(cd4LM26, dataFolder[[1]], "200-249_2")
  EcdcWrite(cd4LM27, dataFolder[[1]], "250-299_2")
  EcdcWrite(cd4LM28, dataFolder[[1]], "300-349_2")
  EcdcWrite(cd4LM29, dataFolder[[1]], ">350_2")
  
  # Risk groups -----------------------------------------------------------
  
  # Incidence method
  hivcsvExp <- typeDiag(hivFrame, "hiv",
                        adjustUnique = overallPropUnique)
  hivaidscsvExp <- typeDiag(hivFrame, "hivaids", 
                            adjustUnique = overallPropUnique)
  aidscsvExp <- typeDiag(hivFrame, "aids",
                         adjustUnique = overallPropUnique)
  
  EcdcWrite(hivcsvExp, dataFolder[[2]], "hiv")
  EcdcWrite(hivaidscsvExp, dataFolder[[2]], "hivaids")
  EcdcWrite(aidscsvExp, dataFolder[[2]], "aids")
  
  cd4Exp1 <- cd4Exposure(hivFrame, ">500")
  cd4Exp2 <- cd4Exposure(hivFrame, "350-499")
  cd4Exp3 <- cd4Exposure(hivFrame, "200-349")
  cd4Exp4 <- cd4Exposure(hivFrame, "<200")
  
  EcdcWrite(cd4Exp1, dataFolder[[2]], ">500")
  EcdcWrite(cd4Exp2, dataFolder[[2]], "350-499")
  EcdcWrite(cd4Exp3, dataFolder[[2]], "200-349")
  EcdcWrite(cd4Exp4, dataFolder[[2]], "<200")
  
  # London method
  cd4LMexp10 <- cd4Exposure(hivFrame, "Not Reported", london = TRUE, 
                            lmset = TRUE)
  cd4LMexp11 <- cd4Exposure(hivFrame, "<20", london = TRUE, lmset = TRUE)
  cd4LMexp12 <- cd4Exposure(hivFrame, "20-49", london = TRUE, 
                            lmset = TRUE)
  cd4LMexp13 <- cd4Exposure(hivFrame, "50-99", london = TRUE, 
                            lmset = TRUE)
  cd4LMexp14 <- cd4Exposure(hivFrame, "100-149", london = TRUE, 
                            lmset = TRUE)
  cd4LMexp15 <- cd4Exposure(hivFrame, "150-199", london = TRUE, 
                            lmset = TRUE)
  cd4LMexp16 <- cd4Exposure(hivFrame, "200-249", london = TRUE, 
                            lmset = TRUE)
  cd4LMexp17 <- cd4Exposure(hivFrame, "250-299", london = TRUE, 
                            lmset = TRUE)
  cd4LMexp18 <- cd4Exposure(hivFrame, "300-349", london = TRUE, 
                            lmset = TRUE)
  cd4LMexp19 <- cd4Exposure(hivFrame, ">350", london = TRUE, 
                            lmset = TRUE)
  
  EcdcWrite(cd4LMexp10, dataFolder[[2]], "Not Reported_1")
  EcdcWrite(cd4LMexp11, dataFolder[[2]], "<20_1")
  EcdcWrite(cd4LMexp12, dataFolder[[2]], "20-49_1")
  EcdcWrite(cd4LMexp13, dataFolder[[2]], "50-99_1")
  EcdcWrite(cd4LMexp14, dataFolder[[2]], "100-149_1")
  EcdcWrite(cd4LMexp15, dataFolder[[2]], "150-199_1")
  EcdcWrite(cd4LMexp16, dataFolder[[2]], "200-249_1")
  EcdcWrite(cd4LMexp17, dataFolder[[2]], "250-299_1")
  EcdcWrite(cd4LMexp18, dataFolder[[2]], "300-349_1")
  EcdcWrite(cd4LMexp19, dataFolder[[2]], ">350_1")
  
  cd4LMexp20 <- cd4Exposure(hivFrame, "Not Reported", london = TRUE)
  cd4LMexp21 <- cd4Exposure(hivFrame, "<20", london = TRUE)
  cd4LMexp22 <- cd4Exposure(hivFrame, "20-49", london = TRUE)
  cd4LMexp23 <- cd4Exposure(hivFrame, "50-99", london = TRUE)
  cd4LMexp24 <- cd4Exposure(hivFrame, "100-149", london = TRUE)
  cd4LMexp25 <- cd4Exposure(hivFrame, "150-199", london = TRUE)
  cd4LMexp26 <- cd4Exposure(hivFrame, "200-249", london = TRUE)
  cd4LMexp27 <- cd4Exposure(hivFrame, "250-299", london = TRUE)
  cd4LMexp28 <- cd4Exposure(hivFrame, "300-349", london = TRUE)
  cd4LMexp29 <- cd4Exposure(hivFrame, ">350", london = TRUE)
  
  EcdcWrite(cd4LMexp20, dataFolder[[2]], "Not Reported_2")
  EcdcWrite(cd4LMexp21, dataFolder[[2]], "<20_2")
  EcdcWrite(cd4LMexp22, dataFolder[[2]], "20-49_2")
  EcdcWrite(cd4LMexp23, dataFolder[[2]], "50-99_2")
  EcdcWrite(cd4LMexp24, dataFolder[[2]], "100-149_2")
  EcdcWrite(cd4LMexp25, dataFolder[[2]], "150-199_2")
  EcdcWrite(cd4LMexp26, dataFolder[[2]], "200-249_2")
  EcdcWrite(cd4LMexp27, dataFolder[[2]], "250-299_2")
  EcdcWrite(cd4LMexp28, dataFolder[[2]], "300-349_2")
  EcdcWrite(cd4LMexp29, dataFolder[[2]], ">350_2")
  
  # Don't need explicit output
  return(NULL)
  
}
