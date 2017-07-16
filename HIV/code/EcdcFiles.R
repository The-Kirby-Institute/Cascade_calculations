## EcdcFIles.R

# Richard T. Gray

# This functions produces all the ECDC model data files required for 
# the proportion of PLHIV undiagnosed. It relies on functions from
# EcdcCalculations to be loaded. 

EcdcFiles <- function(hivFrame, dataFolder, propUnique = NULL) {
  
  # All diagnoses----------------------------------------------------------
  
  # Incidence method
  hivcsvAll <- typeDiag(hivFrame, "hiv", exposure = FALSE, 
                        adjustUnique = propUnique)
  hivaidscsvAll <- typeDiag(hivFrame, "hivaids", exposure = FALSE, 
                            adjustUnique = propUnique)
  aidscsvAll <- typeDiag(hivFrame, "aids", exposure = FALSE, 
                         adjustUnique = propUnique)
  
  EcdcWrite(hivcsvAll, dataFolder[[1]], "hiv")
  EcdcWrite(hivaidscsvAll, dataFolder[[1]], "hivaids")
  EcdcWrite(aidscsvAll, dataFolder[[1]], "aids")
  
  cd4All1 <- cd4All(hivFrame, "cg500")
  cd4All2 <- cd4All(hivFrame, "c350_499")
  cd4All3 <- cd4All(hivFrame, "c200_349")
  cd4All4 <- cd4All(hivFrame, "cl200")
  
  EcdcWrite(cd4All1, dataFolder[[1]], "cg500")
  EcdcWrite(cd4All2, dataFolder[[1]], "c350_499")
  EcdcWrite(cd4All3, dataFolder[[1]], "c200_349")
  EcdcWrite(cd4All4, dataFolder[[1]], "cl200")
  
  # London method
  cd4LM10 <- cd4All(hivFrame, "not_reported", london = TRUE, lmset = TRUE)
  cd4LM11 <- cd4All(hivFrame, "cl20", london = TRUE, lmset = TRUE)
  cd4LM12 <- cd4All(hivFrame, "c20_49", london = TRUE, lmset = TRUE)
  cd4LM13 <- cd4All(hivFrame, "c50_99", london = TRUE, lmset = TRUE)
  cd4LM14 <- cd4All(hivFrame, "c100_149", london = TRUE, lmset = TRUE)
  cd4LM15 <- cd4All(hivFrame, "c150_199", london = TRUE, lmset = TRUE)
  cd4LM16 <- cd4All(hivFrame, "c200_249", london = TRUE, lmset = TRUE)
  cd4LM17 <- cd4All(hivFrame, "c250_299", london = TRUE, lmset = TRUE)
  cd4LM18 <- cd4All(hivFrame, "c300_349", london = TRUE, lmset = TRUE)
  cd4LM19 <- cd4All(hivFrame, "cg350", london = TRUE, lmset = TRUE)
  
  EcdcWrite(cd4LM10, dataFolder[[1]], "not_reported_1")
  EcdcWrite(cd4LM11, dataFolder[[1]], "cl20_1")
  EcdcWrite(cd4LM12, dataFolder[[1]], "c20_49_1")
  EcdcWrite(cd4LM13, dataFolder[[1]], "c50_99_1")
  EcdcWrite(cd4LM14, dataFolder[[1]], "c100_149_1")
  EcdcWrite(cd4LM15, dataFolder[[1]], "c150_199_1")
  EcdcWrite(cd4LM16, dataFolder[[1]], "c200_249_1")
  EcdcWrite(cd4LM17, dataFolder[[1]], "c250_299_1")
  EcdcWrite(cd4LM18, dataFolder[[1]], "c300_349_1")
  EcdcWrite(cd4LM19, dataFolder[[1]], "cg350_1")
  
  cd4LM20 <- cd4All(hivFrame, "not_reported", london = TRUE)
  cd4LM21 <- cd4All(hivFrame, "cl20", london = TRUE)
  cd4LM22 <- cd4All(hivFrame, "c20_49", london = TRUE)
  cd4LM23 <- cd4All(hivFrame, "c50_99", london = TRUE)
  cd4LM24 <- cd4All(hivFrame, "c100_149", london = TRUE)
  cd4LM25 <- cd4All(hivFrame, "c150_199", london = TRUE)
  cd4LM26 <- cd4All(hivFrame, "c200_249", london = TRUE)
  cd4LM27 <- cd4All(hivFrame, "c250_299", london = TRUE)
  cd4LM28 <- cd4All(hivFrame, "c300_349", london = TRUE)
  cd4LM29 <- cd4All(hivFrame, "cg350", london = TRUE)
  
  EcdcWrite(cd4LM20, dataFolder[[1]], "not_reported_2")
  EcdcWrite(cd4LM21, dataFolder[[1]], "cl20_2")
  EcdcWrite(cd4LM22, dataFolder[[1]], "c20_49_2")
  EcdcWrite(cd4LM23, dataFolder[[1]], "c50_99_2")
  EcdcWrite(cd4LM24, dataFolder[[1]], "c100_149_2")
  EcdcWrite(cd4LM25, dataFolder[[1]], "c150_199_2")
  EcdcWrite(cd4LM26, dataFolder[[1]], "c200_249_2")
  EcdcWrite(cd4LM27, dataFolder[[1]], "c250_299_2")
  EcdcWrite(cd4LM28, dataFolder[[1]], "c300_349_2")
  EcdcWrite(cd4LM29, dataFolder[[1]], "cg350_2")
  
  # Risk groups -----------------------------------------------------------
  
  # Incidence method
  hivcsvExp <- typeDiag(hivFrame, "hiv",
                        adjustUnique = propUnique)
  hivaidscsvExp <- typeDiag(hivFrame, "hivaids", 
                            adjustUnique = propUnique)
  aidscsvExp <- typeDiag(hivFrame, "aids",
                         adjustUnique = propUnique)
  
  EcdcWrite(hivcsvExp, dataFolder[[2]], "hiv")
  EcdcWrite(hivaidscsvExp, dataFolder[[2]], "hivaids")
  EcdcWrite(aidscsvExp, dataFolder[[2]], "aids")
  
  cd4Exp1 <- cd4Exposure(hivFrame, "cg500")
  cd4Exp2 <- cd4Exposure(hivFrame, "c350_499")
  cd4Exp3 <- cd4Exposure(hivFrame, "c200_349")
  cd4Exp4 <- cd4Exposure(hivFrame, "cl200")
  
  EcdcWrite(cd4Exp1, dataFolder[[2]], "cg500")
  EcdcWrite(cd4Exp2, dataFolder[[2]], "c350_499")
  EcdcWrite(cd4Exp3, dataFolder[[2]], "c200_349")
  EcdcWrite(cd4Exp4, dataFolder[[2]], "cl200")
  
  # London method
  cd4LMexp10 <- cd4Exposure(hivFrame, "not_reported", london = TRUE, 
                            lmset = TRUE)
  cd4LMexp11 <- cd4Exposure(hivFrame, "cl20", london = TRUE, lmset = TRUE)
  cd4LMexp12 <- cd4Exposure(hivFrame, "c20_49", london = TRUE, 
                            lmset = TRUE)
  cd4LMexp13 <- cd4Exposure(hivFrame, "c50_99", london = TRUE, 
                            lmset = TRUE)
  cd4LMexp14 <- cd4Exposure(hivFrame, "c100_149", london = TRUE, 
                            lmset = TRUE)
  cd4LMexp15 <- cd4Exposure(hivFrame, "c150_199", london = TRUE, 
                            lmset = TRUE)
  cd4LMexp16 <- cd4Exposure(hivFrame, "c200_249", london = TRUE, 
                            lmset = TRUE)
  cd4LMexp17 <- cd4Exposure(hivFrame, "c250_299", london = TRUE, 
                            lmset = TRUE)
  cd4LMexp18 <- cd4Exposure(hivFrame, "c300_349", london = TRUE, 
                            lmset = TRUE)
  cd4LMexp19 <- cd4Exposure(hivFrame, "cg350", london = TRUE, 
                            lmset = TRUE)
  
  EcdcWrite(cd4LMexp10, dataFolder[[2]], "not_reported_1")
  EcdcWrite(cd4LMexp11, dataFolder[[2]], "cl20_1")
  EcdcWrite(cd4LMexp12, dataFolder[[2]], "c20_49_1")
  EcdcWrite(cd4LMexp13, dataFolder[[2]], "c50_99_1")
  EcdcWrite(cd4LMexp14, dataFolder[[2]], "c100_149_1")
  EcdcWrite(cd4LMexp15, dataFolder[[2]], "c150_199_1")
  EcdcWrite(cd4LMexp16, dataFolder[[2]], "c200_249_1")
  EcdcWrite(cd4LMexp17, dataFolder[[2]], "c250_299_1")
  EcdcWrite(cd4LMexp18, dataFolder[[2]], "c300_349_1")
  EcdcWrite(cd4LMexp19, dataFolder[[2]], "cg350_1")
  
  cd4LMexp20 <- cd4Exposure(hivFrame, "not_reported", london = TRUE)
  cd4LMexp21 <- cd4Exposure(hivFrame, "cl20", london = TRUE)
  cd4LMexp22 <- cd4Exposure(hivFrame, "c20_49", london = TRUE)
  cd4LMexp23 <- cd4Exposure(hivFrame, "c50_99", london = TRUE)
  cd4LMexp24 <- cd4Exposure(hivFrame, "c100_149", london = TRUE)
  cd4LMexp25 <- cd4Exposure(hivFrame, "c150_199", london = TRUE)
  cd4LMexp26 <- cd4Exposure(hivFrame, "c200_249", london = TRUE)
  cd4LMexp27 <- cd4Exposure(hivFrame, "c250_299", london = TRUE)
  cd4LMexp28 <- cd4Exposure(hivFrame, "c300_349", london = TRUE)
  cd4LMexp29 <- cd4Exposure(hivFrame, "cg350", london = TRUE)
  
  EcdcWrite(cd4LMexp20, dataFolder[[2]], "not_reported_2")
  EcdcWrite(cd4LMexp21, dataFolder[[2]], "cl20_2")
  EcdcWrite(cd4LMexp22, dataFolder[[2]], "c20_49_2")
  EcdcWrite(cd4LMexp23, dataFolder[[2]], "c50_99_2")
  EcdcWrite(cd4LMexp24, dataFolder[[2]], "c100_149_2")
  EcdcWrite(cd4LMexp25, dataFolder[[2]], "c150_199_2")
  EcdcWrite(cd4LMexp26, dataFolder[[2]], "c200_249_2")
  EcdcWrite(cd4LMexp27, dataFolder[[2]], "c250_299_2")
  EcdcWrite(cd4LMexp28, dataFolder[[2]], "c300_349_2")
  EcdcWrite(cd4LMexp29, dataFolder[[2]], "cg350_2")
  
  # Don't need explicit output
  return(NULL)
  
}
