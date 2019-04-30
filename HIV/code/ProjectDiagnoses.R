#' Set-up vector of future diagnoes
#' 
#' This function projects the future number of diagnoses of HIV into the 
#' future so that projected estimates for the number of PLDHIV can be 
#' estimated.
#' 
#' @param
#' @param 
#' 
#' @return A list containing a vector and optional matrix for number of
#' diagnoses overall and by age for each year  
#' 
#' @author Richard T Gray, \email{Rgray@kirby.unsw.edu.au}
#' 
#' @export
#'  
ProjectDiagnoses <- function(diagnoses, diagyears, decrease, projyears, 
  option, diagnosesAge = NULL) {
  
  # Set-up defaults
  doAge <- FALSE
  
  if (!is.null(diagnosesAge)) {
    doAge <- TRUE 
    nages <- nrow(diagnosesAge)
  }
  
  nprojYears <- length(projyears)
  totalYears <- c(diagyears, projyears)
  finalyear <- End(diagyears)
  
  # Setup Projection relative decrease (linear over projectYears)
  projectDecreaseFuture <- seq(1, decrease, 
    length = (nprojYears+1))
  projectDecreaseAll <- c(rep(1, length(diagyears)), 
    tail(projectDecreaseFuture, nprojYears))
  
  # Default vectors for projections assume everything stays the same.
  # Keeping the number of diagnoses and annUnique the same as the 
  # analysis year value means the same number of unique diagnoses will
  # occur each year. SELECT STANDARD OPTION OR MANUALLY CHANGE TO RUN
  # ALTERNATE SCENARIOS.
  
  # Diagnoses options for overall calculations - if a state or region 
  # need overall as well
  if (option == "status-quo") {
    diagnosesFuture <- ProjVec(diagnoses, nprojYears)
  } else if (option == "linear") {
    # Trend diagnoses (using trend over last 5 years)
    trendlm <- lm(diagnoses ~ year, 
      data.frame(year = (finalyear - 4):finalyear, 
        diagnoses = tail(diagnoses, 5)))
    trendDiags <- predict(trendlm, data.frame(year = projyears))
    diagnosesFuture <- c(diagnoses, trendDiags)
  } else if(option == "reduce") {
    diagnosesFuture <- projectDecreaseAll * 
      ProjVec(diagnoses, nprojYears)
  } else {
    # MANUALLY CHANGE HERE TO RUN ALTERNATE SCENARIOS
  }
  
  diagnosesAgeFuture <- NULL # initialize
  
  if (doAge) {
    
    # Setup hivResultsAge for future. Most are the same as for the 
    # non-age calculations except diagnoses, relAgeDeath, and 
    # relAgeMigrate need to be extended into the future. 
    
    # Status quo diagnoses and relative reductions can be applied across
    # the age groups
    
    if (option == "status-quo") {
      diagnosesAgeFuture <- cbind(diagnosesAge,
        matrix(rep(diagnosesAge[,ncol(diagnosesAge-1)], nprojYears), 
          ncol = nprojYears))
      colnames(diagnosesAgeFuture) <- paste0("y", 
        as.character(totalYears))
    } else if (option == "linear") {
      # Project each age group independently
      diagnosesAgeFuture <- cbind(diagnosesAge,
        matrix(0, ncol = nprojYears, nrow = nages))
      colnames(diagnosesAgeFuture) <- paste0("y", 
        as.character(totalYears))
      for (age in 1:nages) {
        # Extract age group diagnoses
        ageDiagnoses <- diagnosesAge[age, ]
        
        # Estimate trends
        trendlm <- lm(diagnoses ~ year, 
          data.frame(year = (finalyear - 4):finalyear, 
            diagnoses = tail(ageDiagnoses, 5)))
        trendDiags <- predict(trendlm, data.frame(year = projYears))
        
        # Make sure diagnoses are at least zero (okay as normalization 
        # should fix this.
        trendDiags[trendDiags < 0] <- 0
        
        # Store final age trends
        diagnosesAgeFuture[age, ] <- c(ageDiagnoses, trendDiags)
      } 
    } else if(option == "reduce") {
      # Assume same reduction across all age groups
      diagnosesAgeFuture <- cbind(diagnosesAge,
        matrix(0, ncol = nprojYears, nrow = nages))
      colnames(diagnosesAgeFuture) <- paste0("y", 
        as.character(totalYears))
      for (age in 1:nages) {
        # Extract age group diagnoses
        ageDiagnoses <- diagnosesAge[age, ]
        
        # Reduce future diagnoses
        diagnosesFuture <- projectDecreaseAll * ProjVec(ageDiagnoses, 
          nprojYears)
        
        # Store final age trends
        diagnosesAgeFuture[age, ] <- diagnosesFuture
      }
    } else {
      # MANUALLY CHANGE HERE TO RUN ALTERNATE SCENARIOS
      
      # Example showing counterfactual to calculate number living 
      # diagnosed before 1996 etc
      # diagnoses[totalYears > 1996] <- 0 
      # projectName <- "pre1996"
      # Example looking at future impact of PrEP after 2016 assuming 
      # 50% reduction in diagnoses
      # diagnoses[totalYears > 2016] <- 0.5 * diagnoses[totalYears > 2016]
    }
  }  
  
  # Return results
  list(diagnosesFuture, diagnosesAgeFuture)  
  
}
