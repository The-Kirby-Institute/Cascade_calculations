## R function to estimate duplicates in the HIV registry

# R. T. Gray
# Function is to replace current estimates with estimates for 
# specific populations obtained separately for a specific time period. 
# These values are read in from a csv file. 

ReplaceEstimates <- function(cascade, hardcode) {  
  # Replace with updated values
  for (ii in 1:nrow(hardcode)) {
    cascade <- cascade %>%
      mutate(value = ifelse(population == hardcode$population[ii] &
                              year == hardcode$year[ii] &
                              stage == hardcode$stage[ii], 
                            hardcode$value[ii],
                            value),
             lower = ifelse(population == hardcode$population[ii] &
                              year == hardcode$year[ii] &
                              stage == hardcode$stage[ii], 
                            hardCodeValues$lower[ii],
                            lower),
             upper = ifelse(population == hardcode$population[ii] &
                              year == hardcode$year[ii] &
                              stage == hardcode$stage[ii], 
                            hardcode$upper[ii],
                            upper))
  }
  
  return(cascade)
}
