## R function to calculate error range in Prospection PharmDash data

# R.T. Gray (formula derived by Hamish McManus)

PharmDashError <- function(overallPop, treatEstimate, pharmDashProp = 0.1) {
  # Calculate the upper and lower bound for the estimated number of people
  # taking a treatment provided by Prospection
  #
  # Args:
  #   overallPop: The overall population size of the population under 
  #     consideration.
  #   treatEstimate: the number of people estimated to be on treatment.
  #   pharmDashProp: the factor Prospection uses to inflate the sample they
  #     have to obtain treatEstimate. This is fixed to 0.1 but can change
  #     if this function is required elsewhere.
  # Returns:
  #   PharmDashError: A two element vector with the lower and upper error
  #     bound.
  #
  # -----------------------------------------------------------------------

  # Initialize calculation values
  subPopulation <- pharmDashProp * overallPop
  treatSample <- pharmDashProp * treatEstimate 
  
  # Caculate the lower and upper bounds
  lower <- overallPop * ((treatSample / subPopulation) - 
    1.96 * sqrt((1 / subPopulation) * 
                  (treatSample / subPopulation) * 
                  (1 - treatSample / subPopulation)))
  
  upper <- overallPop * ((treatSample / subPopulation) + 
    1.96 * sqrt((1 / subPopulation) * 
                  (treatSample / subPopulation) * 
                  (1 - treatSample / subPopulation)))
  
  # Return the bounds as a data frame (so can be vectorised)
  return(data.frame(lower = lower, upper = upper))
}
