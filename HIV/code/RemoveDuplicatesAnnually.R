## R function to extract subset of interest from notifications

# N.A. Bretana and R.T. Gray

numUnique <- function(dobframe, years,ignore, file = NA) {
  # Loop through years and calculate cumulative number of unique cases 
  numcases <- rep(NA,length(years))
  for (ii in seq(along=years)) {
    dobvector <- filter(dobframe,yeardiagnosis <= years[ii])$dob
    if (length(dobvector) != 0) {
      # Make sure our dobvector isn't empty
      numcases[ii] <- RemoveDuplicates(dobvector,ignore)
    }
  }
  
  # If selected write final output to file
  if (!is.na(file)) {
    numcases[ii] <- RemoveDuplicates(dobvector,ignore, write = file)
  }
  
  # Return vector of unique cases
  return(numcases)
}