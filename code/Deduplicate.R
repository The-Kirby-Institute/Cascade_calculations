## R function to extract subset of interest from notifications

# N.A. Bretana and R.T. Gray

deduplicateFile <- function(pop, state, mode, years) {
  filename <- paste(basePath, "/output/uniquehiv-", state, "_", mode, 
                    "_", toString(tail(years,1)),sep="")
}