#' Determine the state or territory correspodning to a local region
#' 
#' This function simply returns the state or territory correspodning to
#' a specific SA3 local region.
#' 
#' @param localRegion Character specifying the name of the local region
#' 
#' @return A character string specifying the state/territory
#'
#' @author Richard T Gray, \email{Rgray@kirby.unsw.edu.au}
#' 
#' @export
#' @import tidyverse  
#' 
RegionState <- function(localRegion, regionCodes) {
  
  # Extract the postcodes of the region
  postcodes <- filter(regionCodes, SA3_name_2016 == localRegion)$postcode
  
  # First see if postcodes is empty - shouldn't be
  if (length(postcodes) == 0) {
    stop("Error: Unknown local region")
  }
  
  # See which state postcodes belong to
  # Note have specified 2618 to be in NSW (it is across the border with ACT)
  if (all(postcodes %in% c(200:299, 2600:2617, 2900:2920))) {
    state <- "act" 
  } else if (all(postcodes %in% c(1000:2599, 2618:2899, 2921:2999, 3586, 
    3644, 3707))) {
    state <- "nsw"
  } else if (all(postcodes %in% 800:999)) {
    state <- "nt"
  } else if (all(postcodes %in% c(4000:4999, 9000:9999))) {
    state <- "qld"
  } else if (all(postcodes %in% 5000:5999)) {
    state <- "sa"
  } else if (all(postcodes %in% 7000:7999)) {
    state <- "tas"
  } else if (all(postcodes %in% c(3000:3999, 8000:8999))) {
    state <- "vic"
  } else if (all(postcodes %in% 6000:6999)) {
    state <- "wa"
  } else {
    stop("Error: postcodes aren't all in the same state or territory") 
  }
  
  # Return state
  return(state)
  
}
