##R function to format results

# This function formats data and results into string showing the value 
# and range

FormatData <- function(estimate, lower = NULL, upper = NULL, 
                        places = -2, money = FALSE) {
  
  if (is.null(lower) | is.null(upper)) {
    if (money) {
      formStr <- paste("$", formatC(round(estimate, places), format = "d", big.mark = ','), sep="")
    } else {
      formStr <- paste(formatC(round(estimate, places), format = "d", big.mark = ','), sep="")
    }
  } else {
    if (money) { 
      formStr <- paste("$", formatC(round(estimate, places), format="d", big.mark=','), 
                       " ($", formatC(round(lower, places), format="d", big.mark=','), 
                       " - $", formatC(round(upper, places), format="d", big.mark=','), 
                       ")", sep="")
    } else {
      
      formStr <- paste(formatC(round(estimate, places), format="d", big.mark=','), 
                       " (", formatC(round(lower, places), format="d", big.mark=','), 
                       " - ", formatC(round(upper, places), format="d", big.mark=','), 
                       ")", sep="")
    }
  }  
  
  return(formStr)
} 
