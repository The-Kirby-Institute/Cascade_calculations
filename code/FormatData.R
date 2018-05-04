#' Format results for documents and manuscripts
#' 
#' This function is used to produce a string from numbers describing 
#' results (value and range) for documents and manuscripts. 
#' 
#' @details The idea of this function is to take a number and range 
#' (optional) and put it into the suitable format for writing purposes 
#' using the appropriate format, range description, and units. 
#' 
#' For specific applications it is useful to right wrapper 
#' functions to save having to spell out all the options. 
#' 
#' The punctuation generally follows the Lancet style.    
#' 
#' @param estimate Numeric value we are reporting.
#' @param lower Lower value of the number we are reporting. Optional so 
#' NULL by default. Can just present the lower bound, e.g., to present 
#' "10 (SD 2)"
#' @param upper Upper value of the number we are reporting. Optional so 
#' NULL by default. 
#' @param places Integer indicating the number of decimal places (in round).
#' Negative values are allowed and means rounding to a power of ten, so for 
#' example round(x, digits = -2) rounds to the nearest hundred. 
#' @param prefix String to put in front of numbers. Primarily used for 
#' adding "$" or "A$" for monetary values.     
#' @param suffix String to put at the end of numeric values. Primarily used
#' used for adding percent symbol. 
#' @param units String to place after estimate for units. 
#' @param rangestr String to put in front of the range. For example 
#' "range: ", "95 percent CI: ".
#' @param rangeto String to link the lower and upper value in the range.
#' Default is an en-dash but could be ":", " to ", etc.
#' @param rangebracket Vector of two string to separate estimate from range. 
#' Default is round brackets c(" (",")") but could use other brackets or 
#' commas c(", ", ",") or something else. 
#' @param rangeappend Append prefix and suffix to both lower and upper 
#' ("both) or just prefix to lower and suffix to upper ("single"). 
#' @param thousands String to separate estimates by thousands. Default is 
#' a comma "," but can be a space " ". 
#' @param decimal String to show decimal only does lower or midline. 
#' Default is "." but Lancet uses a midline decimal.
#' @param tail String to attach to the end of the overall string. NOT sure
#' if this is useful or not.
#' 
#' @return String with the estimate and range in appropriate format.
#' 
#' @author Richard T Gray, \email{Rgray@kirby.unsw.edu.au}
#' 
#' @examples 
#' FormatData(1951508)
#' FormatData(1951508, prefix = "US$")
#' 
#' FormatData(36.7858, 5.8124, rangestr = "SD ")
#' FormatData(36.7858, 5.8124, units = "copies/ml", rangestr = "SD ")
#' 
#' FormatData(1235.45, 179.3, 12707.5, prefix = "A$", rangeappend = "both", 
#' rangeto = " to ", rangestr = "range: ")
#' 
#' FormatData(0.84, 0.71, 0.98, decimal = "mid", rangestr = "95% CI: ") 
#' FormatData(0.84, 0.71, 0.98, rangebracket = c(", ",","), 
#' rangestr = "95% CI: ", tail = paste0(" p=", toString(0.03)))
#' 
#' FormatData(84.3, 72.456, 98.123245, suffix = "%")
#' FormatData(84.3, 72.456, 98.123245, suffix = "%", units = "per year", 
#' rangeappend = "both", rangeto = " to ")
#' 
#' @export
#' @import stringr
#' 
FormatData <- function(estimate, lower = NA, upper = NA, places = NULL, 
  prefix = "", suffix = "", units = "", rangestr = "", rangeto = "\u2012",
  rangebracket = c(" (",")"), rangeappend = c("single", "both"), 
  thousands = c(",", " "), decimal = c("low","mid"), tail = "") {
  
  # Setup defaults - "mid" specifies a mid-line decimal
  decimal <- match.arg(decimal)
  decimal <- ifelse(decimal == "low", ".", "\xB7")
  thousands <- match.arg(thousands)
  rangeappend <- match.arg(rangeappend)
  
  # Round values appropriately
  if (is.null(places)) {
    
    # Change depending on estimate value using a function
    ValuePlaces <- function(value) {
      if (is.na(value)) {
        places <- 0
      } else if (value < 10) {
        places <- 2
      } else if (value >= 10 && value < 1000) {
        places <- 1
      } else if (value >= 1000 && value < 10000) {
        places - 0
      } else {
        places <- -2
      }
      
      return(places)
    }
    
    # Round estimate and range depending on value
    estValue <- round(estimate, ValuePlaces(estimate))
    lowValue <- round(lower, ValuePlaces(lower))
    upValue <- round(upper, ValuePlaces(upper))
    
  } else {
    
    # Round estimate and range using places
    estValue <- round(estimate, places)
    lowValue <- round(lower, places)
    upValue <- round(upper, places)
  } 
  
  # Convert values and range into strings - TODO may need to add format as 
  # a variable to specify scientific notation. 
  estStr <- formatC(estValue, big.mark = thousands, decimal.mark = decimal,
    format = "fg")
  lowStr <- ifelse(is.na(lowValue), "", formatC(lowValue, 
    big.mark = thousands, decimal.mark = decimal, format = "fg"))
  upStr <- ifelse(is.na(upValue), "", formatC(upValue, 
    big.mark = thousands, decimal.mark = decimal, format = "fg"))
  
  # Add suffixes and prefixes
  if (units != "") {
    estStr <- paste0(prefix, estStr, suffix, " ", units) 
  } else {
    estStr <- paste0(prefix, estStr, suffix)
  }
  
  # Setup range string
  if (lowStr == "" && upStr == "") {
  # No range
   rangeString <- ""
  } else if (upStr == "") {
    # Only lower bound as a single value
    rangeString <- paste0(rangebracket[1], rangestr, prefix, lowStr, 
      suffix, rangebracket[2])
  } else {
    # Full range
    if (rangeappend == "single") {
      rangeString <- paste0(rangebracket[1], rangestr, prefix, lowStr,
        rangeto, upStr, suffix, rangebracket[2])
    } else {
      rangeString <- paste0(rangebracket[1], rangestr, prefix, lowStr,
        suffix, rangeto, prefix, upStr, suffix, rangebracket[2])
    }
  }
  
  # Create and return final string
  finalStr <- paste0(estStr, rangeString, tail)
  return(finalStr)
} 
