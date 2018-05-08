#' A function to load packages
#'
#' This function is a wrapper for require() to check if a package is available 
#' and then attempts to install the package if it is unavailable. By rights 
#' should be called LoadPackage.
#'
#' @param package Character string (unquoted) specifying name of package
#' 
#' @return none
#' 
#' @author Richard T. Gray, \email{Rgray@kirby.unsw.edu.au}
#' @references \url{https://stackoverflow.com/questions/5595512/what-is-the-difference-between-require-and-library}
#'
#' @examples
#' LoadLibrary(tidyverse)
#' 
#' @export
#'
LoadLibrary <- function(package) {

  # Coerce string into a name
  package <- as.character(substitute(package))
  
  # Load the required package
  if(require(package, character.only = TRUE)) {    
    print(paste(package, "is loaded correctly"))
  } else {
    
    print(paste("trying to install", package))
    install.packages(package)
    
    if(require(package, character.only = TRUE)) {
      print(paste(package, "installed and loaded"))
    } else {
      stop(paste("could not install", package))
    }
  }
  
}
