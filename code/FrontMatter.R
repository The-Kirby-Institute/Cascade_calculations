#' R Front Matter
#'
#' This function is used to create a string of information regarding the
#' R/Rstudio version and R package details used for an analysis for
#' printing in documents. The returned string includes newline characters so
#' requires cat() to display properly.
#'
#' @param packages Vector of packages we want to display. Default is NULL saying
#' we don't want to return any package information. Otherwise it can
#' be "all" to display information for all packages loaded.
#' @param rstudio String specifying Rstudio version used. Default is NULL.
#'
#' @return A string with the front matter information. By default returns R
#' version and time.
#'
#' @author Richard T Gray, \email{Rgray@kirby.unsw.edu.au}
#'
#' @examples
#' FrontMatter()
#' FrontMatter("ggplot2")
#' FrontMatter(packages = "all", rstudio = "1.1.423")
#'
#' @export
#' @importFrom stringr str_c
#'
FrontMatter <- function(packages = NULL, rstudio = NULL) {

  # String materials
  versionStr <- R.Version()$version
  time <- format(Sys.Date(), format="%Y-%m-%d")

  # Setup R/Rstudio string
  rstr <- str_c("Version info: Code run using ", versionStr)
  if (!is.null(rstudio)) {
    rstr <- str_c(rstr, "\nin: Rstudio version ", rstudio)
  }

  # Setup time string
  tstr <- str_c("On: ", time)

  # Setup package string
  pstr <- ""
  if (!is.null(packages)) {
    if (packages == "all") {
      packages <- names(sessionInfo()$otherPkgs)
    }
    # loop through inputed list of packages
    pstr <- "With: "
    for (pkg in packages) {
      pstr <- str_c(pstr, pkg, " ", packageVersion(pkg), "; ")

    }
  }
  # Put everything together and return
  finalStr <- str_c(rstr, "\n", tstr, "\n", pstr)
  return(finalStr)
}
