removeDuplicates <- function(dobvector, days.ignore, 
                             na.rm = FALSE, 
                             write = NA){
  # Calculate the number of unique people given recorded date of birth. 
  # Requires dplyr library.
  # Args:
  #   dobvector: A vector of strings specifying the date of births of  
  #               individuals. Dates of birth must be in string form YYYY-MM-DD.
  #   days.ignore: A vector of days to ingnore in calculations.
  #   na.rm: If TRUE exclude missing dob from calculation. If FALSE use 
  #           missing values using the proportion calculated for the complete  
  #           data to estimate thenumber of unique people with missing dob. 
  #           Deafult is FALSE.
  #   write: String specifying file to save output dataframe as a csv file. 
  #           Default is NA meaning no file is written.         
  # Returns:
  #   The estimated number of unique cases in the list of date of birth vector. 
  #   If specified a csv file of results is also saved. 
  
  # Setup ---------------------------------------------------------------------

  # Make sure we have required libraries 
  if (require("dplyr")) {
#     print("dplyr is loaded correctly")
  } else {
    print("trying to install dplyr")
    install.packages("dplyr")
    if (require(dplyr)) {
      print("dplyr installed and loaded")
    } else {
      stop("could not install dplyr")
    }
  }
  
  # Error handling
  # Check dobvector is a vector with the correct string format
  # Make sure days.ignore vector min > 0 and max <= 31 
  
  #(TODO): Need to add error checking code
  
  # Number of unique dates -----------------------------------------------------
  
  # From the days.ignore vector calculate the number of days to ignore each year. 
  # This is a bit complicated due to ssues with the 29th, 30th, and 31st. This 
  # is for a standard year. Leap years handled below.
  days.remove <- 0
  for (ii in 1:31) {
    if (ii %in% days.ignore) {
      if (ii <= 28) {
        days.remove <- days.remove + 12
      } else if(ii == 29){
        days.remove <- days.remove + 11 
      } else if(ii == 30){
        days.remove <- days.remove + 11
      } else {
        # ii is 31
        days.remove <- days.remove + 7
      }    
    }
  }
  
  # Main program --------------------------------------------------------------
  
  # Remove NAs and save number missing
  dob.data <- dobvector[!is.na(dobvector)]
  num.missing <- sum(is.na(dobvector) == TRUE) 
  
  # Extract birthyear for each case
  birth.year <- as.numeric(substr(dob.data,1,4))
  birth.day <- as.numeric(substr(dob.data,9,10))
  
  # Years, leap years and days in each year
  years <- min(birth.year,na.rm = TRUE):max(birth.year,na.rm = TRUE)
  leap.years <- seq(min(birth.year,na.rm = TRUE),max(birth.year,na.rm = TRUE),
                    by=4) 
  leap.years <- leap.years[leap.years != 1900]  # 1900 is not a leap year but 2000 is
  
  year.days <- rep(365,length(years))
  if (!(29 %in% days.ignore)) {
    # For leap years use 366 unless the 29th is to be ignored
    year.days[match(leap.years,years)] <- 366
  }
  year.days.ignore <- year.days-days.remove
  
  # Tally up total dates and unique dates overall.
  # Set up cases dataframe to make it easy to count unique dates
  cases <- data.frame(dob = dob.data,birthyear = birth.year, birthday = birth.day)
  
  total.cases <- count(cases,birthyear)
  unique.dates <- cases %>% 
    dplyr::group_by(birthyear) %>% 
    summarise(n = dplyr::n_distinct(dob))
  possible.days <- year.days[match(total.cases$birthyear,years)]
  
  sub.total.cases <- count(cases[!(cases$birthday %in% days.ignore),],birthyear)
  sub.unique.dates <- cases[!(cases$birthday %in% days.ignore),] %>% 
    dplyr::group_by(birthyear) %>% summarise(n = dplyr::n_distinct(dob))
  possible.days.ignore <- year.days.ignore[match(sub.total.cases$birthyear,years)]
  
  # Calculate result vectors
  unique.cases <- possible.days * log(possible.days / (possible.days - unique.dates$n))
  variance.cases <- possible.days * unique.dates$n / (possible.days - unique.dates$n)
  
  unique.sub.cases <- possible.days.ignore * log(possible.days.ignore / 
                                                   (possible.days.ignore - sub.unique.dates$n))
  variance.sub.cases <- possible.days.ignore * sub.unique.dates$n / 
                          (possible.days.ignore - sub.unique.dates$n)
  
  # Calculate number of duplicates
  num.cases <- sum(total.cases$n)  
  duplicates <- num.cases - sum(unique.cases)
  prop.duplicates <- duplicates/num.cases
  
  num.sub.cases <- sum(sub.total.cases$n)
  sub.duplicates <- num.sub.cases - sum(unique.sub.cases)
  prop.sub.duplicates <- sub.duplicates/num.sub.cases
  
  total.duplicates <- sub.duplicates + (num.cases-num.sub.cases)*
    prop.sub.duplicates 
  
  # Return number of unique cases
  total <- num.cases - total.duplicates
  if (!na.rm) {
    prop.unique <- total/num.cases
    total <- total + prop.unique * num.missing
  }
  
  # Write to file -------------------------------------------------------------
  
  if (!is.na(write)) {
    # Create an output data frame and write to file   
    # Initilize an output dataframe
    outputcols <- c("birthyear","cases","days","totaldates","unique","variance",
                    "subcases","subdays","subtotaldates","subunique","subvariance")
    output <- data.frame(matrix(0,nrow = length(years),ncol = length(outputcols)))
    colnames(output) <-outputcols
  
    # Fill in data frame for output
    output$birthyear <- years
    output$cases[match(total.cases$birthyear,output$birthyear)] <- total.cases$n
    output$days <- year.days
    output$totaldates[match(unique.dates$birthyear,output$birthyear)] <- unique.dates$n
    output$unique[match(unique.dates$birthyear,output$birthyear)] <- unique.cases
    output$variance[match(unique.dates$birthyear,output$birthyear)] <- variance.cases
    output$subcases[match(sub.total.cases$birthyear,output$birthyear)] <- sub.total.cases$n
    output$subdays <- year.days.ignore
    output$subtotaldates[match(sub.unique.dates$birthyear,output$birthyear)] <- sub.unique.dates$n
    output$subunique[match(sub.unique.dates$birthyear,output$birthyear)] <- unique.sub.cases
    output$subvariance[match(sub.unique.dates$birthyear,output$birthyear)] <- variance.sub.cases
    
    # Write to file
    write.csv(output, file = paste(write,".csv", sep =""), row.names = FALSE)
  }
  
  # Return what we want
  total
}
