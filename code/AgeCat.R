## R function to group a vector into bins

AgeCat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  # Categorise ages into age bins specified by lower, upper and by.
  #
  # From the website: http://www.r-bloggers.com/r-function-of-the-day-cut/
  
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}
