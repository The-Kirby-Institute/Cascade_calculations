## R function to group a vector into bins

AgeCat <- function(x, lower = 0, upper, by = 10,
                    sep = "_", above.char = "+") {
  # Categorise ages into age bins specified by lower, upper and by.
  # 
  # From the website: http://www.r-bloggers.com/r-function-of-the-day-cut/
  # 
  # Uses sprintf to extend prepend numbers < 10 with a 0.
  
  
  labs <- c(paste(sprintf("%02d", seq(lower, upper - by, by = by)),
                  sprintf("%02d", seq(lower + by - 1, upper - 1, by = by)),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  labs <- paste0("a", labs)
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}
