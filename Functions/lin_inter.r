lin_inter <- function(day) {
  # This function takes as input a maturity in year (360-day basis)
  # and outputs the interpolated risk-free rate using a term structure
  # specified in the assignment.
  
  # Loop through the term structure and identify the largest smaller
  # maturity (lb) and the lowest larger maturity (ub).
  for (i in 1:nrow(rates)) {
    if (day > rates[i,1]) {
       lb <- i
       ub <- i + 1
      }
  }
  
  # Compute the numerator and the denominator used to compute 'w_1'
  num   <- day - rates[lb,1]
  denum <- rates[ub,1] - rates[lb,1]
  
  w_1 <- num / denum
  w_2 <- 1 - w_1
  
  # Compute the interpolated risk-free rate
  r <- w_1 * rates[ub,2] + w_2 * rates[lb,2]
  
  # Output the interpolated risk-free rate
  return(r)
}