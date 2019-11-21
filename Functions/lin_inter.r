lin_inter <- function(day) {
  
  for (i in 1:nrow(rates)) {
    if (day > rates[i,1]) {
       lb <- i
       ub <- i + 1
      }
  }
  
  num   <- day - rates[lb,1]
  denum <- rates[ub,1] - rates[lb,1]
  
  w_1 <- num / denum
  w_2 <- 1 - w_1
    
  r <- w_1 * rates[ub,2] + w_2 * rates[lb,2]
  
  return(r)
}