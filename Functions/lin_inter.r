
lin_inter <- function(day) {
  
  
  for (i in 1:6){
  if (day > as.numeric(attributes(rf[i]))){
     tmp1 <- i
     tmp2 <- i + 1
  }
  }
  
  num   <- day - as.numeric(attributes(rf[tmp1]))
  denum <- as.numeric(attributes(rf[tmp2])) - as.numeric(attributes(rf[tmp1]))
  
  w1 <- num / denum
  w2 <- 1 - w1
    
  r <- w1 * rf[tmp2] + w2 *rf[tmp1]
  
  return(r)
}