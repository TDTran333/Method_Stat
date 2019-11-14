Price_call <- function(S, K, r, vol, N) {
  
  d1 <- (log(S/K) + (r + vol^2 / 2) * N) / (vol * N^0.5)
  d2 <- d1 - vol * N^0.5
  
  price <- S * pnorm(d1) - K * exp(r * N) * pnorm(d2)  
  
  if (price < 0){
      price <- 0
  }
  
  return(price)
}