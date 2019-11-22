Price_call <- function(S, K, r, vol, M) {
  # This function takes as input the underlying asset price (S), the
  # option strike price (K), the risk-free rate (r), the volatility
  # (vol), and the option time to expiry (M) in year (360-day basis).
  
  # The function outputs the price of a call option with those five
  # parameters using the Black-Scholes (1973) model.
  
  # Compute the quantities 'd_1' and 'd_2'
  d_1 <- (log(S / K) + (r + vol^2 / 2) * M) / (vol * M^0.5)
  d_2 <- d_1 - vol * M^0.5
  
  # Compute the call option price
  price <- S * pnorm(d_1) - K * exp(r * M) * pnorm(d_2)
  
  # Make sure the call options price is larger or equal to zero
  if (price < 0) {
      price <- 0
  }
  
  # Output the call option price
  return(price)
}