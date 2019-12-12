price_BS <- function(S, K, r, vol, M, Type) {
  # This function takes as input the underlying asset price (S), the
  # option strike price (K), the risk-free rate (r), the volatility
  # (vol), and the option time to expiry (M) in year (360-day basis).
  
  # The function outputs the price of a call option with those five
  # parameters using the Black-Scholes (1973) model.
  
  # Compute the quantities 'd_1' and 'd_2'
  d_1 <- (log(S / K) + (r + vol^2 / 2) * M) / (vol * M^0.5)
  d_2 <- d_1 - vol * M^0.5
  
  # Compute the option price
  if (Type == 1) {
  price <- S * pnorm(d_1) - K * exp(-r * M) * pnorm(d_2)
  } else if (Type == 0) {
  price <- K * exp(-r * M) * pnorm(-d_2) - S * pnorm(-d_1)
  } else {
    stop("Wrong option type. Specify Call (1) or Put (0).")
  }
  
  # Output the option price
  return(price)
}