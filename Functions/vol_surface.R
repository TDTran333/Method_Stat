vol_surface <- function(S, K, M, a1, a2, a3, a4) {
  # This function takes as input the underlying asset price (S), the
  # option strike price (K), the option time to expiry (M) in year
  # (360-day basis), and four other parameters named a1, a2, a3, a4.
  
  # The function outputs the implied volatility from the parametric
  # volatility surface.
  
  # Compute the moneyness
  moneyness <- K / S
  
  # Compute the implied volatility from the parametric volatility surface
  vol <- a1 + a2 * (moneyness - 1)^2 + a3 * (moneyness - 1)^3 + a4 * sqrt(M)
  
  # Output the implied volatility
  return(vol)
}