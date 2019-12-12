vol_calibrate <- function(a) {
  # This function takes as input the vector of four parameters a.
  
  # The function outputs the sum of absolute deviations between the
  # implied volatility of traded call and put options and the implied
  # volatility of the parametric volatility surface
  
  # Compute the sum of absolute deviations
  abs_dev <- sum(abs(mkt_vol[,4] - vol_surface(mkt_vol[,1],
                                               mkt_vol[,2],
                                               mkt_vol[,3],
                                               a[1], a[2], a[3], a[4])))
}