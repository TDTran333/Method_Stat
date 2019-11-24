nll_student <- function(theta, x) {
  # This function takes as input a vector of parameters theta for
  # the student-t distribution (mu, sigma, and nu) and a value x.
  
  # The function outputs the negative log-likelihood function for
  # the student-t distribution.
  
  # Parameters
  mu  <- theta[1]
  sig <- theta[2]
  nu  <- theta[3]
  
  # Compute the negative log-likelihood function value
  nll <- -sum(dt((x - mu) / sig, df = nu, log = TRUE) - log(sig))
  
  # Make sure the negative log-likelihood function has a finite value
  if (!is.finite(nll)) {
    nll <- 1e10
  }
  
  # Output the negative log-likelihood function value
  return(nll)
}