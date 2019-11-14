#Question 1 - Pricing calls

# here packages
install.packages("here")
library("here")

# load data
load(file = here("Data", "Market.rda"))

# load function 
source(file = here("Functions", "price_call.r")) # function that price Calls with Black-Shcole  formula
source(file = here("Functions", "lin_inter.r"))  # interpolation lineaire entre les taux d'interet de la courbe d'interet


# data
vix    <- Market$vix
sp_500 <- Market$sp500
rf     <- Market$rf
calls  <- Market$calls
puts   <- Market$puts


# Portfolio  Calls - Caracteristics
book <- matrix(data=NA, nrow = 4, ncol = 4)
colnames(book) <- c("Quantity","Call","StrikePrice","Maturity")

book[1,] <- c(1, 1, 1600, 20 / 365)
book[2,] <- c(1, 1, 1650, 20 / 365)
book[3,] <- c(1, 1, 1750, 40 / 365)
book[4,] <- c(1, 1, 1800, 40 / 365)


# input for black-Schole calls pricing 
# interest rate interpolated
r_1 <- lin_inter(book[1,4])
r_2 <- lin_inter(book[3,4])
# Spot price, Volatility, Strike price, Maturity
S   <- sp_500[length(sp_500)]
vol <- vix[length(vix)]
K   <- book[,3]
N   <- book[,4] 

# calculating and Stocking Call's prices
call_price <- c(rep(NA, 4))
call_price[1] <- Price_call(S, K[1], r_1, vol, N[1])
call_price[2] <- Price_call(S, K[2], r_1, vol, N[2])
call_price[3] <- Price_call(S, K[3], r_2, vol, N[3])
call_price[4] <- Price_call(S, K[4], r_2, vol, N[4])

# stocking Portfolio Value
PF_price <- sum(call_price)




# Question 2

# log return - sp_500
log_return <-diff(log(sp_500))

# model calibration
tmp    <- length(log_return)

mu_hat <- mean(log_return[seq(1,tmp,5)])
s2_hat <- mean((log_return[seq(1,tmp,5)] - mu_hat)^2) #n/(n-1)
s_hat  <- s2_hat^0.5


# number of simulation
H <- 10000
t <- 5 / 365

# Portfolio  Calls - Caracteristics - 5 days later
book_2 <- book                 # same as previous except days until maturity
book_2[,4] <- book_2[,4] - t   # t days later

# log return sp_500 projected in 5 days
# set seed(1234).....
sample <- rnorm(H, mean=mu_hat, sd=s_hat)

# price sp_500 projected in 5 days
S_2 <- sp_500[length(sp_500)] * exp(sample)


# input for black-Schole calls pricing
r_3   <- lin_inter(book_2[1,4])
r_4   <- lin_inter(book_2[3,4])
vol_2 <- vix[length(vix)]
K_2   <- book_2[,3]
N_2   <- book_2[,4] 

# stocking matrix (Hx4) - H simulation, 4 calls 
call_price_2 <- matrix(NA, nrow = H, ncol = 4)

# calculating and stocking call's price simulation
for (i in 1:10000){
  call_price_2[i,1] <- Price_call(S_2[i], K_2[1], r_3, vol_2, N_2[1])
  call_price_2[i,2] <- Price_call(S_2[i], K_2[2], r_3, vol_2, N_2[2])
  call_price_2[i,3] <- Price_call(S_2[i], K_2[3], r_4, vol_2, N_2[3])
  call_price_2[i,4] <- Price_call(S_2[i], K_2[4], r_4, vol_2, N_2[4])
}

# stocking H portfolio value 
PF_price_2 <- apply(call_price_2,1,sum)


# P&N of the H simulation 
r_pl <- lin_inter(t)  #actualisation factor
pl   <- PF_price_2 * exp(-t *r_pl) - PF_price

# VaR and EVaR
alpha <- 0.95
VaR <- sort(pl)[(1 - alpha) * H]
EVaR <- mean(sort(pl)[1:((1 - alpha) * H)])


# histogram
hist(pl, nclass = round(10 * log(length(pl))), probability = TRUE)

# Add vertical line for VaR value
abline(v = quantile(pl, probs = (1 - alpha)),
       lty = 2, ## line type: 2 = dashed
       lwd = 2.5, ## line width: the largher, the thicker
       col = "red")





# Question 3

