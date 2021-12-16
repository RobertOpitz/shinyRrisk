rrisk_rtnorm <- function(n, mean = 0, sd = 1, 
                         lower = -Inf, upper = Inf) {
  min_p <- pnorm(q = lower, mean = mean, sd = sd)
  max_p <- pnorm(q = upper, mean = mean, sd = sd)
  qnorm(p    = runif(n, min = min_p, max = max_p),
        mean = mean, 
        sd   = sd)
}

# rrisk_qtnorm <- function(p, mean = 0, sd = 1,
#                          lower = -Inf, upper = Inf) {
#   #if (p == 0.0)
#   #  return(lower)
#   #else if (p == 1.0)
#   #  return(upper)
#   min_p <- pnorm(q = lower, mean = mean, sd = sd)
#   max_p <- pnorm(q = upper, mean = mean, sd = sd)
#   qnorm(p    = p * (max_p - min_p) + min_p,
#         mean = mean,
#         sd   = sd)
# }

rrisk_rmultinom <- function(n, probs) {
  result <- rmultinom(n, size =1, prob = probs)
  sapply(seq_len(ncol(result)), 
         function(icol) which(result[,icol] == 1))
}

rrisk_mod_rpert <- function(n, min_value, mode_value, max_value, shape = 4) {
  alpha <- 1 + shape * (mode_value - min_value) / (max_value - min_value)
  beta  <- 1 + shape * (max_value - mode_value) / (max_value - min_value)
  q <-  qbeta(p      = runif(n),
              shape1 = alpha,
              shape2 = beta)
  #cat("Erwartungswert E[X] =", mu <- (min_value + shape*mode_value + max_value)/(shape+2), "\n")
  #cat("Median =", (min_value + 6*mode_value + max_value)/8, "\n") 
  #cat("Varianz var[X] =", ((mu - min_value)*(max_value - mu))/7, "\n")
  q * (max_value - min_value) + min_value
}

# rrisk_rpearsonV <- function(n, a, b) {
#   r <- runif(n)
#   qgamma(p = r * gamma(a),
#          shape = a,
#          rate = b/qgamma(r, a, b))
# }
# 
# ppearsonV <- function(q, shape, rate) {
#   pgamma(q, shape, rate/q) / gamma(q)
# }
# plot(q, ppearsonV(q, 1, 1), type = "l")