

BurrLoss <- function(y, tau, K, alpha_L, c_L) {
  power1 <- c_L - 1
  power2 <- (alpha_L^c_L * c_L - alpha_L^c_L - tau^c_L) / (tau^c_L * c_L) + 1
  term1 <- y / tau
  term2 <- (alpha_L^c_L + tau^c_L) / (alpha_L^c_L + y^c_L)
  term3 <- (term1^power1) * (term2^power2)
  result <- K * (1 - term3)
  return(result)
}


This function takes five parameters, namely y, tau, K, alpha_L, and c_L."

Example:

y <- 5
tau <- 10
K <- 3
alpha_L <- 2
c_L <- 4
result <- BurrLoss(y, tau, K, alpha_L, c_L)
------------------------------------------------------------------------------
ExpBurrLoss <- function(tau, K, alpha_L, c_L, alpha, c, k) {
  integrand <- function(y) {
    term1 <- K * (1 - ((y / tau)^(c_L - 1) * ((alpha_L^c_L + tau^c_L) / (alpha_L^c_L + y^c_L))^((alpha_L^c_L * c_L - alpha_L^c_L - tau^c_L) / (tau^c_L * c_L) + 1)))
    term2 <- (c * k / alpha) * (y / alpha)^(c - 1) * (1 + (y / alpha)^c)^(-(k + 1))
    result <- term1 * term2
    return(result)
  }
  result <- integrate(integrand, lower = 0, upper = Inf)$value
  return(result)
}

This function takes seven parameters, namely tau, K, alpha_L, c_L, alpha, c, and k.

Example:

tau <- 10
K <- 3
alpha_L <- 2
c_L <- 4
alpha <- 1
c <- 2
k <- 3
result <- EL_TBXII(tau, K, alpha_L, c_L, alpha, c, k)
