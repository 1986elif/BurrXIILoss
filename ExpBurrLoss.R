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
