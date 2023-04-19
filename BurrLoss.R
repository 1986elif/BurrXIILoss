BurrLoss <- function(y, tau, K, alpha_L, c_L) {
  power1 <- c_L - 1
  power2 <- (alpha_L^c_L * c_L - alpha_L^c_L - tau^c_L) / (tau^c_L * c_L) + 1
  term1 <- y / tau
  term2 <- (alpha_L^c_L + tau^c_L) / (alpha_L^c_L + y^c_L)
  term3 <- (term1^power1) * (term2^power2)
  result <- K * (1 - term3)
  return(result)
}
