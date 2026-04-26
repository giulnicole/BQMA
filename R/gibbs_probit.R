# R/gibbs_probit.R
#' @importFrom stats pnorm qnorm rnorm rbinom rbeta runif quantile
NULL
#' Gibbs sampler for Bayesian probit regression with spike-slab prior
#'
#' Internal function. Uses Albert and Chib (1993) data augmentation
#' with George and McCulloch (1993) marginal Bayes factor for
#' variable selection. Optimised with partial residual update.
#'
#' @param X Matrix (n x p). M-values, samples as rows. Must be scaled.
#' @param y Binary numeric vector (0/1) of length n.
#' @param n_iter Integer. MCMC iterations after burnin.
#' @param burnin Integer. Burnin iterations to discard.
#' @param spike_var Numeric. Prior variance for spike component.
#' @param slab_var Numeric. Prior variance for slab component.
#' @param a_pi Numeric. Shape1 of Beta prior on inclusion probability.
#' @param b_pi Numeric. Shape2 of Beta prior on inclusion probability.
#' @return A list with beta, pip, beta_mean, beta_ci.
#' @keywords internal
.gibbs_probit <- function(X, y,
                          n_iter    = 3000L,
                          burnin    = 1000L,
                          spike_var = 1e-4,
                          slab_var  = 1.0,
                          a_pi      = 0.05,
                          b_pi      = 0.95) {
  n   <- nrow(X)
  p   <- ncol(X)
  Xss <- colSums(X^2)   # precompute column norms
  stopifnot(all(y %in% c(0, 1)))
  # initialise
  beta  <- rep(0, p)
  pi0   <- a_pi / (a_pi + b_pi)
  z     <- ifelse(y == 1, 0.5, -0.5)
  # storage
  beta_store  <- matrix(0, n_iter, p)
  gamma_store <- matrix(0, n_iter, p)
  total_iter <- n_iter + burnin
  for (it in seq_len(total_iter)) {
    #
    # Step 1: z_i | beta, y_i -- TruncatedNormal vectorised
    #
    mu_z <- as.numeric(X %*% beta)
    u    <- runif(n)
    idx1    <- y == 1
    p_lo    <- pmin(pnorm(0, mu_z[idx1], 1), 1 - 1e-10)
    z[idx1] <- qnorm(p_lo + u[idx1] * (1 - p_lo), mu_z[idx1], 1)
    idx0    <- y == 0
    p_hi    <- pmax(pnorm(0, mu_z[idx0], 1), 1e-10)
    z[idx0] <- qnorm(u[idx0] * p_hi, mu_z[idx0], 1)
    z <- pmax(pmin(z, 10), -10)
    #
    # Step 2: beta_j | z, gamma_j -- spike-slab with marginal BF
    # George & McCulloch (1993)
    #
    r <- as.numeric(z - X %*% beta)
    for (j in seq_len(p)) {
      # partial residual
      rj <- r + X[, j] * beta[j]
      # slab posterior N(mu_s, var_s)
      var_s <- 1 / (Xss[j] + 1 / slab_var)
      mu_s  <- var_s * sum(X[, j] * rj)
      # log marginal Bayes factor (analytical integration over beta_j)
      log_bf    <- 0.5 * log(var_s / slab_var) +
        0.5 * mu_s^2 / var_s
      log_odds  <- log_bf + log(pi0 + 1e-10) -
        log(1 - pi0 + 1e-10)
      log_odds  <- min(max(log_odds, -500), 500)
      prob_incl <- 1 / (1 + exp(-log_odds))
      gamma_j  <- rbinom(1, 1, prob_incl)
      beta_new <- if (gamma_j == 1L) rnorm(1, mu_s, sqrt(var_s)) else 0
      # update residual
      r       <- r - X[, j] * (beta_new - beta[j])
      beta[j] <- beta_new
      if (it > burnin)
        gamma_store[it - burnin, j] <- gamma_j
    }
    #
    # Step 3: pi0 | gamma -- Beta conjugate update
    #
    n_in <- sum(beta != 0)
    pi0  <- rbeta(1, a_pi + n_in, b_pi + p - n_in)
    # store post-burnin
    if (it > burnin)
      beta_store[it - burnin, ] <- beta
  }
  # summarise posterior
  pip <- colMeans(gamma_store)
  beta_mean <- sapply(seq_len(p), function(j) {
    ins <- gamma_store[, j] == 1
    if (sum(ins) == 0) return(0)
    mean(beta_store[ins, j])
  })
  beta_ci <- apply(beta_store, 2, quantile,
                   probs = c(0.025, 0.975))
  list(
    beta      = beta_store,
    pip       = pip,
    beta_mean = beta_mean,
    beta_ci   = t(beta_ci)
  )
}
