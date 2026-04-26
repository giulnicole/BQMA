# R/gibbs_sampler.R

#' @importFrom stats dnorm rbinom rnorm rgamma rbeta runif quantile
NULL

#' Gibbs sampler for Asymmetric Laplace quantile regression
#'
#' Internal function. Uses the location-scale mixture representation
#' of the AL distribution (Kozumi and Kobayashi 2011) with spike-slab prior.
#'
#' @param X Matrix (n x p) of covariates. M-values, samples as rows.
#' @param y Numeric vector of length n. Phenotype.
#' @param tau Numeric. Quantile to fit, in (0,1).
#' @param n_iter Integer. MCMC iterations after burnin.
#' @param burnin Integer. Burnin iterations to discard.
#' @param spike_var Numeric. Prior variance for spike component.
#' @param slab_var Numeric. Prior variance for slab component.
#' @param a0 Numeric. Shape hyperprior for sigma.
#' @param b0 Numeric. Rate hyperprior for sigma.
#' @param a_pi Numeric. Shape1 of Beta prior on inclusion probability.
#' @param b_pi Numeric. Shape2 of Beta prior on inclusion probability.
#' @return A list with beta, pip, sigma, beta_mean, beta_ci.
#' @keywords internal
.gibbs_AL <- function(X, y, tau,
                      n_iter    = 3000L,
                      burnin    = 1000L,
                      spike_var = 1e-4,
                      slab_var  = 0.1,
                      a0        = 0.01,
                      b0        = 0.01,
                      a_pi      = 1,
                      b_pi      = 19) {

  n <- nrow(X)
  p <- ncol(X)

  # Asymmetric Laplace location-scale mixture constants
  theta <- (1 - 2 * tau) / (tau * (1 - tau))
  tau2  <- 2 / (tau * (1 - tau))

  # initialise
  beta  <- rep(0, p)
  sigma <- 1.0
  pi0   <- a_pi / (a_pi + b_pi)
  v     <- rep(1, n)

  # storage
  beta_store  <- matrix(0, n_iter, p)
  gamma_store <- matrix(0, n_iter, p)
  sigma_store <- numeric(n_iter)

  total_iter <- n_iter + burnin

  for (it in seq_len(total_iter)) {

    # 1. Update auxiliary variables v_i
    resid <- as.numeric(y - X %*% beta)
    chi   <- resid^2 / (tau2 * sigma)
    psi   <- theta^2 / tau2 + 2 / sigma
    mu_ig <- sqrt(chi / psi)
    v     <- .rinvgauss(n, mu_ig, chi)
    v     <- pmax(v, 1e-10)

    # 2. Update beta_j coordinate-wise spike-slab
    w <- tau2 * sigma * v

    for (j in seq_len(p)) {
      Xj  <- X[, j]
      r_j <- y - X[, -j, drop = FALSE] %*% beta[-j] - theta * v

      prec_slab <- sum(Xj^2 / w) + 1 / slab_var
      var_slab  <- 1 / prec_slab
      mu_slab   <- var_slab * sum(Xj * r_j / w)

      log_m1 <- dnorm(0, mu_slab, sqrt(var_slab + slab_var),
                      log = TRUE) + log(pi0 + 1e-10)
      log_m0 <- dnorm(0, 0, sqrt(slab_var),
                      log = TRUE) + log(1 - pi0 + 1e-10)

      log_odds  <- min(max(log_m1 - log_m0, -500), 500)
      prob_incl <- 1 / (1 + exp(-log_odds))
      prob_incl <- min(max(prob_incl, 1e-10), 1 - 1e-10)

      gamma_j <- rbinom(1, 1, prob_incl)
      beta[j] <- if (gamma_j == 1L) {
        rnorm(1, mu_slab, sqrt(var_slab))
      } else {
        0
      }

      if (it > burnin) {
        gamma_store[it - burnin, j] <- gamma_j
      }
    }

    # 3. Update sigma
    resid2 <- as.numeric(y - X %*% beta)
    shape  <- a0 + 1.5 * n
    rate   <- b0 + sum(resid2^2 / (tau2 * v)) + sum(v)
    sigma  <- 1 / rgamma(1, shape = shape, rate = rate)
    sigma  <- max(sigma, 1e-10)

    # 4. Update pi0 with informative sparse prior
    n_in <- sum(beta != 0)
    pi0  <- rbeta(1, a_pi + n_in, b_pi + p - n_in)

    # store post-burnin
    if (it > burnin) {
      i                <- it - burnin
      beta_store[i, ]  <- beta
      gamma_store[i, ] <- as.numeric(beta != 0)
      sigma_store[i]   <- sigma
    }
  }

  # summarise posterior
  pip <- colMeans(gamma_store)

  beta_mean <- sapply(seq_len(p), function(j) {
    in_slab <- gamma_store[, j] == 1
    if (sum(in_slab) == 0) return(0)
    mean(beta_store[in_slab, j])
  })

  beta_ci <- apply(beta_store, 2, quantile,
                   probs = c(0.025, 0.975))

  list(
    beta      = beta_store,
    pip       = pip,
    sigma     = sigma_store,
    beta_mean = beta_mean,
    beta_ci   = t(beta_ci)
  )
}

# Inverse-Gaussian sampler (Michaels et al. 1976)
.rinvgauss <- function(n, mu, lambda) {
  mu     <- pmax(mu, 1e-10)
  lambda <- pmax(lambda, 1e-10)
  z <- rnorm(n)
  y <- z^2
  x <- mu + (mu^2 * y) / (2 * lambda) -
    (mu / (2 * lambda)) *
    sqrt(4 * mu * lambda * y + mu^2 * y^2)
  u <- runif(n)
  ifelse(u <= mu / (mu + x), x, mu^2 / x)
}
