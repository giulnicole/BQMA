# R/BQMA.R

NULL

#' Fit a Bayesian Quantile Methylation Analysis model
#'
#' Fits a Bayesian sparse regression model on methylation array data.
#' For continuous phenotypes uses an Asymmetric Laplace likelihood with
#' quantile regression. For binary phenotypes uses Bayesian probit
#' regression with Albert and Chib (1993) data augmentation.
#' Both models use a spike-slab prior for variable selection with
#' an informative sparse Beta prior on the inclusion probability.
#'
#' @param X Matrix of M-values (samples x CpGs).
#'   Rows are samples, columns are CpG probes.
#' @param y Numeric phenotype vector (length = nrow(X)).
#'   For \code{family = "binary"} must be coded as 0/1.
#' @param tau Numeric vector of quantiles to fit.
#'   Default \code{c(0.25, 0.5, 0.75)}.
#'   Ignored when \code{family = "binary"}.
#' @param family Character. Either \code{"continuous"} for quantile
#'   regression with Asymmetric Laplace likelihood, or \code{"binary"}
#'   for probit regression. Default \code{"continuous"}.
#' @param n_iter Integer. MCMC iterations after burnin. Default 3000.
#' @param burnin Integer. Burnin iterations. Default 1000.
#' @param spike_var Numeric. Spike prior variance. Default 1e-4.
#' @param slab_var Numeric or NULL. Slab prior variance.
#'   If NULL (default) auto-tuned: 0.1 for continuous, 2/p for binary.
#' @param expected_incl Numeric. Expected proportion of associated probes.
#'   Used to set the Beta prior on inclusion probability.
#'   Default 0.05 (5 percent sparsity).
#' @param verbose Logical. Print progress messages. Default TRUE.
#'
#' @return A \code{BQMAResult} object with slots:
#'   \code{pip} (posterior inclusion probabilities),
#'   \code{beta_mean} (posterior mean effects),
#'   \code{beta_ci} (95 percent credible intervals),
#'   \code{tau}, \code{n_iter}, \code{burnin}, \code{call}.
#'
#' @references
#' Kozumi H, Kobayashi G (2011). Gibbs sampling methods for
#' Bayesian quantile regression. Journal of Statistical Computation
#' and Simulation, 81(11), 1565-1578.
#'
#' Albert JH, Chib S (1993). Bayesian analysis of binary and
#' polychotomous response data. Journal of the American Statistical
#' Association, 88(422), 669-679.
#'
#' @examples
#' set.seed(42)
#' X <- matrix(rnorm(47 * 50), nrow = 47, ncol = 50)
#' colnames(X) <- paste0("cg", seq_len(50))
#'
#' # binary phenotype
#' y_bin <- rbinom(47, 1, 0.5)
#' fit_bin <- BQMA(X, y_bin,
#'                 family = "binary",
#'                 n_iter = 100L,
#'                 burnin = 50L)
#' show(fit_bin)
#'
#' # continuous phenotype
#' y_con <- rnorm(47)
#' fit_con <- BQMA(X, y_con,
#'                 family = "continuous",
#'                 tau    = c(0.25, 0.5, 0.75),
#'                 n_iter = 100L,
#'                 burnin = 50L)
#' show(fit_con)
#'
#' @export
BQMA <- function(X, y,
                 tau           = c(0.25, 0.5, 0.75),
                 family        = c("continuous", "binary"),
                 n_iter        = 3000L,
                 burnin        = 1000L,
                 spike_var     = 1e-4,
                 slab_var      = NULL,
                 expected_incl = 0.05,
                 verbose       = TRUE) {

  family <- match.arg(family)

  # input checks
  if (!is.matrix(X))
    X <- as.matrix(X)
  if (!is.numeric(y))
    stop("y must be numeric.")
  if (nrow(X) != length(y))
    stop("nrow(X) must equal length(y).")
  if (any(is.na(X)) || any(is.na(y)))
    stop("NA values detected. Remove or impute before fitting.")
  if (expected_incl <= 0 || expected_incl >= 1)
    stop("expected_incl must be between 0 and 1.")

  if (family == "binary") {
    if (!all(y %in% c(0, 1)))
      stop("For family='binary' y must be coded as 0/1.")
    if (length(unique(y)) < 2)
      stop("y must have both 0 and 1 values.")
    tau <- 0.5
    if (verbose)
      message("BQMA: family = 'binary'  Bayesian probit with ",
              "spike-slab prior (Albert & Chib 1993).")
  }

  if (family == "continuous") {
    if (any(tau <= 0) || any(tau >= 1))
      stop("tau values must be strictly between 0 and 1.")
    if (verbose)
      message("BQMA: family = 'continuous'  Bayesian quantile ",
              "regression with AL likelihood.")
  }

  # auto-tune slab_var per family
  if (is.null(slab_var) && family == "continuous") {
    slab_var <- 0.1
    if (verbose)
      message("BQMA: auto-tuned slab_var = 0.1 for continuous family.")
  }

  # scale X internally
  X_sc  <- scale(X)
  sc_sd <- attr(X_sc, "scaled:scale")
  sc_mu <- attr(X_sc, "scaled:center")

  # remove zero-variance probes
  bad <- !is.finite(colSums(X_sc))
  if (any(bad)) {
    if (verbose)
      message("BQMA: removing ", sum(bad), " zero-variance probes.")
    X_sc  <- X_sc[, !bad, drop = FALSE]
    sc_sd <- sc_sd[!bad]
    sc_mu <- sc_mu[!bad]
  }

  p     <- ncol(X_sc)
  n_tau <- length(tau)

  # compute Beta prior parameters for sparsity
  expected_k    <- max(1, round(expected_incl * p))
  prior_weight  <- 1.0
  a_pi          <- prior_weight * expected_incl
  b_pi          <- prior_weight * (1 - expected_incl)

  if (verbose)
    message("BQMA: sparsity prior Beta(",
            round(a_pi, 3), ", ", round(b_pi, 3),
            ") expected ", expected_k, "/", p, " probes.")


  if (verbose)
    message("BQMA: sparsity prior  expected ", expected_k,
            "/", p, " probes associated. ",
            "Beta(", round(a_pi, 2), ", ", round(b_pi, 2), ")")

  # storage
  pip_mat  <- matrix(0, p, n_tau,
                     dimnames = list(colnames(X_sc),
                                     paste0("tau_", tau)))
  beta_mat <- matrix(0, p, n_tau,
                     dimnames = list(colnames(X_sc),
                                     paste0("tau_", tau)))
  ci_arr   <- array(0, dim = c(p, n_tau, 2),
                    dimnames = list(colnames(X_sc),
                                    paste0("tau_", tau),
                                    c("lower", "upper")))

  # dispatch to correct sampler
  if (family == "binary") {

    if (verbose) message("BQMA: fitting binary probit model...")

    fit_k <- .gibbs_probit(
      X         = X_sc,
      y         = y,
      n_iter    = as.integer(n_iter),
      burnin    = as.integer(burnin),
      spike_var = spike_var,
      slab_var  = 1.0,        # forzato 1.0 temporaneamente
      a_pi      = a_pi,
      b_pi      = b_pi
    )

    pip_mat[, 1]   <- fit_k$pip
    beta_mat[, 1]  <- fit_k$beta_mean / sc_sd
    ci_arr[, 1, 1] <- fit_k$beta_ci[, 1] / sc_sd
    ci_arr[, 1, 2] <- fit_k$beta_ci[, 2] / sc_sd

  } else {

    for (k in seq_along(tau)) {
      if (verbose)
        message("BQMA: fitting tau = ", tau[k],
                " (", k, "/", n_tau, ")")

      fit_k <- .gibbs_AL(
        X         = X_sc,
        y         = y,
        tau       = tau[k],
        n_iter    = as.integer(n_iter),
        burnin    = as.integer(burnin),
        spike_var = spike_var,
        slab_var  = slab_var,
        a_pi      = a_pi,
        b_pi      = b_pi
      )

      pip_mat[, k]   <- fit_k$pip
      beta_mat[, k]  <- fit_k$beta_mean / sc_sd
      ci_arr[, k, 1] <- fit_k$beta_ci[, 1] / sc_sd
      ci_arr[, k, 2] <- fit_k$beta_ci[, 2] / sc_sd
    }
  }

  # assemble BQMAResult
  new("BQMAResult",
      tau       = tau,
      pip       = pip_mat,
      beta_mean = beta_mat,
      beta_ci   = ci_arr,
      n_iter    = as.integer(n_iter),
      burnin    = as.integer(burnin),
      call      = match.call()
  )
}
