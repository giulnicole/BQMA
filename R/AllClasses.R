#' @importFrom methods setClass setGeneric setMethod new is
NULL

#' BQMAResult S4 class
#'
#' Stores the output of a BQMA model fit.
#'
#' @slot tau         Numeric vector of quantiles fitted.
#' @slot pip         Matrix (CpGs x tau) of posterior inclusion probabilities.
#' @slot beta_mean   Matrix (CpGs x tau) of posterior mean effects.
#' @slot beta_ci     Array (CpGs x tau x 2) of 95 percent credible intervals.
#' @slot n_iter      Integer. MCMC iterations after burnin.
#' @slot burnin      Integer. Burnin iterations discarded.
#' @slot call        The matched call.
#' @slot probe_class Named character vector of probe annotations. Empty if
#'   \code{annotate_results()} has not been called.
#'
#' @exportClass BQMAResult
setClass("BQMAResult",
         representation(
           tau         = "numeric",
           pip         = "matrix",
           beta_mean   = "matrix",
           beta_ci     = "array",
           n_iter      = "integer",
           burnin      = "integer",
           call        = "call",
           probe_class = "character"   # named vector; character(0) = not yet annotated
         ),
         prototype(
           probe_class = character(0)
         )
)

#' Show method for BQMAResult
#' @param object A \code{BQMAResult} object.
setMethod("show", "BQMAResult", function(object) {
  cat("BQMA fit\n")
  cat("  Quantiles :", paste(object@tau, collapse = ", "), "\n")
  cat("  CpGs      :", nrow(object@pip), "\n")
  cat("  Iterations:", object@n_iter, "(burnin:", object@burnin, ")\n")
  sig <- colSums(object@pip >= 0.8)
  cat("  PIP>=0.8  :", paste(sig, collapse = ", "), "(per quantile)\n")
  if (length(object@probe_class) > 0) {
    tbl <- table(object@probe_class)
    cat("  Probe QC  :",
        paste(names(tbl), tbl, sep = "=", collapse = "  "), "\n")
  }
})
