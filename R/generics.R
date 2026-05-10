#' @importFrom methods setGeneric setMethod
NULL

#' Extract posterior inclusion probabilities
#'
#' @param x       A \code{BQMAResult} object.
#' @param tau     Numeric. A single quantile value to extract. NULL returns all.
#' @param filter  Character vector of \code{probe_class} values to keep
#'   (e.g. \code{"CLEAN_CpG"}). NULL returns all probes. Ignored if
#'   \code{annotate_results()} has not been called.
#' @param threshold Numeric. Minimum PIP value to return. NULL returns all.
#' @return A matrix of PIPs (CpGs x tau), optionally filtered.
#' @export
setGeneric("getPIP",
           function(x, tau = NULL, filter = NULL, threshold = NULL)
             standardGeneric("getPIP")
)

#' @describeIn getPIP Method for BQMAResult
#' @export
setMethod("getPIP", "BQMAResult", function(x, tau = NULL,
                                           filter = NULL, threshold = NULL) {
  pip <- if (is.null(tau)) {
    x@pip
  } else {
    idx <- which(abs(x@tau - tau) < 1e-6)
    if (length(idx) == 0) stop("tau value not found in fitted model.")
    x@pip[, idx, drop = FALSE]
  }

  # Applica probe_class filter
  if (!is.null(filter) && length(x@probe_class) > 0) {
    keep <- names(x@probe_class)[x@probe_class %in% filter]
    pip  <- pip[rownames(pip) %in% keep, , drop = FALSE]
  }

  # Applica soglia PIP (su almeno una colonna)
  if (!is.null(threshold)) {
    keep <- apply(pip, 1, max) >= threshold
    pip  <- pip[keep, , drop = FALSE]
  }

  pip
})


#' Extract posterior mean beta coefficients
#'
#' Method for \code{BQMAResult} of the \code{getBeta} generic imported
#' from \pkg{minfi}.
#'
#' @param object A \code{BQMAResult} object.
#' @param tau    Numeric. A single quantile value. NULL returns all.
#' @param filter Character vector of \code{probe_class} values to keep.
#'   NULL returns all probes.
#' @return A matrix of posterior means (CpGs x tau).
#' @importFrom minfi getBeta
#' @name getBeta
NULL

#' @rdname getBeta
#' @exportMethod getBeta
setMethod("getBeta", "BQMAResult", function(object, tau = NULL, filter = NULL) {
  beta <- if (is.null(tau)) {
    object@beta_mean
  } else {
    idx <- which(abs(object@tau - tau) < 1e-6)
    if (length(idx) == 0) stop("tau value not found in fitted model.")
    object@beta_mean[, idx, drop = FALSE]
  }

  if (!is.null(filter) && length(object@probe_class) > 0) {
    keep <- names(object@probe_class)[object@probe_class %in% filter]
    beta <- beta[rownames(beta) %in% keep, , drop = FALSE]
  }

  beta
})


#' Extract posterior credible intervals
#'
#' @param x       A \code{BQMAResult} object.
#' @param tau     Numeric. A single quantile value. NULL returns all.
#' @param filter  Character vector of \code{probe_class} values to keep.
#'   NULL returns all probes.
#' @return A matrix with columns lower and upper (95 percent credible interval).
#' @export
setGeneric("getCI",
           function(x, tau = NULL, filter = NULL)
             standardGeneric("getCI")
)

#' @describeIn getCI Method for BQMAResult
#' @export
setMethod("getCI", "BQMAResult", function(x, tau = NULL, filter = NULL) {
  ci <- if (is.null(tau)) {
    x@beta_ci
  } else {
    idx <- which(abs(x@tau - tau) < 1e-6)
    if (length(idx) == 0) stop("tau value not found in fitted model.")
    ci_out <- x@beta_ci[, idx, ]
    colnames(ci_out) <- c("lower", "upper")
    ci_out
  }

  if (!is.null(filter) && length(x@probe_class) > 0) {
    keep <- names(x@probe_class)[x@probe_class %in% filter]
    if (is.matrix(ci)) {
      ci <- ci[rownames(ci) %in% keep, , drop = FALSE]
    } else {
      ci <- ci[dimnames(ci)[[1]] %in% keep, , , drop = FALSE]
    }
  }

  ci
})
