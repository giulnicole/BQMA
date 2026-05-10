#' Annotate probes from a BQMA fit with QC flags and genomic context
#'
#' Takes a [BQMAResult-class] object and returns a tidy `data.frame` with
#' one row per probe per quantile, containing posterior summaries
#' together with reliability annotation drawn from the Illumina manifest,
#' the Chen / McCartney cross-reactive probe lists (via DMRcatedata),
#' and chromosome location.
#'
#' Each probe is classified as:
#' \describe{
#'   \item{`CLEAN_CpG`}{interpretable as epigenetic signal}
#'   \item{`FLAGGED_snp_body`}{SNP in the probe body, distance > 2 bp from
#'     the CpG / SBE site}
#'   \item{`PROBLEMATIC`}{SNP at or adjacent to the CpG (distance 0-2 bp),
#'     cross-reactive probe, or probe on chrX / chrY}
#' }
#' The classification is hierarchical: any `PROBLEMATIC` condition
#' overrides `FLAGGED_snp_body`. The individual underlying flags
#' (`snp_dist_bp`, `cr_flag`, `xy_flag`) are also returned so the user can
#' apply a different rule downstream.
#'
#' Output is in long format: one row per (probe x tau). For the typical
#' median-only case (`tau = 0.5` or `family = "binary"`) this is one row
#' per probe; for multi-quantile fits each probe appears once per
#' quantile.
#'
#' To attach the classification back onto the [BQMAResult-class] object
#' (for use by `show()` and downstream methods), use [add_annotation()].
#'
#' @param object A [BQMAResult-class] object.
#' @param array  Array type: `"450k"` or `"EPIC"`.
#' @param snp_dist_problematic Integer. SNPs at distance <= this (in bp)
#'   from the CpG site mark the probe as `PROBLEMATIC`. Default 2.
#'
#' @return A `data.frame` with columns:
#'   `probe`, `tau`, `pip`, `beta_mean`, `ci_low`, `ci_high`,
#'   `probe_class`, `snp_dist_bp`, `cr_flag`, `xy_flag`,
#'   `gene`, `feature`, `chr`, `pos`.
#'
#' @references
#' Chen YA et al. (2013) Discovery of cross-reactive probes and
#' polymorphic CpGs in the Illumina Infinium HumanMethylation450
#' microarray. Epigenetics 8(2): 203-209.
#'
#' McCartney DL et al. (2016) Identification of polymorphic and
#' off-target probe binding sites on the Illumina Infinium
#' MethylationEPIC BeadChip. Genomics Data 9: 22-24.
#'
#' @seealso [add_annotation()] to write the classification onto the
#'   `BQMAResult` slot.
#'
#' @export
annotate_results <- function(object,
                             array = c("450k", "EPIC"),
                             snp_dist_problematic = 2L) {

  if (!is(object, "BQMAResult"))
    stop("`object` must be a BQMAResult.")

  array <- match.arg(array)
  snp_dist_problematic <- as.integer(snp_dist_problematic)

  probes <- rownames(object@pip)
  if (is.null(probes) || !length(probes))
    stop("BQMAResult has no probe names in rownames(pip).")

  # per-probe annotation layer
  snp_dist_bp <- .get_snp_distance(probes, array)

  cr_set  <- .get_crossreactive(array)
  cr_flag <- probes %in% cr_set

  xy_set  <- .get_sex_chr_probes(array)
  xy_flag <- probes %in% xy_set

  probe_class <- .classify_probes(snp_dist_bp, cr_flag, xy_flag,
                                  snp_dist_problematic = snp_dist_problematic)

  locus <- .get_locus_info(probes, array)

  # posterior summaries: long format, one row per (probe x tau)
  tau_vec <- object@tau
  n_p     <- length(probes)
  n_tau   <- length(tau_vec)

  data.frame(
    probe       = rep(probes,  times = n_tau),
    tau         = rep(tau_vec, each  = n_p),
    pip         = as.vector(object@pip),
    beta_mean   = as.vector(object@beta_mean),
    ci_low      = as.vector(object@beta_ci[, , 1]),
    ci_high     = as.vector(object@beta_ci[, , 2]),
    probe_class = rep(probe_class, times = n_tau),
    snp_dist_bp = rep(snp_dist_bp, times = n_tau),
    cr_flag     = rep(cr_flag,     times = n_tau),
    xy_flag     = rep(xy_flag,     times = n_tau),
    gene        = rep(locus$gene,    times = n_tau),
    feature     = rep(locus$feature, times = n_tau),
    chr         = rep(locus$chr,     times = n_tau),
    pos         = rep(locus$pos,     times = n_tau),
    stringsAsFactors = FALSE
  )
}


#' Attach probe classification onto a BQMAResult
#'
#' Convenience writer: runs [annotate_results()] and copies the per-probe
#' classification onto the `@probe_class` slot of the input object. The
#' slot value is a named character vector (one entry per probe), suitable
#' for display by the `show()` method.
#'
#' Note that the classification is per-probe, not per (probe x tau) — it
#' is determined by the manifest, not by the model fit, so it does not
#' depend on `tau`.
#'
#' @param object A [BQMAResult-class] object.
#' @param array  Array type: `"450k"` or `"EPIC"`.
#' @param snp_dist_problematic Integer. See [annotate_results()].
#'
#' @return The input [BQMAResult-class] object, with `@probe_class`
#'   populated.
#'
#' @seealso [annotate_results()] for the full data.frame output.
#'
#' @export
add_annotation <- function(object,
                           array = c("450k", "EPIC"),
                           snp_dist_problematic = 2L) {

  if (!is(object, "BQMAResult"))
    stop("`object` must be a BQMAResult.")

  array <- match.arg(array)

  df <- annotate_results(object, array = array,
                         snp_dist_problematic = snp_dist_problematic)

  first_tau <- df[df$tau == df$tau[1], , drop = FALSE]
  pc <- as.character(first_tau$probe_class)
  names(pc) <- first_tau$probe
  pc <- pc[rownames(object@pip)]

  object@probe_class <- pc
  object
}
