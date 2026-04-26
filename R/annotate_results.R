#' Annotate probes in a BQMAResult object
#'
#' Classifies each probe into one of three categories:
#' \itemize{
#'   \item \code{CLEAN_CpG}   — no known issues
#'   \item \code{FLAGGED}     — cross-reactive or SNP within \code{snp_dist} bp
#'   \item \code{PROBLEMATIC} — located on sex chromosomes (chrX/chrY)
#' }
#' Classification is hierarchical: \code{PROBLEMATIC} overrides \code{FLAGGED}.
#'
#' @param object      A \code{BQMAResult} object.
#' @param array       Array type: \code{"450k"} or \code{"EPIC"}.
#' @param sex_chr     Logical. Flag probes on chrX/chrY as PROBLEMATIC.
#' @param crossreactive Logical. Flag cross-reactive probes as FLAGGED.
#' @param snp_dist    Integer. Flag probes with a SNP within this many bp of
#'   the CpG site as FLAGGED. Set to \code{NULL} to skip SNP filtering.
#' @param ...         Currently unused.
#' @return The input \code{BQMAResult} object with \code{probe_class} slot
#'   populated.
#' @export
setGeneric("annotate_results",
           function(object, array = c("450k", "EPIC"),
                    sex_chr = TRUE, crossreactive = TRUE,
                    snp_dist = 2L, ...)
             standardGeneric("annotate_results")
)

#' @describeIn annotate_results Method for BQMAResult
#' @export
setMethod("annotate_results", "BQMAResult",
          function(object, array = c("450k", "EPIC"),
                   sex_chr = TRUE, crossreactive = TRUE,
                   snp_dist = 2L, ...) {

            array  <- match.arg(array)
            probes <- rownames(object@pip)

            probe_class <- rep("CLEAN_CpG", length(probes))
            names(probe_class) <- probes

            # ── Livello 1: SNP vicini al CpG ──────────────────────────────────────
            if (!is.null(snp_dist) &&
                requireNamespace("IlluminaHumanMethylation450kanno.ilmn12.hg19",
                                 quietly = TRUE) &&
                requireNamespace("minfi", quietly = TRUE)) {

              anno <- minfi::getAnnotation(
                IlluminaHumanMethylation450kanno.ilmn12.hg19::IlluminaHumanMethylation450kanno.ilmn12.hg19
              )
              snp_probes <- rownames(anno)[
                !is.na(anno$Probe_rs) &
                  !is.na(anno$Probe_rs_distance) &
                  anno$Probe_rs_distance <= snp_dist
              ]
              probe_class[intersect(probes, snp_probes)] <- "FLAGGED"

            } else if (!is.null(snp_dist)) {
              message("SNP annotation skipped: IlluminaHumanMethylation450kanno.ilmn12.hg19 ",
                      "or minfi not available.")
            }

            # ── Livello 2: Cross-reactive ──────────────────────────────────────────
            if (crossreactive) {
              if (requireNamespace("DMRcatedata", quietly = TRUE)) {
                cr_probes <- .get_crossreactive(array)
                probe_class[intersect(probes, cr_probes)] <- "FLAGGED"
              } else {
                message("Cross-reactive annotation skipped: DMRcatedata not available.")
              }
            }

            # ── Livello 3: Cromosomi sessuali (sovrascrive FLAGGED) ────────────────
            if (sex_chr) {
              if (requireNamespace("minfi", quietly = TRUE)) {
                sex_probes <- .get_sex_chr_probes(array)
                probe_class[intersect(probes, sex_probes)] <- "PROBLEMATIC"
              } else {
                message("Sex chromosome annotation skipped: minfi not available.")
              }
            }

            object@probe_class <- probe_class
            object
          }
)
