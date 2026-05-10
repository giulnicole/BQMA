#' @importFrom utils data
NULL

# ---------------------------------------------------------------------------
# Internal helpers for probe annotation.
#
# All functions here are unexported. They take a character vector of probe
# IDs and an `array` argument ("450k" or "EPIC"), and return either a flag
# (logical / integer) per input probe in the same order, or a character
# vector of probe IDs to flag.
# ---------------------------------------------------------------------------


# Resolve the minfi annotation object for the requested array.
# Returns the data.frame from minfi::getAnnotation(), or NULL if the
# annotation package is not installed.
.get_array_anno <- function(array = c("450k", "EPIC")) {
  array <- match.arg(array)

  pkg <- if (array == "450k") {
    "IlluminaHumanMethylation450kanno.ilmn12.hg19"
  } else {
    "IlluminaHumanMethylationEPICanno.ilm10b4.hg19"
  }

  if (!requireNamespace(pkg, quietly = TRUE)) return(NULL)
  if (!requireNamespace("minfi", quietly = TRUE)) return(NULL)

  anno_obj <- getExportedValue(pkg, pkg)
  minfi::getAnnotation(anno_obj)
}


# SNP distance to nearest SNP within the probe.
#
# Returns an integer vector, same length and order as `probes`. The
# 450k / EPIC manifest encodes SNP location implicitly via three
# columns (CpG_rs, SBE_rs, Probe_rs) rather than a single distance:
#
#   CpG_rs  non-NA -> SNP at the C of the CpG  (distance 0)
#   SBE_rs  non-NA -> SNP at single-base extension site (distance 1)
#   Probe_rs non-NA -> SNP elsewhere in the ~50 bp probe body (distance 99)
#   all NA          -> no SNP (NA in output)
#
# We collapse this into a single numeric "distance" so the downstream
# classifier (.classify_probes) can treat it uniformly: 0 and 1 fall
# under the PROBLEMATIC threshold (<=2), 99 falls under FLAGGED_snp_body.
.get_snp_distance <- function(probes, array = c("450k", "EPIC")) {
  array <- match.arg(array)
  anno <- .get_array_anno(array)
  if (is.null(anno)) return(rep(NA_integer_, length(probes)))

  # Match probes safely; unmatched -> NA
  hit <- match(probes, rownames(anno))
  out <- rep(NA_integer_, length(probes))
  in_manifest <- !is.na(hit)
  if (!any(in_manifest)) return(out)

  cpg_rs <- as.character(anno[hit[in_manifest], "CpG_rs"])
  sbe_rs <- as.character(anno[hit[in_manifest], "SBE_rs"])
  prb_rs <- as.character(anno[hit[in_manifest], "Probe_rs"])

  has_cpg <- !is.na(cpg_rs) & nzchar(cpg_rs)
  has_sbe <- !is.na(sbe_rs) & nzchar(sbe_rs)
  has_prb <- !is.na(prb_rs) & nzchar(prb_rs)

  # Encode as sentinel distances. Priority: CpG > SBE > probe body.
  d <- rep(NA_integer_, sum(in_manifest))
  d[has_prb] <- 99L     # SNP in probe body
  d[has_sbe] <- 1L      # SNP at SBE site (overrides body)
  d[has_cpg] <- 0L      # SNP at CpG (overrides everything)

  out[in_manifest] <- d
  out
}


# Cross-reactive probes (Chen 2013 for 450k, McCartney 2016 for EPIC).
# Returns a character vector of probe IDs flagged as cross-reactive, or
# character(0) if DMRcatedata is not available.
.get_crossreactive <- function(array = c("450k", "EPIC")) {
  array <- match.arg(array)
  if (!requireNamespace("DMRcatedata", quietly = TRUE)) return(character(0))

  e <- new.env(parent = emptyenv())
  if (array == "450k") {
    utils::data("chen.crossreactive", package = "DMRcatedata", envir = e)
    return(as.character(e$chen.crossreactive))
  } else {
    utils::data("epic.crossreactive", package = "DMRcatedata", envir = e)
    return(as.character(e$epic.crossreactive))
  }
}


# Sex chromosome probes (chrX, chrY).
.get_sex_chr_probes <- function(array = c("450k", "EPIC")) {
  array <- match.arg(array)
  anno <- .get_array_anno(array)
  if (is.null(anno)) return(character(0))
  rownames(anno)[anno$chr %in% c("chrX", "chrY")]
}


# Genomic location + gene annotation for the output table.
# Returns a data.frame with rownames = probes; probes absent from the
# manifest get NA. Returns an all-NA frame of correct shape if the
# annotation package is unavailable.
.get_locus_info <- function(probes, array = c("450k", "EPIC")) {
  array <- match.arg(array)
  anno <- .get_array_anno(array)

  empty <- data.frame(
    chr     = rep(NA_character_, length(probes)),
    pos     = rep(NA_integer_,   length(probes)),
    gene    = rep(NA_character_, length(probes)),
    feature = rep(NA_character_, length(probes)),
    row.names = probes,
    stringsAsFactors = FALSE
  )
  if (is.null(anno)) return(empty)

  hit <- match(probes, rownames(anno))
  data.frame(
    chr     = as.character(anno$chr[hit]),
    pos     = suppressWarnings(as.integer(anno$pos[hit])),
    gene    = as.character(anno$UCSC_RefGene_Name[hit]),
    feature = as.character(anno$UCSC_RefGene_Group[hit]),
    row.names = probes,
    stringsAsFactors = FALSE
  )
}


# Hierarchical classifier.
#
# Inputs are equal-length vectors over the same probes:
#   snp_dist_bp : integer, NA if no SNP
#   cr_flag     : logical, TRUE = cross-reactive
#   xy_flag     : logical, TRUE = chrX / chrY
# threshold:
#   snp_dist_problematic : SNPs at distance <= this are "at CpG"
#                          and mark the probe PROBLEMATIC. Default 2.
#
# Rules (highest priority wins):
#   PROBLEMATIC      <- xy_flag OR cr_flag OR (snp_dist_bp <= threshold)
#   FLAGGED_snp_body <- snp_dist_bp >  threshold (and not PROBLEMATIC)
#   CLEAN_CpG        <- otherwise
.classify_probes <- function(snp_dist_bp, cr_flag, xy_flag,
                             snp_dist_problematic = 2L) {
  n <- length(snp_dist_bp)
  cls <- rep("CLEAN_CpG", n)

  snp_at_cpg  <- !is.na(snp_dist_bp) & snp_dist_bp <= snp_dist_problematic
  snp_in_body <- !is.na(snp_dist_bp) & snp_dist_bp >  snp_dist_problematic

  cls[snp_in_body] <- "FLAGGED_snp_body"
  cls[snp_at_cpg | cr_flag | xy_flag] <- "PROBLEMATIC"

  factor(cls, levels = c("CLEAN_CpG", "FLAGGED_snp_body", "PROBLEMATIC"))
}
