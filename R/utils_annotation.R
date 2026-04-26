#' @importFrom utils data
NULL

.get_crossreactive <- function(array) {
  e <- new.env(parent = emptyenv())
  if (array == "450k") {
    utils::data("chen.crossreactive", package = "DMRcatedata", envir = e)
    return(as.character(e$chen.crossreactive))
  } else {
    utils::data("epic.crossreactive", package = "DMRcatedata", envir = e)
    return(as.character(e$epic.crossreactive))
  }
}

.get_sex_chr_probes <- function(array) {
  if (array == "450k") {
    anno <- minfi::getAnnotation(
      IlluminaHumanMethylation450kanno.ilmn12.hg19::IlluminaHumanMethylation450kanno.ilmn12.hg19
    )
  } else {
    anno <- minfi::getAnnotation(
      IlluminaHumanMethylationEPICanno.ilm10b4.hg19::IlluminaHumanMethylationEPICanno.ilm10b4.hg19
    )
  }
  rownames(anno)[anno$chr %in% c("chrX", "chrY")]
}
