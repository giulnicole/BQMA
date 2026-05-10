
# BQMA: Bayesian Quantile Methylation Analysis

BQMA implements Bayesian sparse regression for Illumina methylation
arrays (450k / EPIC). It returns posterior inclusion probabilities (PIP)
per CpG probe, with a probit spike-slab model for binary phenotypes
(Albert & Chib 1993; George & McCulloch 1993) and an Asymmetric Laplace
quantile regression model for continuous phenotypes (Kozumi & Kobayashi
2011). It also annotates probes with QC flags (SNP at CpG,
cross-reactive, sex chromosome) drawn from the Illumina manifest and the
Chen / McCartney probe lists.

## Installation

``` r
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("BQMA")
```

``` r
library(IlluminaHumanMethylation450kanno.ilmn12.hg19)
#> Loading required package: minfi
#> Warning: package 'minfi' was built under R version 4.3.1
#> Loading required package: BiocGenerics
#> Warning: package 'BiocGenerics' was built under R version 4.3.1
#> 
#> Attaching package: 'BiocGenerics'
#> The following objects are masked from 'package:stats':
#> 
#>     IQR, mad, sd, var, xtabs
#> The following objects are masked from 'package:base':
#> 
#>     anyDuplicated, aperm, append, as.data.frame, basename, cbind,
#>     colnames, dirname, do.call, duplicated, eval, evalq, Filter, Find,
#>     get, grep, grepl, intersect, is.unsorted, lapply, Map, mapply,
#>     match, mget, order, paste, pmax, pmax.int, pmin, pmin.int,
#>     Position, rank, rbind, Reduce, rownames, sapply, setdiff, sort,
#>     table, tapply, union, unique, unsplit, which.max, which.min
#> Loading required package: GenomicRanges
#> Warning: package 'GenomicRanges' was built under R version 4.3.1
#> Loading required package: stats4
#> Loading required package: S4Vectors
#> Warning: package 'S4Vectors' was built under R version 4.3.2
#> 
#> Attaching package: 'S4Vectors'
#> The following object is masked from 'package:utils':
#> 
#>     findMatches
#> The following objects are masked from 'package:base':
#> 
#>     expand.grid, I, unname
#> Loading required package: IRanges
#> Warning: package 'IRanges' was built under R version 4.3.1
#> 
#> Attaching package: 'IRanges'
#> The following object is masked from 'package:grDevices':
#> 
#>     windows
#> Loading required package: GenomeInfoDb
#> Warning: package 'GenomeInfoDb' was built under R version 4.3.3
#> Loading required package: SummarizedExperiment
#> Warning: package 'SummarizedExperiment' was built under R version 4.3.1
#> Loading required package: MatrixGenerics
#> Warning: package 'MatrixGenerics' was built under R version 4.3.1
#> Loading required package: matrixStats
#> Warning: package 'matrixStats' was built under R version 4.3.3
#> 
#> Attaching package: 'MatrixGenerics'
#> The following objects are masked from 'package:matrixStats':
#> 
#>     colAlls, colAnyNAs, colAnys, colAvgsPerRowSet, colCollapse,
#>     colCounts, colCummaxs, colCummins, colCumprods, colCumsums,
#>     colDiffs, colIQRDiffs, colIQRs, colLogSumExps, colMadDiffs,
#>     colMads, colMaxs, colMeans2, colMedians, colMins, colOrderStats,
#>     colProds, colQuantiles, colRanges, colRanks, colSdDiffs, colSds,
#>     colSums2, colTabulates, colVarDiffs, colVars, colWeightedMads,
#>     colWeightedMeans, colWeightedMedians, colWeightedSds,
#>     colWeightedVars, rowAlls, rowAnyNAs, rowAnys, rowAvgsPerColSet,
#>     rowCollapse, rowCounts, rowCummaxs, rowCummins, rowCumprods,
#>     rowCumsums, rowDiffs, rowIQRDiffs, rowIQRs, rowLogSumExps,
#>     rowMadDiffs, rowMads, rowMaxs, rowMeans2, rowMedians, rowMins,
#>     rowOrderStats, rowProds, rowQuantiles, rowRanges, rowRanks,
#>     rowSdDiffs, rowSds, rowSums2, rowTabulates, rowVarDiffs, rowVars,
#>     rowWeightedMads, rowWeightedMeans, rowWeightedMedians,
#>     rowWeightedSds, rowWeightedVars
#> Loading required package: Biobase
#> Warning: package 'Biobase' was built under R version 4.3.1
#> Welcome to Bioconductor
#> 
#>     Vignettes contain introductory material; view with
#>     'browseVignettes()'. To cite Bioconductor, see
#>     'citation("Biobase")', and for packages 'citation("pkgname")'.
#> 
#> Attaching package: 'Biobase'
#> The following object is masked from 'package:MatrixGenerics':
#> 
#>     rowMedians
#> The following objects are masked from 'package:matrixStats':
#> 
#>     anyMissing, rowMedians
#> Loading required package: Biostrings
#> Warning: package 'Biostrings' was built under R version 4.3.3
#> Loading required package: XVector
#> Warning: package 'XVector' was built under R version 4.3.1
#> 
#> Attaching package: 'Biostrings'
#> The following object is masked from 'package:base':
#> 
#>     strsplit
#> Loading required package: bumphunter
#> Warning: package 'bumphunter' was built under R version 4.3.1
#> Loading required package: foreach
#> Loading required package: iterators
#> Loading required package: parallel
#> Loading required package: locfit
#> Warning: package 'locfit' was built under R version 4.3.3
#> locfit 1.5-9.12   2025-03-05
#> Setting options('download.file.method.GEOquery'='auto')
#> Setting options('GEOquery.inmemory.gpl'=FALSE)
```

## Quick start

A minimal end-to-end example: simulate a binary EWAS, fit BQMA, extract
PIPs, then annotate the result.

``` r
library(BQMA)

set.seed(42)
n <- 200
p <- 100

X <- matrix(rnorm(n * p), nrow = n, ncol = p)
colnames(X) <- paste0("cg", seq_len(p))

# 10 truly associated probes
beta_true       <- rep(0, p)
beta_true[1:5]  <- c( 0.8, -0.8,  0.6, -0.6,  0.7)
beta_true[6:10] <- c(-0.7,  0.5, -0.5,  0.6, -0.6)

# Binary phenotype from a probit model
y <- rbinom(n, 1, pnorm(X %*% beta_true))

fit <- BQMA(X, y,
            family        = "binary",
            n_iter        = 3000L,
            burnin        = 1000L,
            expected_incl = 0.05,
            verbose       = FALSE)

show(fit)
#> BQMA fit
#>   Quantiles : 0.5 
#>   CpGs      : 100 
#>   Iterations: 3000 (burnin: 1000 )
#>   PIP>=0.8  : 10 (per quantile)
```

## Extracting results

``` r
pip  <- getPIP(fit)[, 1]
beta <- BQMA::getBeta(fit)[, 1]   # qualified to avoid conflict with minfi::getBeta
ci   <- getCI(fit)[, 1, ]         # CpGs x 2, first tau

cat("Probes with PIP > 0.5:", sum(pip > 0.5), "\n")
#> Probes with PIP > 0.5: 12
cat("Probes with PIP > 0.8:", sum(pip > 0.8), "\n")
#> Probes with PIP > 0.8: 9

top10 <- order(pip, decreasing = TRUE)[1:10]

data.frame(
  probe   = names(pip)[top10],
  pip     = round(pip[top10],     3),
  beta    = round(beta[top10],    3),
  ci_low  = round(ci[top10, 1],   3),
  ci_high = round(ci[top10, 2],   3),
  is_true = names(pip)[top10] %in% paste0("cg", 1:10)
)
#>      probe   pip   beta ci_low ci_high is_true
#> cg2    cg2 1.000 -0.867 -1.306  -0.464    TRUE
#> cg5    cg5 1.000  0.941  0.546   1.357    TRUE
#> cg9    cg9 1.000  0.845  0.498   1.259    TRUE
#> cg8    cg8 0.996 -0.782 -1.212  -0.374    TRUE
#> cg10  cg10 0.982 -0.797 -1.262  -0.257    TRUE
#> cg16  cg16 0.941  0.716  0.000   1.213   FALSE
#> cg6    cg6 0.916 -0.603 -1.007   0.000    TRUE
#> cg36  cg36 0.862 -0.672 -1.108   0.000   FALSE
#> cg1    cg1 0.820  0.513  0.000   0.861    TRUE
#> cg76  cg76 0.800 -0.477 -0.776   0.000   FALSE
```

## Annotating probes with QC flags

`annotate_results()` returns a tidy `data.frame` with the posterior
summaries plus per-probe QC classification (`CLEAN_CpG`,
`FLAGGED_snp_body`, `PROBLEMATIC`) and genomic context (gene, chr, pos).
Use real Illumina probe IDs (cgXXXXXXXX from the 450k or EPIC manifest)
for the annotation lookup to return non-NA flags.

``` r
ann_df <- annotate_results(fit, array = "450k")
#> Warning in utils::data("chen.crossreactive", package = "DMRcatedata", envir =
#> e): data set 'chen.crossreactive' not found
table(ann_df$probe_class)
#> 
#>        CLEAN_CpG FLAGGED_snp_body      PROBLEMATIC 
#>              100                0                0


# Filter to reliable hits
reliable <- ann_df[ann_df$pip >= 0.5 &
                   ann_df$probe_class == "CLEAN_CpG", ]
```

To attach the classification onto the `BQMAResult` object (so that
`show(fit)` prints the QC summary), use `add_annotation()`:

``` r
fit <- add_annotation(fit, array = "450k")
#> Warning in utils::data("chen.crossreactive", package = "DMRcatedata", envir =
#> e): data set 'chen.crossreactive' not found
show(fit)
#> BQMA fit
#>   Quantiles : 0.5 
#>   CpGs      : 100 
#>   Iterations: 3000 (burnin: 1000 )
#>   PIP>=0.8  : 10 (per quantile)
#>   Probe QC  : CLEAN_CpG=100
```

## Documentation

See the package vignette for the full workflow on real probe IDs from
`minfiData`, plus a comparison against limma:

``` r
#vignette("How-to-use-BMQA_new", package = "BQMA")
```

## References

- Albert JH, Chib S (1993). Bayesian analysis of binary and
  polychotomous response data. *JASA* 88(422), 669–679.
- George EI, McCulloch RE (1993). Variable selection via Gibbs sampling.
  *JASA* 88(423), 881–889.
- Kozumi H, Kobayashi G (2011). Gibbs sampling methods for Bayesian
  quantile regression. *J. Stat. Comput. Simul.* 81(11), 1565–1578.
- Chen YA et al. (2013). Discovery of cross-reactive probes and
  polymorphic CpGs in the Illumina HumanMethylation450 microarray.
  *Epigenetics* 8(2), 203–209.
- McCartney DL et al. (2016). Identification of polymorphic and
  off-target probe binding sites on the Illumina MethylationEPIC
  BeadChip. *Genomics Data* 9, 22–24.
