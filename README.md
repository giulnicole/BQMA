
## Bayesian Quantile Methylation Analysis

## Overview

BQMA (Bayesian Quantile Methylation Analysis) implements Bayesian sparse
regression for Illumina methylation arrays. It provides posterior
inclusion probabilities (PIP) for each CpG probe, quantile-specific
effect profiles for continuous phenotypes, and a probit model for binary
phenotypes. The spike-slab prior performs variable selection directly,
returning a probability of association rather than a p-value. This
vignette demonstrates the full workflow on simulated data that mimics a
real methylation array experiment, then shows how to compare results
with limma.

## Installation

``` r
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("BQMA")
```

## Simulating methylation data

We simulate a dataset with 200 samples and 100 CpG probes. Ten probes
are truly associated with a binary phenotype (case/control). The
remaining 90 probes are null. This setup mimics a small methylation
array experiment and allows us to evaluate method performance against
known ground truth.

``` r
library(BQMA)

set.seed(42)
n <- 200
p <- 100

X <- matrix(rnorm(n * p), nrow = n, ncol = p)
colnames(X) <- paste0("cg", seq_len(p))

# Ten truly associated probes with moderate effects
beta_true            <- rep(0, p)
beta_true[1:5]       <- c(0.8, -0.8, 0.6, -0.6, 0.7)
beta_true[6:10]      <- c(-0.7, 0.5, -0.5, 0.6, -0.6)

# Binary phenotype generated from a probit model
eta <- X %*% beta_true
y   <- rbinom(n, 1, pnorm(eta))

cat("Cases    :", sum(y == 1), "\n")
#> Cases    : 111
cat("Controls :", sum(y == 0), "\n")
#> Controls : 89
cat("Signal probes  :", sum(beta_true != 0), "\n")
#> Signal probes  : 10
cat("Null probes    :", sum(beta_true == 0), "\n")
#> Null probes    : 90
```

The correlation between the true signal probes and the phenotype
confirms that the simulated signal is detectable but moderate, as
expected in real EWAS data.

``` r
cor_true  <- mean(abs(cor(X[, 1:10],  y)))
cor_false <- mean(abs(cor(X[, 11:100], y)))

cat("Mean correlation - signal probes:", round(cor_true,  3), "\n")
#> Mean correlation - signal probes: 0.212
cat("Mean correlation - null probes  :", round(cor_false, 3), "\n")
#> Mean correlation - null probes  : 0.062
```

## Fitting BQMA for a binary phenotype

The main function is `BQMA()`. For a binary phenotype coded as 0/1 we
use `family = "binary"`, which fits a Bayesian probit model with a
spike-slab prior using Albert and Chib (1993) data augmentation and the
George and McCulloch (1993) marginal Bayes factor for variable
selection.

The `expected_incl` argument sets the prior probability that any given
probe is associated with the phenotype. The default of 0.05 encodes the
expectation that roughly 5 percent of tested probes are truly
associated, which is a reasonable assumption for most EWAS.

``` r
fit <- BQMA(
  X             = X,
  y             = y,
  family        = "binary",
  n_iter        = 3000L,
  burnin        = 1000L,
  expected_incl = 0.05,
  verbose       = FALSE
)

show(fit)
#> BQMA fit
#>   Quantiles : 0.5 
#>   CpGs      : 100 
#>   Iterations: 3000 (burnin: 1000 )
#>   PIP>=0.8  : 10 (per quantile)
```

## Extracting results

The main output is the posterior inclusion probability (PIP) for each
probe. A PIP close to 1 means the data strongly support inclusion of
that probe in the model. A PIP close to 0 means the probe is likely
null. We use a threshold of 0.5 as a default decision boundary.

``` r
library(minfi)
#> Loading required package: BiocGenerics
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
#> Loading required package: stats4
#> Loading required package: S4Vectors
#> 
#> Attaching package: 'S4Vectors'
#> The following object is masked from 'package:utils':
#> 
#>     findMatches
#> The following objects are masked from 'package:base':
#> 
#>     expand.grid, I, unname
#> Loading required package: IRanges
#> 
#> Attaching package: 'IRanges'
#> The following object is masked from 'package:grDevices':
#> 
#>     windows
#> Loading required package: GenomeInfoDb
#> Loading required package: SummarizedExperiment
#> Loading required package: MatrixGenerics
#> Loading required package: matrixStats
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
#> Loading required package: XVector
#> 
#> Attaching package: 'Biostrings'
#> The following object is masked from 'package:base':
#> 
#>     strsplit
#> Loading required package: bumphunter
#> Loading required package: foreach
#> Loading required package: iterators
#> Loading required package: parallel
#> Loading required package: locfit
#> locfit 1.5-9.12   2025-03-05
#> Setting options('download.file.method.GEOquery'='auto')
#> Setting options('GEOquery.inmemory.gpl'=FALSE)
#> 
#> Attaching package: 'minfi'
#> The following object is masked from 'package:BQMA':
#> 
#>     getBeta
pip  <- getPIP(fit)[, 1]
beta <- BQMA::getBeta(fit)[, 1]
ci   <- getCI(fit)

cat("Probes with PIP > 0.5:", sum(pip > 0.5), "\n")
#> Probes with PIP > 0.5: 12
cat("Probes with PIP > 0.8:", sum(pip > 0.8), "\n")
#> Probes with PIP > 0.8: 9

# Top 10 probes ordered by PIP
top10 <- order(pip, decreasing = TRUE)[1:10]
# estrai CI correttamente dall'array 3D
ci_mat <- getCI(fit)[, 1, ]   # CpGs x 2 -- prendi il primo tau

results <- data.frame(
  probe   = names(pip)[top10],
  pip     = round(pip[top10], 3),
  beta    = round(beta[top10], 3),
  ci_low  = round(ci_mat[top10, 1], 3),
  ci_high = round(ci_mat[top10, 2], 3),
  is_true = names(pip)[top10] %in% paste0("cg", 1:10)
)

print(results)
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
