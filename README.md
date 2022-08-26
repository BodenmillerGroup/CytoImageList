# CytoImageList

<!-- badges: start -->
[![codecov](https://codecov.io/gh/BodenmillerGroup/CytoImageList/branch/main/graph/badge.svg)](https://codecov.io/gh/BodenmillerGroup/CytoImageList)
[![docs](https://github.com/BodenmillerGroup/CytoImageList/workflows/docs/badge.svg?branch=main)](https://github.com/BodenmillerGroup/CytoImageList/actions?query=workflow%3Adocs)
<!-- badges: end -->

**Of note: this package is under development**

S4 class for storing multiple single- and multi-channel images in R.

Its official package page can be found here: [https://bioconductor.org/packages/CytoImageList](https://bioconductor.org/packages/CytoImageList)

## Check status

| Bioc branch | Checks |
|:-----------:|:------:|
| Release     |[![build-check-release](https://github.com/BodenmillerGroup/CytoImageList/workflows/build-checks-release/badge.svg)](https://github.com/BodenmillerGroup/cytomapper/actions?query=workflow%3Abuild-checks-release)|
| Devel       |[![build-check-devel](https://github.com/BodenmillerGroup/CytoImageList/workflows/build-checks-devel/badge.svg)](https://github.com/BodenmillerGroup/cytomapper/actions?query=workflow%3Abuild-checks-devel)|


## Introduction


## Installation

The `CytoImageList` package can be installed from `Bioconductor` via:

```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
    
BiocManager::install("CytoImageList")
```

The development version of the `CytoImageList` package can be installed from Github using `remotes` in R.
Please make sure to also install its dependecies:

```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

# install.packages("remotes")

remotes::install_github("BodenmillerGroup/CytoImageList", build_vignettes = TRUE, dependencies = TRUE)
```

To load the package in your R session, type the following:

```r
library(CytoImageList)
```


## Citation

Please cite `CytoImageList` as:

```
Nils Eling, Nicolas Damond, Tobias Hoch, Bernd Bodenmiller (2020). cytomapper: an R/Bioconductor package for visualization of highly
  multiplexed imaging data. Bioinformatics, doi: 10.1093/bioinformatics/btaa1061
```

## Maintainer

[Nils Eling](https://github.com/nilseling) nils.eling 'at' dqbm.uzh.ch




