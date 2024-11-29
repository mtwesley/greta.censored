
<!-- README.md is generated from README.Rmd. Please edit that file -->

## greta.censored: Censored Distributions for `greta`

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/greta.censored)](https://CRAN.R-project.org/package=greta.censored)
[![Codecov test
coverage](https://codecov.io/gh/mtwesley/greta.censored/branch/main/graph/badge.svg)](https://app.codecov.io/gh/mtwesley/greta.censored?branch=main)
[![R-CMD-check](https://github.com/mtwesley/greta.censored/workflows/R-CMD-check/badge.svg)](https://github.com/mtwesley/greta.censored/actions)
<!-- badges: end -->

## Overview

`greta.censored` is an R package that extends
[`greta`](https://github.com/greta-dev/greta) to support the following
distributions with right, left, and interval censoring:

- Normal
- Log-Normal
- Student’s T
- Gamma
- Exponential
- Weibull
- Pareto
- Beta

## Installation

You can install the stable version of `greta.censored` from CRAN with:

``` r
install.packages("greta.censored")
```

Alternatively, you can install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("mtwesley/greta.censored")
```

## Usage

Here are some examples demonstrating how to use the main functions of
the `greta.censored` package:

### Example 1: Censored Normal Distribution

``` r
library(greta)
library(greta.censored)

# Define the parameters
mean <- as.greta_array(0)
sd <- as.greta_array(1)
is_censored <- as.greta_array(c(0, 1, 0))
censor <- "right"
lower <- -Inf
upper <- 1

# Create the censored normal distribution
dist <- normal_censored(mean, sd, is_censored, censor, lower, upper)

# Print the distribution
print(dist)
```

### Example 2: Censored Log-Normal Distribution

``` r
library(greta)
library(greta.censored)

# Define the parameters
meanlog <- as.greta_array(0)
sdlog <- as.greta_array(1)
is_censored <- as.greta_array(c(0, 1, 0))
censor <- "left"
lower <- 0
upper <- Inf

# Create the censored log-normal distribution
dist <- lognormal_censored(meanlog, sdlog, is_censored, censor, lower, upper)

# Print the distribution
print(dist)
```

## Code of Conduct

Please note that the `greta.censored` project is released with a [Code
of
Conduct](https://github.com/mtwesley/greta.censored/blob/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## Citation

If you use the `greta.censored` package, please cite it as follows:

Wesley M (2024). *greta.censored: Censored Distributions for ‘greta’*. R
package version 0.1.0,
<https://CRAN.R-project.org/package=greta.censored>.

Or use the following:

``` r
citation("greta.censored")
```

Or in BibTeX format:

    @Manual{
      title = {greta.censored: Censored Distributions for 'greta'},
      author = {Mlen-Too Wesley},
      year = {2024},
      note = {R package version 0.1.0},
      url = {https://CRAN.R-project.org/package=greta.censored},
    }

Alternatively, refer to the package’s CRAN page: [greta.censored on
CRAN](https://CRAN.R-project.org/package=greta.censored).
