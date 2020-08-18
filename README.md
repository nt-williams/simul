
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simul

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Travis build
status](https://travis-ci.com/nt-williams/simul.svg?branch=master)](https://travis-ci.com/nt-williams/simul)
<!-- badges: end -->

> Fast Simultaneous Confidence Bands Based on the Efficient Influence
> Function and Multiplier Bootstrap

-----

## Installation

You can install simul from [GitHub](https://github.com) with:

``` r
devtools::install_github("nt-williams/simul")
```

## Scope

Compute fast critical values for constructing uniform (simultaneous)
confidence bands and hypothesis tests. The critical value is calculated
using a multiplier bootstrap of the empirical efficient influence
function as described by Kennedy (2019).

## References

Edward H. Kennedy (2019) Nonparametric Causal Effects Based on
Incremental Propensity Score Interventions, Journal of the American
Statistical Association, 114:526, 645-656, DOI:
10.1080/01621459.2017.1422737