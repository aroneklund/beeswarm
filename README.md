# beeswarm

[![CRAN status](https://www.r-pkg.org/badges/version/beeswarm)](https://cran.r-project.org/package=beeswarm)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/beeswarm)](https://cran.r-project.org/package=beeswarm)

An R package implementing bee swarm plots

## Installation

You can install the latest release on CRAN like this:

```r
install.packages("beeswarm")
```

You can install the latest development version from GitHub like this:

```r
# install.packages("remotes")
remotes::install_github("aroneklund/beeswarm")
```

## Related works

* [**ggbeeswarm**](https://cran.r-project.org/package=ggbeeswarm)
bee swarm and related plots in the ggplot2 framework

## It's also known as a dot plot

Leland Wilkinson (1999). Dot plots. The American Statistician. 53(3):276-281. 
[Paper](https://www.tandfonline.com/doi/abs/10.1080/00031305.1999.10474474)

It seems to be the same basic point layout algorithm, except that Wilkinson suggests an additional smoothing step that the beeswarm package does not do.
