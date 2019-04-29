beeswarm
========

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/beeswarm)](https://cran.r-project.org/package=beeswarm)
[![CRAN_Downloads_Badge](http://cranlogs.r-pkg.org/badges/beeswarm)](https://cran.r-project.org/package=beeswarm)

An R package implementing bee swarm plots


You can see some examples here:
http://www.cbs.dtu.dk/~eklund/beeswarm/


Installation
------------

You can install the latest release on CRAN like this:

	install.packages("beeswarm")


Or you can install the latest development version from GitHub like this:

	## install.packages("devtools")
	devtools::install_github("aroneklund/beeswarm")


Related works
-------------

* [**ggbeeswarm**](https://cran.r-project.org/package=ggbeeswarm) 
bee swarm and related plots in the ggplot2 framework
* [**pybeeswarm**](https://github.com/mgymrek/pybeeswarm)
bee swarm plots in Python


It's also known as a dot plot
-----------------------------

Leland Wilkinson (1999). Dot plots. The American Statistician. 53(3):276-281. 
[PDF](https://www.cs.uic.edu/~wilkinson/Publications/dotplots.pdf)

It seems to be the same basic point layout algorithm, except that Wilkinson suggests an additional smoothing step that the beeswarm package does not do.
