\name{beeswarm}

\alias{beeswarm}
\alias{beeswarm.default}
\alias{beeswarm.formula}

\title{Bee swarm plot}
\description{
Create a bee swarm plot.  A bee swarm plot is a one-dimensional scatter plot similar to \code{\link{stripchart}}, but with various methods to separate coincident points such that each point is visible.  Also, \code{beeswarm} introduces additional features unavailable in \code{stripchart}, such as the ability to control the color and plotting character of each point. 
}

\usage{
beeswarm(x, \dots)

\method{beeswarm}{formula}(formula, data = NULL, subset, na.action = NULL, 
         pwpch = NULL, pwcol = NULL, pwbg = NULL, dlab, glab, \dots)

\method{beeswarm}{default}(x, 
    method = c("swarm", "center", "hex", "square"), 
    vertical = TRUE, horizontal = !vertical, 
    cex = 1, spacing = 1, breaks = NULL,
    labels, at = NULL, 
    corral = c("none", "gutter", "wrap", "random", "omit"),
    corralWidth, side = 0L, 
    priority = c("ascending", "descending", "density", "random", "none"),
    pch = par("pch"), col = par("col"), bg = NA, 
    pwpch = NULL, pwcol = NULL, pwbg = NULL,
    do.plot = TRUE, add = FALSE, log = FALSE,
    xlim = NULL, ylim = NULL, dlim = NULL, glim = NULL,
    xlab = NULL, ylab = NULL, dlab = "", glab = "",
    \dots)
}

\arguments{
  \item{formula}{A formula, such as \code{y ~ grp}, where \code{y} is a
    numeric vector of data values to be split into groups according to
    the grouping variable \code{grp} (usually a factor).}
  \item{data}{A data.frame (or list) from which the variables in
    \code{formula} should be taken.}
  \item{subset}{An optional vector specifying a subset of observations
    to be used.}
  \item{na.action}{A function which indicates what should happen
    when the data contain \code{NA}s.  The default is to quietly ignore missing
    values in either the response or the group.}
  \item{x}{ A numeric vector, or a data frame or list of numeric vectors, each of which is plotted as an individual swarm.}
  \item{method}{ Method for arranging points (see Details). }
  \item{vertical, horizontal}{ Orientation of the plot. \code{horizontal} takes precedence if both are specified. }
  \item{cex}{ Size of points relative to the default given by \code{par("cex")}. Unlike other plotting functions, this must be a single value.}
  \item{spacing}{ Relative spacing between points.}
  \item{breaks}{ Breakpoints (optional).  If \code{NULL}, breakpoints are chosen automatically. If \code{NA}, bins are not used (similar to \code{stripchart} with \code{method = "stack"}).}
  \item{labels}{ Labels for each group. Recycled if necessary. By default, these are inferred from the data. }
  \item{at}{ Numeric vector giving the locations where the swarms should be drawn; defaults to \code{1:n} where \var{n} is the number of groups. }
  \item{corral}{ Method to adjust points that would be placed outside their own group region (see Details). }
  \item{corralWidth}{ Width of the "corral" in user coordinates. If missing, a sensible value will be chosen. }
    \item{side}{ Direction to perform jittering: 0: both directions; 1: to the right or upwards; -1: to the left or downwards.}
  \item{priority}{ Order used to perform point layout when method is \code{"swarm"}; ignored otherwise (see Details).}
  \item{pch, col, bg}{ Plotting characters and colors, specified by group. Recycled if necessary (see Details). }
  \item{pwpch, pwcol, pwbg}{ \dQuote{Point-wise} plotting characters and colors, specified for each data point (see Details). }
  \item{do.plot}{ Draw a plot? }
  \item{add}{ Add to an existing plot? }
  \item{log}{ Use a logarithmic scale on the data axis? }
  \item{xlim, ylim}{ Limits of the plot. }
  \item{dlim, glim}{ An alternative way to specify limits (see Details). }
  \item{xlab, ylab}{ Axis labels. }
  \item{dlab, glab}{ An alternative way to specify axis labels (see Details). }
  \item{\dots}{ Further arguments passed to \code{\link{plot}}. }
}

\details{ 
Several methods for placing the points are available; each method uses a different algorithm to avoid overlapping points. 

The default method, \code{swarm}, places points in increasing order. If a point would overlap an existing point, it is shifted sideways (along the group axis) by a minimal amount sufficient to avoid overlap. \code{breaks} is ignored.

The other three methods first discretize the values along the data axis, in order to create more efficient packing: \code{square} places the points on a square grid, whereas \code{hex} uses a hexagonal grid.  \code{center} uses a  square grid to produce a symmetric swarm. By default, the number of breakpoints for discretization is determined by a combination of the available plotting area and the plotting character size. The discretization of the data can be explicitly controlled using \code{breaks}. If \code{breaks} is set to \code{NA}, the data will not be grouped into intervals; this may be a sensible option if the data is already discrete.

In contrast to most other plotting functions, changing the size of the graphics device will often change the position of the points.  

The plotting characters and colors can be controlled in two ways. First, the arguments \code{pch}, \code{col} and \code{bg} can specify plotting characters and colors in the same way as \code{\link{stripchart}} and \code{\link{boxplot}}: in short, the arguments apply to each group as a whole (and are recycled if necessary).

Alternatively, the \dQuote{point-wise} characteristics of each individual data point can be controlled using \code{pwpch}, \code{pwcol}, and \code{pwbg}, which override \code{pch}, \code{col} and \code{bg} if these are also specified.  These arguments can be specified as a list or vector.  If supplied using the formula method, the arguments can be specified as part of the formula interface; i.e. they are affected by \code{data} and \code{subset}.

  The \code{dlab} and \code{glab} labels may be used instead of \code{xlab}
  and \code{ylab} if those are not specified.  \code{dlab} applies to the 
  continuous data axis (the Y axis unless \code{horizontal} is \code{TRUE});
  \code{glab} to the group axis. Likewise, \code{dlim} and \code{glim} can be used to specify limits of the axes instead of \code{xlim} or \code{ylim}.

This function is intended to be mostly compatible with calls to \code{\link{stripchart}} or \code{\link{boxplot}}.  Thus, code that works with these functions should work with \code{beeswarm} with minimal modification.

By default, swarms from different groups are not prevented from overlapping. Thus, large data sets, or data sets with uneven distributions, may produce somewhat unpleasing beeswarms. If this is a problem, consider reducing \code{cex}. Another approach is to control runaway points (those that would be plotted outside a region allotted to each group) with the \code{corral} argument: The default, \code{"none"}, does not control runaway points. \code{"gutter"} collects runaway points along the boundary between groups. \code{"wrap"} implements periodic boundaries. \code{"random"} places runaway points randomly in the region. \code{"omit"} omits runaway points. See Examples below.

When using the \code{"swarm"} method, \code{priority} controls the order in which the points are placed; this generally has a noticeable effect on the resulting appearance. \code{"ascending"} gives the "traditional" beeswarm plot in which the points are placed in an ascending order. \code{"descending"} is the opposite. \code{"density"} prioritizes points with higher local density. \code{"random"} places points in a random order. \code{"none"} places points in the order provided.

}

\value{ A data frame with plotting information, invisibly. }

\seealso{ \code{\link{stripchart}}, \code{\link{boxplot}} }

\examples{ 
  ## One of the examples from 'stripchart'
  beeswarm(decrease ~ treatment,
    data = OrchardSprays, log = TRUE, 
    pch = 16, col = rainbow(8))

  ## One of the examples from 'boxplot', with a beeswarm overlay
   boxplot(len ~ dose, data = ToothGrowth,
              main = "Guinea Pigs' Tooth Growth",
              xlab = "Vitamin C dose mg",
              ylab = "Tooth length")  
   beeswarm(len ~ dose, data = ToothGrowth, col = 2, add = TRUE)

  ## Compare the 4 methods
  op <- par(mfrow = c(2,2))
  for (m in c("swarm", "center", "hex", "square")) {
    beeswarm(len ~ dose, data = ToothGrowth, method = m, main = m)
  }
  par(op)

  ## Demonstrate the use of 'pwcol'
  data(breast)
  beeswarm(time_survival ~ ER, data = breast,
    pch = 16, pwcol = 1 + as.numeric(event_survival),
    xlab = "", ylab = "Follow-up time (months)",
    labels = c("ER neg", "ER pos"))
  legend("topright", legend = c("Yes", "No"),
    title = "Censored", pch = 16, col = 1:2)

  ## The list interface
  distributions <- list(runif = runif(100, min = -3, max = 3), 
                        rnorm = rnorm(100),
                        rlnorm = rlnorm(100, sdlog = 0.5))
  beeswarm(distributions, col = 2:4)

  ## Demonstrate 'pwcol' with the list interface 
  myCol <- lapply(distributions, function(x) cut(x, breaks = quantile(x), labels = FALSE))
  beeswarm(distributions, pch = 16, pwcol = myCol)
  legend("bottomright", legend = 1:4, pch = 16, col = 1:4, title = "Quartile")

  ## Demonstrate the 'corral' methods
  par(mfrow = c(2,3))
  beeswarm(distributions, col = 2:4, 
    main = 'corral = "none" (default)')
  beeswarm(distributions, col = 2:4, corral = "gutter", 
    main = 'corral = "gutter"')
  beeswarm(distributions, col = 2:4, corral = "wrap", 
    main = 'corral = "wrap"')
  beeswarm(distributions, col = 2:4, corral = "random", 
    main = 'corral = "random"')
  beeswarm(distributions, col = 2:4, corral = "omit", 
    main = 'corral = "omit"')  
 
  ## Demonstrate 'side' and 'priority'
  par(mfrow = c(2,3))
  beeswarm(distributions, col = 2:4, 
    main = 'Default')
  beeswarm(distributions, col = 2:4, side = -1, 
    main = 'side = -1')
  beeswarm(distributions, col = 2:4, side = 1, 
    main = 'side = 1')
  beeswarm(distributions, col = 2:4, priority = "descending", 
    main = 'priority = "descending"')
  beeswarm(distributions, col = 2:4, priority = "random", 
    main = 'priority = "random"')  
  beeswarm(distributions, col = 2:4, priority = "density", 
    main = 'priority = "density"')  
}

\keyword{ hplot }
