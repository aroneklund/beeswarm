\name{bxplot}

\alias{bxplot}
\alias{bxplot.default}
\alias{bxplot.formula}

\title{Plot quantile lines}

\description{
Plot lines indicating the specified quantiles for each group. This function is intended as a simplified interpretation of \code{\link{boxplot}},
 which can be combined with a \code{\link{beeswarm}} (or \code{\link{stripchart}}) plot.
}

\usage{
bxplot(x, \dots)

\method{bxplot}{formula}(formula, data = NULL, \dots, subset, na.action = NULL)

\method{bxplot}{default}(x, probs = c(0.25, 0.5, 0.75),
    vertical = TRUE, horizontal = !vertical, add = FALSE,
    col = par("col"), lty = par("lty"), lwd = NULL, 
    at = NULL, width = 0.75, \dots)
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
  \item{x}{A numeric vector, or a data frame or list of numeric vectors, each of which is considered as a group.}
  \item{probs}{A numeric vector of probabilities with values in [0,1]}
  \item{vertical, horizontal}{ Orientation of the plot. \code{horizontal} takes precedence if both are specified. }
  \item{add}{Add to an existing plot?}
  \item{col, lty}{Color and line type for each probability.}
  \item{lwd}{Line width for each probability (see below).}
  \item{at}{Numeric vector giving the locations where the swarms should be drawn; defaults to \code{1:n} where \var{n} is the number of groups.}
  \item{width}{Width of the lines.}
  \item{\dots}{Further arguments passed to \code{\link{boxplot}}.}
}
\details{
This function is intended as a minimalistic interpration of \code{\link{boxplot}}; however, the quantiles plotted by \code{bxplot} are not necessarily the same as the hinges plotted by a \code{boxplot}.

Notice that specifying a vector of graphical parameters such as \code{lwd} or \code{col} will refer to each of \code{probs}, \emph{not} to each group in the data (as one might expect by analogy with \code{boxplot}).

If \code{lwd} is \code{NULL}, and if the \code{probs} includes 0.5, \code{lwd} will be set to 3 times \code{par("lwd")} for probs=0.5, and \code{par("lwd")} for the others.  (Thus something resembling the median line and hinges of a boxplot is produced by default.)
}

\value{ None.}

\examples{
  ## bxplot on bottom
  beeswarm(len ~ dose, data = ToothGrowth)
  bxplot(len ~ dose, data = ToothGrowth, add = TRUE)
  
  ## bxplot on top
  bxplot(decrease ~ treatment, data = OrchardSprays, probs = 0.5, col = 2)
  beeswarm(decrease ~ treatment, data = OrchardSprays, add = TRUE)

  ## Show deciles 
  data(breast)
  bxplot(time_survival ~ event_survival, data = breast, 
    probs = seq(0, 1, by = 0.1), col = rainbow(10))
  beeswarm(time_survival ~ event_survival, data = breast, 
    pch = 21, bg = "gray75", add = TRUE)
}

\keyword{ hplot }
