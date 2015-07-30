\name{swarmx}
\alias{swarmx}
\alias{swarmy}
\title{ Adjust 1-d data to separate coincident points }
\description{Take a series of points lying in a horizontal or vertical line, and jitter them in the other dimension such that no points are overlapping. }
\usage{
swarmx(x, y, 
    xsize = xinch(0.08, warn.log = FALSE), 
    ysize = yinch(0.08, warn.log = FALSE),
    log = NULL, cex = par("cex"), side = 0L, 
    priority = c("ascending", "descending", "density", "random", "none"))
swarmy(x, y, 
    xsize = xinch(0.08, warn.log = FALSE), 
    ysize = yinch(0.08, warn.log = FALSE),
    log = NULL, cex = par("cex"), side = 0L, 
    priority = c("ascending", "descending", "density", "random", "none"))
}
\arguments{
  \item{x, y}{ Coordinate vectors in any format supported by \code{\link{xy.coords}}. }
  \item{xsize, ysize}{ Width and height of the plotting character in user coordinates. }
  \item{log}{ Character string indicating which axes are logarithmic, as in \code{\link{plot.default}}, or \code{NULL} to figure it out automatically.}
  \item{cex}{ Relative plotting character size.}
  \item{side}{ Direction to perform jittering: 0: both directions; 1: to the right or upwards; -1: to the left or downwards.}
  \item{priority}{ Method used to perform point layout (see below).}
}
\details{ 
For \code{swarmx}, the input coordinates must lie in a vertical line.  For \code{swarmy}, the input coordinates must lie in a horizontal line.

\code{swarmx} adjusts coordinates to the left or right; \code{swarmy} adjusts coordinates up or down.

\code{priority} controls the order in which the points are placed; this has generally has a noticeable effect on the resulting appearance. \code{"ascending"} gives the "traditional" beeswarm plot in which the points are placed in an ascending order. \code{"descending"} is the opposite. \code{"density"} prioritizes points with higher local density. \code{"random"} places points in a random order. \code{"none"} places points in the order provided.

Usually it makes sense to call this function after a plotting device has already been set up (e.g. when adding points to an existing plot), so that the default values for \code{xsize}, \code{ysize}, and \code{log} will be appropriate.
}
\value{ A data frame with columns \code{x} and \code{y} with the new coordinates. }
\seealso{ \code{\link{beeswarm}}, \code{\link{jitter}} }
\examples{ 

## Plot points in one dimension
index <- rep(0, 100)
values <- rnorm(100)
plot(index, values, xlim = c(-0.5, 2.5))
points(swarmx(index + 1, values), col = 2)
points(swarmx(index + 2, values, cex = 1.5), col = 3, cex = 1.5)

## Try the horizontal direction, with a log scale
plot(values, index, log = "x", ylim = c(-1, 2))
points(swarmy(values, index + 1), col = 2)

## Newer examples using "side" and "priority"
plot(c(-0.5, 3.5), range(values), type = 'n')
points(swarmx(index + 0, values), col = 1)
points(swarmx(index + 0.9, values, side = -1), col = 2)
points(swarmx(index + 1.1, values, side =  1, priority = "descending"), col = 3)
points(swarmx(index + 2  , values, priority = 'density'), col = 4)
points(swarmx(index + 3  , values, priority = 'random'), col = 5)

}
\keyword{ dplot }
