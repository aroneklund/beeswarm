# bxplot.R
#
# Aron Charles Eklund
##
# A part of the "beeswarm" R package
# 



bxplot <- function (x, ...) 
  UseMethod("bxplot")

bxplot.default <- function(x, probs = c(0.25, 0.5, 0.75),
    vertical = TRUE, horizontal = !vertical, add = FALSE,
    col = par("col"), lty = par("lty"), lwd = NULL, 
    at = NULL, width = 0.75, ...) {
  if(is.numeric(x)) {
    x <- list(x)
  }
  n <- length(x)
  n.probs <- length(probs)
  if(is.null(lwd)) {  ## default is a thick line at the median
    lwd <- rep(par('lwd'), length.out = n.probs)
    if(0.5 %in% probs) lwd[probs == 0.5] <- par('lwd') * 3
  }
  y <- lapply(x, quantile, probs = probs, na.rm = TRUE)
  if(is.null(at)) at <- 1:n
  if(!add) {
    boxplot(x, horizontal = horizontal, at = at, 
      pars = list(whisklty = 0, staplelty = 0, outpch = NA, 
        boxlty = 0, medlty = 0), ...)
  }
  hw <- width / 2  # half-width
  if(horizontal) {
    for (i in 1:n) {
      segments(y0 = at[i] - hw, y1 = at[i] + hw, x0 = y[[i]], 
        col = col, lwd = lwd, lty = lty)
    }
  } else {
    for (i in 1:n) {
      segments(x0 = at[i] - hw, x1 = at[i] + hw, y0 = y[[i]], 
        col = col, lwd = lwd, lty = lty)
    }
  }
}  

bxplot.formula <- function (formula, data = NULL, ..., subset, na.action = NULL) 
{
    if (missing(formula) || (length(formula) != 3L)) 
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame()))) 
        m$data <- as.data.frame(data)
    m$... <- NULL
    m$na.action <- na.action
    m[[1L]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    response <- attr(attr(mf, "terms"), "response")
    bxplot(split(mf[[response]], mf[-response]), ...)
}


