# beeswarm.R
#
# Aron Charles Eklund
#
# A part of the "beeswarm" R package
# 


beeswarm <- function (x, ...) 
  UseMethod("beeswarm")


## here x should be a list or data.frame or numeric
beeswarm.default <- function(x, 
    method = c("swarm", "compactswarm", "center", "hex", "square"),
    vertical = TRUE, horizontal = !vertical, 
    cex = 1, spacing = 1, breaks = NULL,
    labels, at = NULL, 
    corral = c("none", "gutter", "wrap", "random", "omit"),
    corralWidth, side = 0L, 
    priority = c("ascending", "descending", "density", "random", "none"),
    fast = TRUE,
    pch = par("pch"), col = par("col"), bg = NA, 
    pwpch = NULL, pwcol = NULL, pwbg = NULL, pwcex = NULL,
    do.plot = TRUE, add = FALSE, axes = TRUE, log = FALSE, 
    xlim = NULL, ylim = NULL, dlim = NULL, glim = NULL,
    xlab = NULL, ylab = NULL, dlab = "", glab = "",
    ...) {
    
  method <- match.arg(method)
  corral <- match.arg(corral)
  priority <- match.arg(priority)
  if(length(cex) > 1) {
    stop('the parameter "cex" must have length 1')
  }
  stopifnot(side %in% -1:1)
  if(is.numeric(x)) {
    x <- list(x)
  }
  n.groups <- length(x)

  #### Resolve group labels
  if(missing(labels) || is.null(labels)) {
    if(is.null(names(x))) {
      if(n.groups == 1) {
        labels <- NA
      } else {
        labels <- 1:n.groups
      }
    } else {
      labels <- names(x)
    }
  } else {
    labels <- rep(labels, length.out = n.groups)
  }

  if (is.null(at)) 
    at <- 1:n.groups
  else if (length(at) != n.groups) 
    stop(gettextf("'at' must have length equal to %d, the number of groups", 
      n.groups), domain = NA)

  if (is.null(dlab)) 
     dlab <- deparse(substitute(x))

  ## this function returns a "group" vector, to complement "unlist"
  unlistGroup <- function(x, nms = names(x)) rep(nms, sapply(x, length))

  x.val <- unlist(x)
  x.gp <- unlistGroup(x, nms = labels)
  if((range(x.val, finite = TRUE)[1] <= 0) && log)
    warning('values <= 0 omitted from logarithmic plot')
  
  n.obs <- length(x.val)
  n.obs.per.group <- lengths(x)
  
  #### Resolve xlim, ylim, dlim, xlab, ylab
  if(is.null(dlim)) {
      if(log) {
        dlim <- 10 ^ (extendrange(log10(x.val[x.val > 0])))
      } else {
        dlim <- extendrange(x.val, f = 0.01)
      }
  } else if (length(dlim) != 2) {
    stop ("'dlim' must have length 2")
  }
  if(is.null(glim)) {
    glim <- c(min(at) - 0.5, max(at) + 0.5)
  } else if (length(glim) != 2) {
    stop ("'glim' must have length 2")
  }
  if(horizontal) {        ## plot is horizontal
    if(is.null(ylim)) 
      ylim <- glim
    if(is.null(xlim)) {
      xlim <- dlim
    } else {
      dlim <- xlim
    }
    if (is.null(xlab)) 
      xlab <- dlab
    if (is.null(ylab)) 
      ylab <- glab
  } else {                ## plot is vertical
    if(is.null(xlim)) 
      xlim <- glim
    if(is.null(ylim)) {
      ylim <- dlim
    } else {
      dlim <- ylim
    }
    if (is.null(ylab)) 
      ylab <- dlab
    if (is.null(xlab)) 
      xlab <- glab
  }
  if(length(xlim) != 2)
    stop ("'xlim' must have length 2")
  if(length(ylim) != 2)
    stop ("'ylim' must have length 2")
  
  #### Resolve plotting characters and colors
  if(is.null(pwpch)) {
    pch.out <- unlistGroup(x, nms = rep(pch, length.out = n.groups))
  } else {
    if(is.list(pwpch)) {
      names(pwpch) <- names(x)
      stopifnot(all(sapply(pwpch, length) == n.obs.per.group))
      pch.out <- unlist(pwpch)
    } else {
      pch.out <- pwpch
    }
  }
  stopifnot(length(pch.out) == n.obs)

  if(is.null(pwcol)) {
    col.out <- unlistGroup(x, nms = rep(col, length.out = n.groups))
  } else {
    if(is.list(pwcol)) {
      names(pwcol) <- names(x)
      stopifnot(all(sapply(pwcol, length) == n.obs.per.group))
      col.out <- unlist(pwcol)
    } else {
      col.out <- pwcol
    }
  }
  stopifnot(length(col.out) == n.obs)

  if(is.null(pwbg)) {
    bg.out <- unlistGroup(x, nms = rep(bg, length.out = n.groups))
  } else {
    if(is.list(pwbg)) {
      names(pwbg) <- names(x)
      stopifnot(all(sapply(pwbg, length) == n.obs.per.group))
      bg.out <- unlist(pwbg)
    } else {
      bg.out <- pwbg
    }
  }
  stopifnot(length(bg.out) == n.obs)
 
   if(is.null(pwcex)) {
    cex.out <- unlistGroup(x, nms = rep(1, length.out = n.groups))
  } else {
    if(is.list(pwcex)) {
      names(pwcex) <- names(x)
      stopifnot(all(sapply(pwcex, length) == n.obs.per.group))
      cex.out <- unlist(pwcex)
    } else {
      cex.out <- pwcex
    }
  }
  stopifnot(length(cex.out) == n.obs)
 
  #### Set up the plot
  if(do.plot & !add) {
    plot(xlim, ylim, 
      type = 'n', axes = FALSE, 
      log = ifelse(log, ifelse(horizontal, 'x', 'y'), ''),
      xlab = xlab, ylab = ylab, ...)
  }

  #### Calculate the size of a plotting character along group- or data-axis
  sizeMultiplier <- par('cex') * cex * spacing
  if(horizontal) {
    size.g <- yinch(0.08, warn.log = FALSE) * sizeMultiplier
    size.d <- xinch(0.08, warn.log = FALSE) * sizeMultiplier
  } else {    # vertical
    size.g <- xinch(0.08, warn.log = FALSE) * sizeMultiplier
    size.d <- yinch(0.08, warn.log = FALSE) * sizeMultiplier
  }
  
  ##### Calculate point positions g.pos, d.pos 
  if(method %in% c('swarm', 'compactswarm')) {
    compact <- method == 'compactswarm'
    if(horizontal) {
      g.offset <- lapply(x, function(a) swarmy(x = a, y = rep(0, length(a)), 
          cex = sizeMultiplier, side = side, priority = priority,
          fast = fast, compact = compact)$y)
    } else {
      g.offset <- lapply(x, function(a) swarmx(x = rep(0, length(a)), y = a, 
          cex = sizeMultiplier, side = side, priority = priority,
          fast = fast, compact = compact)$x)
    }
    d.pos <- x
  } else {          ####   non-swarm methods
    ##### first determine positions along the data axis
      if(method == 'hex') size.d <- size.d * sqrt(3) / 2  
      if(log) {              ## if data axis IS on a log scale
        if(is.null(breaks))
          breaks <- 10 ^ seq(log10(dlim[1]), log10(dlim[2]) + size.d, by = size.d)
        if(length(breaks) == 1 && is.na(breaks[1])) {
          d.index <- x
          d.pos <- x
        } else {
          mids <- 10 ^ ((log10(head(breaks, -1)) + log10(tail(breaks, -1))) / 2)
          d.index <- lapply(x, cut, breaks = breaks, labels = FALSE, include.lowest = TRUE)
          d.pos <- lapply(d.index, function(a) mids[a])  
        }
      } else {               ## if data axis is NOT on a log scale
        if(is.null(breaks))
          breaks <- seq(dlim[1], dlim[2] + size.d, by = size.d)
        if(length(breaks) == 1 && is.na(breaks[1])) {
          d.index <- x
          d.pos <- x
        } else {
          mids <- (head(breaks, -1) + tail(breaks, -1)) / 2
          d.index <- lapply(x, cut, breaks = breaks, labels = FALSE, include.lowest = TRUE)
          d.pos <- lapply(d.index, function(a) mids[a])  
        }
      }  
    ##### now determine positions along the group axis
      x.index <- lapply(d.index, function(v) {
        if(length(na.omit(v)) == 0) 
          return(v)
        v.s <- lapply(split(v, v), seq_along)
        if(method %in% c('center', 'square') && side == -1)
          v.s <- lapply(v.s, function(a) a - max(a))
        else if(method %in% c('center', 'square') && side == 1)
          v.s <- lapply(v.s, function(a) a - 1)
        else if(method == 'center')
          v.s <- lapply(v.s, function(a) a - mean(a))
        else if(method == 'square')
          v.s <- lapply(v.s, function(a) a - floor(mean(a)))
        else if(method == 'hex') {
          odd.row <- (as.numeric(names(v.s)) %% 2) == 1
          if(side == 0) {
            v.s[ odd.row] <- lapply(v.s[ odd.row], function(a) a - floor(mean(a)) - 0.25)
            v.s[!odd.row] <- lapply(v.s[!odd.row], function(a) a - ceiling(mean(a)) + 0.25)
          } else if(side == -1) {
            v.s[ odd.row] <- lapply(v.s[ odd.row], function(a) a - max(a))
            v.s[!odd.row] <- lapply(v.s[!odd.row], function(a) a - max(a) - 0.5)
          } else if(side ==  1) {
            v.s[ odd.row] <- lapply(v.s[ odd.row], function(a) a - 1)
            v.s[!odd.row] <- lapply(v.s[!odd.row], function(a) a - 0.5)
          }
        }
        unsplit(v.s, v)
      }) 
      
      g.offset <- lapply(1:n.groups, function(i) x.index[[i]] * size.g)
  }                   ###### end of non-swarm methods

  ##### now check for runaway points (if "corral" has been set)
  if(corral != 'none') {
    if(missing(corralWidth)) {
      if(n.groups > 1) {
        corralWidth <- min(at[-1] - at[-n.groups]) - (2 * size.g)
      } else {
        corralWidth <- 2 * (min(diff(c(par('usr')[1], at, par('usr')[2]))) - size.g)
      }
    } else {
      stopifnot(length(corralWidth) == 1)
      stopifnot(corralWidth > 0)
    }
    corralLo <- (side - 1) * corralWidth / 2
    corralHi <- (side + 1) * corralWidth / 2
    if(corral == 'gutter') {
      g.offset <- lapply(g.offset, function(zz) pmin(corralHi, pmax(corralLo, zz)))
    }
    if(corral == 'wrap') {
      if(side == -1) { ## special case with side=-1: reverse the corral to avoid artifacts at zero
        g.offset <- lapply(g.offset, function(zz) corralHi - ((corralHi - zz) %% corralWidth))
      } else {
        g.offset <- lapply(g.offset, function(zz) ((zz - corralLo) %% corralWidth) + corralLo)
      }
    }  
    if(corral == 'random') {
      g.offset <- lapply(g.offset, function(zz) ifelse(zz > corralHi | zz < corralLo, yes = runif(length(zz), corralLo, corralHi), no = zz))
    }
    if(corral == 'omit') {
      g.offset <- lapply(g.offset, function(zz) ifelse(zz > corralHi | zz < corralLo, yes = NA, no = zz))
    }
  }
  
  g.pos <- lapply(1:n.groups, function(i) at[i] + g.offset[[i]])

  out <- data.frame(x = unlist(g.pos), y = unlist(d.pos), 
                    pch = pch.out, col = col.out, bg = bg.out, cex = cex * cex.out,
                    x.orig = x.gp, y.orig = x.val,
                    stringsAsFactors = FALSE)

  if(do.plot) {
    if(horizontal) {     ## plot is horizontal
      points(out$y, out$x, pch = out$pch, col = out$col, bg = out$bg, cex = out$cex)  
      if(axes & !add) {
        axis(1, ...)
        axis(2, at = at, labels = labels, tick = FALSE, ...)
        box(...)
      }
    } else {             ## plot is vertical
      points(out$x, out$y, pch = out$pch, col = out$col, bg = out$bg, cex = out$cex)  
      if(axes & !add) {
        axis(2, ...)
        axis(1, at = at, labels = labels, tick = FALSE, ...)
        box(...)
      }
    }
  }
  invisible(out)
}

   
  
beeswarm.formula <- function (formula, data = NULL, subset, na.action = NULL, 
    pwpch = NULL, pwcol = NULL, pwbg = NULL, pwcex = NULL, dlab, glab, ...) 
{
    if (missing(formula) || (length(formula) != 3)) 
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame()))) 
        m$data <- as.data.frame(data)
    m$... <- NULL
    m$dlab <- NULL
    m$glab <- NULL
    m$na.action <- na.action
    m[[1]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    response <- attr(attr(mf, "terms"), "response")
    if (missing(dlab)) 
        dlab <- names(mf)[response]
    if (missing(glab)) 
        glab <- as.character(formula)[3]
    f <- mf[-response]
    f <- f[names(f) %in% attr(attr(mf, "terms"), "term.labels")]
    if(!is.null(mf$'(pwpch)')) pwpch <- split(mf$'(pwpch)', f)
    if(!is.null(mf$'(pwcol)')) pwcol <- split(mf$'(pwcol)', f)
    if(!is.null(mf$'(pwbg)')) pwbg <- split(mf$'(pwbg)',f)
    if(!is.null(mf$'(pwcex)')) pwcex <- split(mf$'(pwcex)',f)
    beeswarm(split(mf[[response]], f), 
      pwpch = pwpch, pwcol = pwcol, pwbg = pwbg, pwcex = pwcex,
      dlab = dlab, glab = glab, ...)
}


#### hidden function to do swarm layout
.calculateSwarm <- function(x, dsize, gsize, side = 0L, priority = "ascending") {
  if(length(x) == 0) return(numeric(0))
  stopifnot(side %in% -1:1)
  out <- data.frame(x = x / dsize, y = 0, index = seq_along(x))
  
  #### Determine the order in which points will be placed
  if(     priority == "ascending" ) { out <- out[order( out$x), ] } ## default "smile"
  else if(priority == "descending") { out <- out[order(-out$x), ] } ## frown
  else if(priority == "none") {  } ## do not reorder
  else if(priority == "density") {
  	dens.x <- density(out$x, na.rm = TRUE)  ## compute kernel density estimate
  	dens.interp <- approx(dens.x$x, dens.x$y, xout = out$x, rule = 2)  ## interpolated density
  	out <- out[order(-dens.interp$y), ]  ## arrange outward from densest areas
  }
  else if(priority == "random") {
	out <- out[sample(nrow(out)), ]
  }
  #### place the points
  if(nrow(out) > 1) {
    for (ii in 2:nrow(out)) {          ## we will place one point at a time
      xi <- out$x[ii]
       ## identify previously-placed points with potential to overlap the current point
      isPotOverlap <- (abs(xi - out$x) < 1) & (1:nrow(out) < ii)
      isPotOverlap[is.na(isPotOverlap)] <- FALSE
      if(any(isPotOverlap)) {
        pre.x <- out[isPotOverlap, 'x']
        pre.y <- out[isPotOverlap, 'y']
        poty.off <- sqrt(1 - ((xi - pre.x) ^ 2))         ## potential y offsets
        poty <- switch(side + 2, 
          c(0, pre.y - poty.off),
          c(0, pre.y + poty.off, pre.y - poty.off),
          c(0, pre.y + poty.off)
        )
        poty.bad <- sapply(poty, function(y) {           ## check for overlaps
          any(((xi - pre.x) ^ 2 + (y - pre.y) ^ 2) < 0.999)
        })
        poty[poty.bad] <- Inf
        out$y[ii] <- poty[which.min(abs(poty))]
      } else {
        out$y[ii] <- 0
      }
    }
  }
  out[is.na(out$x), 'y'] <- NA        ## missing x values should have missing y values
  out$y[order(out$index)] * gsize
}


.calculateCompactSwarm <- function(x, dsize, gsize, side = 0L, priority = "ascending") {
  if(length(x) == 0) return(numeric(0))
  stopifnot(side %in% -1:1)

  ## out$y.low: best permitted position <= 0 for each point
  ## out$y.high: best permitted position >= 0 for each point
  ## out$y.best: best permitted position for each point (or Inf for placed points)
  ## out$y.placed: which points have been placed
  out <- data.frame(x = x / dsize, y = 0, index = seq_along(x), placed = FALSE,
                    y.low = 0, y.high = 0, y.best = 0)

  #### Determine the order in which points will be placed
  if(     priority == "ascending" ) { out <- out[order( out$x), ] } ## default "smile"
  else if(priority == "descending") { out <- out[order(-out$x), ] } ## frown
  else if(priority == "none") {  } ## do not reorder
  else if(priority == "density") {
    dens.x <- density(out$x, na.rm = TRUE)  ## compute kernel density estimate
    dens.interp <- approx(dens.x$x, dens.x$y, xout = out$x, rule = 2)  ## interpolated density
    out <- out[order(-dens.interp$y), ]  ## arrange outward from densest areas
  }
  else if(priority == "random") {
    out <- out[sample(nrow(out)), ]
  }
  #### place the points
  if(nrow(out) > 1) {
    for (iter in 1:nrow(out)) {          ## we will place one point at a time
      i <- which.min(abs(out$y.best))    ## Choose a point that can be placed
                                         ## close to non-data axis
      xi <- out$x[i]
      yi <- out$y[i] <- out$y.best[i]
      out$placed[i] <- TRUE
      out$y.best[i] <- Inf               ## Ensure it won't be chosen again
      xdiff = abs(xi - out$x)
      if(side == 0) {
        for (j in which(!out$placed & xdiff < 1)) {
          y.offset <- sqrt(1 - (xdiff[j] ^ 2))
          y.high <- out$y.high[j] <- max(out$y.high[j], yi + y.offset)
          y.low <- out$y.low[j] <- min(out$y.low[j], yi - y.offset)
          out$y.best[j] <- ifelse(-y.low < y.high, y.low, y.high)
        }
      } else if(side == 1) {
        for (j in which(!out$placed & xdiff < 1)) {
          y.offset <- sqrt(1 - (xdiff[j] ^ 2))
          out$y.best[j] <- out$y.high[j] <- max(out$y.high[j], yi + y.offset)
        }
      } else {
        for (j in which(!out$placed & xdiff < 1)) {
          y.offset <- sqrt(1 - (xdiff[j] ^ 2))
          out$y.best[j] <- out$y.low[j] <- min(out$y.low[j], yi - y.offset)
        }
      }
    }
  }
  out[is.na(out$x), 'y'] <- NA        ## missing x values should have missing y values
  out$y[order(out$index)] * gsize
}


.calculateSwarmUsingC <- function(x, dsize, gsize, side = 0L, priority = "ascending",
    compact = FALSE) {
  if(length(x) == 0) return(numeric(0))
  stopifnot(side %in% -1:1)

  out <- data.frame(x = x / dsize, index = seq_along(x))

  #### Determine the order in which points will be placed
  if(     priority == "ascending" ) { out <- out[order( out$x), ] } ## default "smile"
  else if(priority == "descending") { out <- out[order(-out$x), ] } ## frown
  else if(priority == "none") {  } ## do not reorder
  else if(priority == "density") {
    dens.x <- density(out$x, na.rm = TRUE)  ## compute kernel density estimate
    dens.interp <- approx(dens.x$x, dens.x$y, xout = out$x, rule = 2)  ## interpolated density
    out <- out[order(-dens.interp$y), ]  ## arrange outward from densest areas
  }
  else if(priority == "random") {
    out <- out[sample(nrow(out)), ]
  }

  out <- out[!is.na(out$x), ]
  n <- nrow(out)

  #### place the points
  result <- .C(C_calculateSwarm,
               x = as.double(out$x),
               n = n,
               compact = as.logical(compact),
               side = as.integer(side),
               placed = integer(n),         # used internally by C implementations
               workspace = numeric(n * 4),  # used internally by C implementations
               y = numeric(n))
  y <- rep(NA, length(x))
  y[out$index] <- result[[7]] * gsize
  y
}


### jitter points horizontally
swarmx <- function(x, y, 
    xsize = xinch(0.08, warn.log = FALSE), 
    ysize = yinch(0.08, warn.log = FALSE),
    log = NULL, cex = par("cex"), side = 0L, 
    priority = c("ascending", "descending", "density", "random", "none"),
    fast = TRUE,
    compact = FALSE) { 
  priority <- match.arg(priority)
  if(is.null(log)) 
    log <- paste(ifelse(par('xlog'), 'x', ''), ifelse(par('ylog'), 'y', ''), sep = '')
  xlog <- 'x' %in% strsplit(log, NULL)[[1L]]
  ylog <- 'y' %in% strsplit(log, NULL)[[1L]]
  xy <- xy.coords(x = x, y = y, recycle = TRUE, log = log)
  stopifnot((length(unique(xy$x)) <= 1))
  if(xlog) xy$x <- log10(xy$x)
  if(ylog) xy$y <- log10(xy$y)
  if (fast) {
    x.new <- xy$x + .calculateSwarmUsingC(xy$y, dsize = ysize * cex,
      gsize = xsize * cex, side = side, priority = priority, compact = compact)
  } else {
    swarmFn <- ifelse(compact, .calculateCompactSwarm, .calculateSwarm)
    x.new <- xy$x + swarmFn(xy$y, dsize = ysize * cex, gsize = xsize * cex,
      side = side, priority = priority)
  }
  out <- data.frame(x = x.new, y = y)
  if(xlog) out$x <- 10 ^ out$x
  out
}

### jitter points vertically
swarmy <- function(x, y, 
    xsize = xinch(0.08, warn.log = FALSE), 
    ysize = yinch(0.08, warn.log = FALSE),
    log = NULL, cex = par("cex"), side = 0L, 
    priority = c("ascending", "descending", "density", "random", "none"),
    fast = TRUE,
    compact = FALSE) { 
  priority <- match.arg(priority)
  if(is.null(log)) 
    log <- paste(ifelse(par('xlog'), 'x', ''), ifelse(par('ylog'), 'y', ''), sep = '')
  xlog <- 'x' %in% strsplit(log, NULL)[[1L]]
  ylog <- 'y' %in% strsplit(log, NULL)[[1L]]
  xy <- xy.coords(x = x, y = y, recycle = TRUE, log = log)
  stopifnot((length(unique(xy$y)) <= 1))
  if(xlog) xy$x <- log10(xy$x)
  if(ylog) xy$y <- log10(xy$y)
  if (fast) {
    y.new <- xy$y + .calculateSwarmUsingC(xy$x, dsize = xsize * cex,
      gsize = ysize * cex, side = side, priority = priority, compact = compact)
  } else {
    swarmFn <- ifelse(compact, .calculateCompactSwarm, .calculateSwarm)
    y.new <- xy$y + swarmFn(xy$x, dsize = xsize * cex, gsize = ysize * cex,
      side = side, priority = priority)
  }
  out <- data.frame(x = x, y = y.new)
  if(ylog) out$y <- 10 ^ out$y
  out
}
