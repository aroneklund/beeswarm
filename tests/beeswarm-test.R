library(beeswarm)

test_swarms <- function(x) {
  for (na_positions in list(integer(0), c(2L,4L,6L,8L))) {
    x[na_positions] <- NA
    for (compact in c(FALSE, TRUE)) {
      for (priority in c("ascending", "descending", "density", "random", "none")) {
        for (side in -1:1) {
          print(na_positions)
          print(compact)
          print(priority)
          print(side)
          # compare R and C versions of swarmy and swarmx
          set.seed(1)
          y1 <- swarmy(x, numeric(length(x)), compact=compact, side=side, priority=priority, fast=TRUE)$y
          set.seed(1)
          y2 <- swarmy(x, numeric(length(x)), compact=compact, side=side, priority=priority, fast=FALSE)$y
          stopifnot(all.equal(y1, y2))
          # No values should be NA after swarm, regardless of NA in the other direction
          stopifnot(!any(is.na(y1)))

          set.seed(1)
          x1 <- swarmx(numeric(length(x)), x, compact=compact, side=side, priority=priority, fast=TRUE)$x
          set.seed(1)
          x2 <- swarmx(numeric(length(x)), x, compact=compact, side=side, priority=priority, fast=FALSE)$x
          stopifnot(all.equal(x1, x2))
          # No values should be NA after swarm, regardless of NA in the other direction
          stopifnot(!any(is.na(x1)))
        }
      }
    }
  }
}

set.seed(1)
test_swarms(rnorm(250) / 100)
test_swarms(numeric(10))
test_swarms(1:10)

# Ensure no error with infinite values (GitHub issue #19)
stopifnot(any(swarmx(x = rep(1, 5), y = c(rep(-Inf, 3), 0, 1))$x > 1))
stopifnot(any(swarmy(x = c(rep(-Inf, 3), 0, 1), y = rep(1, 5))$y > 1))

# Confirm that xToFinite() works as expected
stopifnot(xToFinite(NA, 10) == -200)
stopifnot(xToFinite(Inf, 10) == 100)
stopifnot(xToFinite(-Inf, 10) == -100)
stopifnot(xToFinite(c(-Inf, 1), 10) == c(-99, 1))
stopifnot(xToFinite(c(-Inf, Inf, NA, 1), 10) == c(-99, 101, -199, 1))
