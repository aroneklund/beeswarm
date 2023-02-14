library(beeswarm)

test_swarms <- function(x) {
  for (na_positions in list(integer(0), c(2L,4L,6L,8L))) {
    x[na_positions] <- NA
    for (compact in c(FALSE, TRUE)) {
      for (priority in c("ascending", "descending", "density", "random", "none")) {
        for (side in -1:1) {
          for (epsilon in c(0, 0.00001, 0.1)) {
            if (epsilon > 0 & !compact) {
              next;
            }
            print(na_positions)
            print(compact)
            print(priority)
            print(side)
            print(epsilon)
            # compare R and C versions of swarmy and swarmx
            set.seed(1)
            y1 <- swarmy(x, numeric(length(x)), compact=compact, side=side, priority=priority, fast=TRUE, epsilon=epsilon)$y
            set.seed(1)
            y2 <- swarmy(x, numeric(length(x)), compact=compact, side=side, priority=priority, fast=FALSE, epsilon=epsilon)$y
            stopifnot(all.equal(y1, y2))
            stopifnot(identical(which(is.na(y1)), na_positions))

            set.seed(1)
            x1 <- swarmx(numeric(length(x)), x, compact=compact, side=side, priority=priority, fast=TRUE, epsilon=epsilon)$x
            set.seed(1)
            x2 <- swarmx(numeric(length(x)), x, compact=compact, side=side, priority=priority, fast=FALSE, epsilon=epsilon)$x
            stopifnot(all.equal(x1, x2))
            stopifnot(identical(which(is.na(x1)), na_positions))
          }
        }
      }
    }
  }
}

set.seed(1)
test_swarms(rnorm(250) / 100)
test_swarms(numeric(10))
test_swarms(1:10)
