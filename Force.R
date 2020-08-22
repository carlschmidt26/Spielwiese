f <- function(y) function() y
lf <- vector("list", 5)
for (i in seq_along(lf)) {
  lf[[i]] <- f(i)}
lf[[1]]()  # returns 5
