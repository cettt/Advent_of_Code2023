x <- as.matrix(read.fwf("Input/day14.txt", widths = rep(1, 100), comment.char = ""))


tilt <- function(x) {
  for (j in 1:ncol(x)) {
    for (i in 1:nrow(x)) {
      if ((x[i, j]) == "O") {
        if (any(x[seq_len(i-1), j] == ".")) {
          x[i, j] <- "."
          if (all(x[seq_len(i-1), j] == ".")) {
            x[1L, j] <- "O"
          } else x[i + 1L - which(cumsum(x[(i-1):1, j] != ".") == 1)[1], j] <- "O"
        }
      }
    }
  }
  x
}

#part 1------
sum(nrow(x) - which(tilt(x) == "O", arr.ind = TRUE)[,1] + 1L)

#part2------
l <- c()
sc <- c()
while (length(l) < 200) {
  xn <- tilt(x)
  xw <- t(tilt(t(xn)))
  xs <- tilt(xw[nrow(x):1, ])[nrow(x):1,]
  xe <- t(tilt(t(xs)[nrow(x):1, ])[nrow(x):1, ])
  x <- xe
  l <- c(l, sum(t(which(x == "O", arr.ind = T)) * c(ncol(x), 1)))
  # if (anyDuplicated(l)) break
  sc <- c(sc, sum(nrow(x) - which(x == "O", arr.ind = TRUE)[,1] + 1L))
}


k <- diff(which(l == l[200]))[1]  # cycle length
sc[200-k + which((1e9 - (200-k + 1):200) %% k == 0)]


