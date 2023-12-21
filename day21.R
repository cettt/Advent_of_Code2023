n <- 131L
data21 <- unlist(read.fwf("Input/day21.txt", widths = rep(1L, n), com = ""))

gr <- unname(which(data21 != "#")) # graph

find_adj <- function(k, m) {
  m <- k %% n
  res <- k + c(if (k > n) -n, if (k < n * (n - 1L)) n, if (m != 1L) -1L, if (m != 0L) 1L)
  fld <- data21[res]
  res[fld != "#"]
}

lookup <- lapply(seq_along(data21), find_adj)

walk <- function(stp, strt) {
  cur <- strt
  for (k in 1:stp) cur <- unique(unlist(lookup[cur]))
  cur
}

length(walk(64L, which(data21 == "S")))

#part2----------
cur2 <- walk(132L, which(data21 == "S"))
n2 <- length(cur2) # number of plots in starting field after even number of steps
n1 <- length(walk(1L, cur2)) # number of plots in starting field after odd number of steps

N <- 26501365L %/% n  #202300 :D

n_even <- N^2
n_odd <- (N - 1)^2

tmp  <- sapply(c(66L,  n^2 - 65L, 65*n + 1L, 66*n), \(x) length(walk(130L, x)))
corner <- c(1L, n, n*n + 1L - n, n*n) # corner tiles

tmp2 <- sapply(corner, \(x) length(walk(64L, x)))
tmp3 <- sapply(corner, \(x) length(walk(64L + 131L, x)))

res <- c(n_even * n2, n_odd * n1, sum(tmp), (N - 1) * sum(tmp3), N * sum(tmp2))

sprintf("%.f", sum(res))
