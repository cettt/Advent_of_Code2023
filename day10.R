data10 <- as.matrix(read.fwf("Input/day10.txt", widths = rep(1L, 140L)))
n <- nrow(data10)*2L + 2L
gr <- matrix("", nrow = n, ncol = n)
gr[seq_len(nrow(data10)) * 2L, seq_len(nrow(data10)) * 2L] <- data10


lp <- which(gr == "S")
dir <- 1L

while (TRUE) {
  lp <- c(lp[1] + dir*2:1, lp)
  cur <- gr[lp[1]]
  
  if (cur == "L") {
    dir <- if (dir == 1L) n else -1L 
  }
  else if (cur == "J") {
    dir <- if (dir == 1L) -n else -1L 
  }
  else if (cur == "7") {
    dir <- if (dir == n) 1L else -n 
  }
  else if (cur == "F") {
    dir <- if (dir == -1L) n else 1L 
  } else if (cur == "S") break
}

#part1-------
length(lp[-1]) / 4L

#part 2-------
find_adj <- function(k) {
  m <- k %% n
  k + c(if (k <= n^2 - n) n, if (k > n) -n, if (m != 1L) -1L, if (m != 0L) 1L)
}

lookup <- sapply(seq_along(gr), find_adj)
outside <- which(sapply(lookup, length) < 4L)
cur <- outside

while (length(cur) > 0L) {
  nxt <- unique(unlist(lookup[cur]))
  cur <- nxt[match(nxt, c(outside, lp), 0L) == 0L]
  outside <- c(outside, cur)
}

x <- seq_along(gr)[-c(outside, lp)]
sum((((x - 1L) %/% n) %% 2L == 1L) & x %% 2L == 0L)
