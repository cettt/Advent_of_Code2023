data13 <- c(list(character()), strsplit(readLines("Input/day13.txt"), ""))

gr <- cumsum(sapply(data13, length) == 0)

check_mat <- function(co) {
  res <- c(0L, 0L)
  for (r in 1:(max(Im(co)) - 1)) {
    size <- min(r, max(Im(co)) - r)
    co2 <- co[abs(Im(co) - r - 0.5) < size]
    co3 <- co2[Im(co2) - r <= 0]
    co4 <- co2[Im(co2) - r > 0]
    co_ref <- Re(co3) + (2*r - Im(co3) + 1)*1i
    tar <- length(c(setdiff(co_ref, co4), setdiff(co4, co_ref)))
    if (tar <= 1L) res[tar + 1L] <- r
  }
  return(res)
}

#part 1 and 2------
res <- c(0L, 0L)
for (k in unique(gr)) {
  y <- do.call(rbind, data13[gr == k][-1])
  co <- apply(which(y == "#", arr.ind = TRUE), 1, \(x) x[1]*1i + x[2])
  res <- res + 100 * check_mat(co) + check_mat(Im(co) + Re(co) * 1i)
}

res


