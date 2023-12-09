data09 <- lapply(strsplit(readLines("Input/day09.txt"), " "), as.integer)

extend <- function(x, n = length(x))  {
  l <- sapply(1:(n - 1L), \(d) diff(x, d = d)[c(n - d, 1L)] * c(1L, (-1L)^d))
  rowSums(l) + x[c(n, 1L)]
}

#part1 and 2--------
rowSums(sapply(data09, extend))
