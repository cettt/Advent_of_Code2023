data09 <- lapply(strsplit(readLines("Input/day09.txt"), " "), as.integer)

f <- function(x)  {
  
  l <- c(list(x[c(1, length(x))]), lapply(seq_along(x[-1]), \(d) diff(x, difference = d)[c(1, length(x) - d)]))
  l <- l[sapply(l, \(x) any(x != 0))]
  
  c(sum(sapply(l, \(x) x[2])), sum(sapply(seq_along(l), \(k) l[[k]][1] * (-1)^(k+1))))
  
}


#part1 and 2--------
rowSums(sapply(data09, f))
