x <- unname(unlist(read.fwf("Input/day21.txt", widths = rep(1, 131), comment.char = "")))
n <- 131L


gr <- which(x != "#")

find_adj <- function(k, n) {
  m <- k %% n
  res <- k + c(if (k > n) -n, if (k < (n^2 - n)) n, if (m != 1) -1, if (m != 0) 1)
  res[res %in% gr]
}


lookup <- sapply(seq_along(x), find_adj, n = n)

cur <- which(x == "S")

for (k in 1:201) {
  cur <- unique(unlist(lookup[cur]))
  if (length(cur) == length(gr)) break
}

length(cur)


#part2----------
# three extensions
x <- as.matrix(read.fwf("Input/day21.txt", widths = rep(1, 131), comment.char = ""))

x2 <- as.character(do.call(rbind, lapply(1:3, \(y) cbind(x, x, x))))

gr2 <- which(x2 != "#")


26501365 / (5 * 11)
