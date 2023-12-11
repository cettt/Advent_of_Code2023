data11 <- read.fwf("Input/day11.txt", widths = rep(1, 140), comment.char = "")

nr <- which(rowSums(data11 == "#") == 0L)
nc <- unname(which(colSums(data11 == "#") == 0L))

gal <- apply(unname(which(data11 == "#", arr.ind = TRUE)), 1, \(x) x, simplify = FALSE)

find_d <- function(x){
  y1 <- pmin(x[[1]], x[[2]])
  y2 <- pmax(x[[1]], x[[2]])
  c(sum(y2 - y1), sum(y1[1] <= nr & nr <= y2[1]) + sum(y1[2] <= nc & nc <= y2[2]))
}

res <- combn(gal, 2, find_d2)

#part1-------
sum(res)

#part2-------
sum(res * c(1L, 1e6 - 1L))
