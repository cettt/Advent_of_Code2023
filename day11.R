data11 <- read.fwf("Input/day11.txt", widths = rep(1, 140), comment.char = "")

nr <- cumsum(rowSums(data11 == "#") == 0L)
nc <- cumsum(colSums(data11 == "#") == 0L)

gal <- which(data11 == "#", arr.ind = TRUE, useNames = FALSE)
gal <- t(cbind(gal, nr[gal[, 1]], nc[gal[, 2]]))

res <- apply(gal, 2L, \(x) colSums(matrix(rowSums(abs(gal - x)), nrow = 2L)))

#part1-------
sum(res) / 2L

#part2-------
sum(res * c(1L, 1e6 - 1L)) / 2L
