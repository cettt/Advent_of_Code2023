x <- read.table("Input/day18.txt", comment.char = "", sep = " ")
dir <- c(R = 1, D = 1i, L = -1, U = -1i)

dig <- function(dirs, lngs) {

  l <- sum(lngs) #length of boundary
  bnd <- cumsum(c(0 + 0i, dir[dirs] * lngs)) # coordinates of boundary 

  ar <- sum((Im(bnd)[-length(bnd)] + Im(bnd[-1])) * diff(Re(bnd))) / 2L # Shoelace formula
  sprintf("%.f", abs(ar) + 1L + l / 2L) #Pick's theorem
}

#part1-----
dig(x[,1], x[,2])

#part2------------
x[,4] <- names(dir)[as.integer(substr(x[,3], 8, 8)) + 1L]
x[,5] <- strtoi(substr(x[,3], 3, 7), base = 16)

dig(x[,4], x[,5])
