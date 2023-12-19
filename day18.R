data18 <- read.table("Input/day18.txt", comment.char = "", sep = " ")
dir_vec <- c(R = 1, D = 1i, L = -1, U = -1i)

dig <- function(dirs, lngs) {

  bnd <- cumsum(c(0 + 0i, dir_vec[dirs] * lngs)) # coordinates of boundary 

  ar <- sum(Im(bnd)[-1] * diff(Re(bnd))) # Shoelace formula for rectangles
  sprintf("%.f", abs(ar) + 1L + sum(lngs) / 2L) #Pick's theorem
}

#part1-----
dig(data18[, 1L], data18[, 2L])

#part2------------
data18[, 4L] <- names(dir_vec)[as.integer(substr(data18[, 3L], 8L, 8L)) + 1L]
data18[, 5L] <- strtoi(substr(data18[, 3L], 3L, 7L), base = 16L)

dig(data18[, 4L], data18[, 5L])

