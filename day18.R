x <- read.table("Input/day18.txt", comment.char = "", sep = " ")
dir <- c(R = 1, D = 1i, L = -1, U = -1i)


dig <- function(dirs, lngs) {

  l <- sum(lngs) + 1L
  cur <- 0 + 0i

  for (k in seq_along(dirs)) cur <- c(cur, tail(cur, 1) + lngs[k] * dir[dirs[k]])
  
  ar <- -sum((Im(cur)[-length(cur)] + Im(cur[-1])) * diff(Re(cur))) / 2L # Shoelace formula
  sprintf("%.f", ar + 1L + (l - 1L) / 2L) #Prick's theorem
}

#part1-----
dig(x[,1], x[,2])

#part2------------
x[,4] <- names(dir)[as.integer(substr(x[,3], 8, 8)) + 1L]
x[,5] <- strtoi(substr(x[,3], 3, 7), base = 16)

dig(x[,4], x[,5])
