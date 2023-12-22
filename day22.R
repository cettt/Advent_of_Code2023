data22 <- sapply(strsplit(readLines("Input/day22.txt"), "[~,]"), as.integer)

data22 <- data22[, order(data22[3,])]
data22[c(1, 4), ] <- data22[c(1, 4), ] - min(data22[c(1, 4), ]) + 1L
data22[c(2, 5), ] <- data22[c(2, 5), ] - min(data22[c(2, 5), ]) + 1L
data22[6L, ] <- data22[6L, ] - data22[3L, ]

flr <- matrix(0L, ncol = max(data22[4, ]), nrow = max(data22[5, ]))

let_fall <-  function(brcks) {
  
  z <- integer(ncol(brcks))
  
  for (k in 1:ncol(brcks)) {
    b <- brcks[,k]
    br_col <- b[1L]:b[4L]
    br_row <- b[2L]:b[5L]
    z[k] <- max(flr[br_row, br_col]) + 1L
    flr[br_row, br_col] <- z[k] + b[6L]
  }
  
  return(z)
  
}

x0 <- let_fall(data22)
x1 <- sapply(1:ncol(data22), \(k) let_fall(data22[, -k]))

#part1-----------
sum(sapply(1:ncol(data22), \(k) sum(x1[,k] != x0[-k]) == 0L))

#part2-------
sum(sapply(1:ncol(data22), \(k) sum(x1[,k] != x0[-k])))
