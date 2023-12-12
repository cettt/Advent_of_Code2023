f <- function(x) {
  x <- strsplit(x, " ")[[1]]
  y <- strsplit(x[1], "")[[1]] 
  list(
    c(y, "."), 
    as.integer(strsplit(x[2], ",")[[1]]),  
    c(rev(cumsum(rev(y) != ".")), 0),
    sapply(seq_along(y), \(k) sum(cumprod(y[k:length(y)] != ".")))
  )
}

data12 <- lapply(readLines("Input/day12.txt"), f)

check_pos <- function(k, y, mx) {
  if (all(y[k + 1:mx - 1L] != ".") & (if (k + mx <= length(y)) y[k + mx] != "#" else TRUE)) k else -1L 
}

solve <- function(x) {

  n <- x[[2]]
  y <- x[[1]]
  rem <- x[[3]]
  a <- x[[4]]
  if (length(n) == 0L) {
    if (any(y == "#")) return(0L) else return(1L)
  }
  if (all(rem < sum(n))) return(0L)
  k_first <- which.max(y != ".")
  k_last1 <- sum(rem >= sum(n))
  k_last <- max(k_first, min(Position(\(y) y, y == "#", nomatch = length(y)), k_last1))
  pos <- seq.int(k_first, k_last)
  pos <- pos[a[pos] >= n[1] & y[pos + n[1]] != "#"]
  # pos <- sapply(seq.int(k_first, k_last), check_pos, y = y, mx = n[1])

  if (length(pos) == 0L) return(0L)
  
  sum(sapply(pos, \(k) {
    xnew <- x
    xnew[[1]] <- xnew[[1]][-seq_len(k + n[1])]
    xnew[[2]] <- xnew[[2]][-1]
    xnew[[3]] <- xnew[[3]][-seq_len(k + n[1])]
    xnew[[4]] <- xnew[[4]][-seq_len(k + n[1])]
    solve(xnew)
  }))
}

solve(data12[[1]])

part1_new <- sapply(data12, solve)
sum(part1_new)


#part2------

#brute force ftw------
f2 <- function(x) {
  x <- strsplit(x, " ")[[1]]
  y <- strsplit(paste(rep(x[1], 5L), collapse = "?"), "")[[1]]
  
  list(
    y, 
    as.integer(strsplit(paste(rep(x[2], 5), collapse = ","), ",")[[1]]),  
    rev(cumsum(rev(y) != "."))
  )
}

data12_b <- lapply(readLines("Input/day12.txt"), f2)

solve(data12_b[[2]])
