data12 <- strsplit(readLines("Input/day12.txt"),  " ") 
tile2int <- c("." = 0L, "#" = 1L, "?" = 2L)

map_springs <- function(x, n = 1L) {
  y <- tile2int[c(strsplit(paste(rep(x[1], n), collapse = "?"), "")[[1]], ".")]
  list(
    matrix(c(
      unname(y),
      rev(cumsum(rev(y) != 0L)), #number of remaining (potentially) non-operating springs
      as.integer(sapply(seq_along(y), \(k) sum(sign(cumprod(y[k:length(y)]))))), #size of largest possible group,
      sapply(seq_along(y), \(k) Position(\(z) z, y[k:length(y)] == 1L, nomatch = 1e5L))# nr of tiles until first damaged spring
    ), ncol = 4), 
    rep(as.integer(strsplit(x[2], ",")[[1]]), n) 
  )
}


count_pos <- function(k = 1L, j = 1L) {
  if (k > nrow(df0)) return(if (j <= length(n0)) 0L else 1L)
  if (j  > length(n0)) return(if (any(df0[k:nrow(df0), 1L] == 1L)) 0L else 1L)
  if (lookup[k*100L + j] >= 0L) return(lookup[k*100L + j])
  
  df <- df0[k:nrow(df0), ,drop = F]
  n  <- n0[j:length(n0)]
  
  if (df[1, 2] < sum(n)) {
    res <- 0L
  } else {
    pos <- seq_len(min(df[1L, 4L], sum(df[, 2L] >= sum(n))))
    pos <- pos[df[pos, 3L] >= n[1L] & df[pos + n[1L], 1L] != 1L]
    if (length(pos) == 0L) return(0L)
    
    res <- sum(sapply(pos, \(i) count_pos(k + i + n[1], j + 1L)))
  }
  
  lookup[k*100L + j] <<- res
  return(res)
}

#read data----
springs <- lapply(c(1L, 5L), \(n) lapply(data12, map_springs, n = n))

res <- c(0L, 0L)
for (k in seq_along(springs2)) {
  for (p in 1:2){
    lookup <- rep.int(-1L, 1e5)
    x <- springs2[[k]]
    df0 <- springs[[p]][[k]][[1]]
    n0 <-  springs[[p]][[k]][[2]]
    res[p] <- res[p] + count_pos(1L, 1L)
  }
}

sprintf("%.f", res)
