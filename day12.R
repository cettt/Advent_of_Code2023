data12 <- strsplit(readLines("Input/day12.txt"),  " ") 
tile2int <- c("." = 0L, "#" = 1L, "?" = 2L)

map_springs <- function(x, n = 1L) {
  y <- tile2int[c(strsplit(paste(rep(x[1], n), collapse = "?"), "")[[1]], ".")]
  list(
    df = matrix(c(
      unname(y),
      rev(cumsum(rev(y) != 0L)), #number of remaining (potentially) non-operating springs
      as.integer(sapply(seq_along(y), \(k) sum(sign(cumprod(y[k:length(y)]))))), #size of largest possible group,
      sapply(seq_along(y), \(k) Position(\(z) z, y[k:length(y)] == 1L, nomatch = 1e5L))# nr of tiles until first damaged spring
    ), ncol = 4), 
    n = rep(as.integer(strsplit(x[2], ",")[[1]]), n) 
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

#part 1 and 2--------
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


#Explanation------
# First, for a given line we compute the following:
#   - a matrix df with four columns
#     - column 1 are the tiles converted to integers (necessary to have an integer matrix)
#     - column 2 the maximum number of non-operating tiles possible from a given position to the end
#     - column 3 the maximum number of damaged tiles that could start at that tile
#     - column 4 the first time after a given tile, that a damaged tile appears
#  - an integer vector, containing the group sizes

# We use a recursive function which does the following:
#  - Given that we are at position k and still have to find all groups starting from j
#     - Find all possible positions (pos) for the next group (of size n0[j])
#     - If no such position can be found return 0
#     - Otherwise increase the current position (k) by n0[j] + pos and increase j by 1 and call the function again.
# - The functions aborts if we either
#   - go past the last position, or
#   - have found all the groups 

# Memoization can be used to speed up (especially for part2)
#  We use k*100L + j as index and store results in an integer vector.