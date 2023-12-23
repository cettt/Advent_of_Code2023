n <- 141L
data23 <- unlist(read.fwf("Input/day23.txt", widths = rep(1L, n), com = ""))

gr <- unname(which(data23 != "#")) # graph
slp <- c(">" = n, "<" = - n, "^" = -1L, "v" = 1L)

find_adj <- function(k, part1 = TRUE) {
  
  if (part1) if (data23[k] %in% names(slp)) return(k + slp[data23[k]])
  if (data23[k] == "#") return(integer(0))
  
  m <- k %% n
  res <- k + c(if (k > n) -n, if (k < n * (n - 1L)) n, if (m != 1L) -1L, if (m != 0L) 1L)
  fld <- data23[res]
  res[fld != "#"]
}


bfs <- function(cur0, lookup, splt) { #bfs 
  res <- matrix(integer(), nrow = 2)
  for (cur in lookup[[cur0]]) {
    pth <- c(cur0, cur)
    while(!cur %in% splt) {
      cur <- setdiff(lookup[[cur]], pth)
      pth <- c(pth, cur)
      if (length(cur) == 0L) break
    }
    if (length(cur) > 0) res <- cbind(res, c(cur, length(pth) - 1L))
  }
  
  res[1, ] <- sapply(res[1,], \(x) which(x == splt))
  res
  
} 

solve_day23 <- function(part1 = TRUE) {
  lookup <- lapply(seq_along(data23), \(k) find_adj(k, part1 = part1))
  splt <- c(min(gr), which(sapply(lookup, length) > 2L), max(gr))
  
  tar <- length(splt)
  gr2 <- lapply(splt, bfs, lookup = lookup, splt = splt) #compressed graph
  tar2 <- which(sapply(gr2, \(x) any(x[1,] == tar)))
  d2 <- gr2[[tar2]][2L, gr2[[tar2]][1,] == tar]
  strt <- gr2[[1]][1]
  d1 <- gr2[[1]][2]
  
  check_valid <- function(pth) { #check if there is still a way to the target
    j <- length(pth)
    while(j <= length(pth)) {
      nxt <- gr2[[pth[j]]][1L, ]
      nxt <- nxt[!nxt %in% pth]
      pth <- c(pth, nxt)
      j <- j + 1L
    }
    if (all(c(tar2, 3L, 9L, 29L, 34L) %in% pth)) return(TRUE) #TODO replcae my longest path nodes
    return(FALSE)
  }
  
  e <- environment()
  e$pth <- integer()
  
  find_longest_way <- function(cur = strt) {
    
    if (cur == tar2) return(0L)
    
    if (!part1) if (length(e$pth) > 10L) if (!check_valid(c(e$pth, cur))) return(-1e4L)
    
    e$pth <<- c(cur, e$pth)
    nxt <- gr2[[cur]]
    nxt <- nxt[, !nxt[1,] %in% e$pth, drop = FALSE]
    
    if (ncol(nxt) != 0L) {
      res <- max(nxt[2L, ] + sapply(nxt[1L,], \(x) find_longest_way(x)))
    } else res <- -1e4L
    
    e$pth <<- e$pth[-1L] 
    return(res)
  }
  
  find_longest_way() + d1 + d2
}


#part1-----
solve_day23(part1 = TRUE)

#part2------
solve_day23(part1 = FALSE) #takes < 2 minutes
