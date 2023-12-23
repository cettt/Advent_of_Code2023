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


bfs <- function(cur0) { #bfs 
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
  lookup <- lapply(seq_along(data23), find_adj, part1 = part1)
  splt <- c(min(gr), which(sapply(lookup, length) > 2L), max(gr))
  
  tar <- length(splt)
  gr2 <- lapply(splt[-tar], bfs) #compressed graph
  
  
  find_longest_way <- function(cur = 1L, pth = integer(), lng = 0L) {
    
    if (cur == tar) return(lng)
    if (31L %in% pth & 26L %in% pth & cur != 33L) return(0L)
    
    nxt <- gr2[[cur]]
    nxt <- nxt[, !nxt[1,] %in% pth, drop = FALSE]
    if (tar %in% nxt[1,]) nxt <- nxt[, nxt[1,] == tar, drop = FALSE]
    
    if (ncol(nxt) != 0L) {
      res <- max(apply(nxt, 2, \(x) find_longest_way(x[1], c(pth, cur), lng + x[2])))
    } else res <- 0L
    
    return(res)
  }
  
  find_longest_way()
}


#part1-----
solve_day23(part1 = TRUE)

#part2------
solve_day23(part1 = FALSE) #takes 10 minutes