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
  gr2 <- lapply(splt[-tar], bfs, lookup = lookup, splt = splt) #compressed graph
  
  check_valid <- function(pth) { #check if there is still a way to the target
    cur <- pth[1]
    j <- 1L
    while(j <= length(cur)) {
      nxt <- gr2[[cur[j]]][1L, ]
      if (any(nxt == tar)) return(TRUE)
      nxt <- nxt[!nxt %in% c(pth, cur)]
      cur <- c(cur, nxt)
      j <- j + 1L
    }
    return(FALSE)
  }
  
  e <- environment()
  e$pth <- integer()
  
  find_longest_way <- function(cur = 1L, lng = 0L) {
    
    if (cur == tar) return(0L)
    
    if (length(e$pth) > 10L) if (!check_valid(c(cur, e$pth))) return(-1e4L)
    
    e$pth <<- c(cur, e$pth)
    nxt <- gr2[[cur]]
    nxt <- nxt[, !nxt[1,] %in% e$pth, drop = FALSE]
    
    if (ncol(nxt) != 0L) {
      res <- max(nxt[2,] + sapply(nxt[1L,], \(x) find_longest_way(x)))
    } else res <- -1e4L
    
    e$pth <<- e$pth[-1] 
    return(res)
  }
  
  find_longest_way()
}


#part1-----
solve_day23(part1 = TRUE)

#part2------
solve_day23(part1 = FALSE) #takes 3.1 minutes
