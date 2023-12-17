x <- unname(unlist(read.fwf("Input/day17.txt", widths = rep(1, 141))))
n2 <- length(x)
n <- as.integer(sqrt(n2))

map_k <- function(k, idx) {
  if (k >= n2) {# right/left
    k - n2 + c(-idx[k %% n >= idx], idx[k %% n + idx <= n - 1L])
  } else {
    k + n2 + c(-idx[k %/% n >= idx], idx[k %/% n + idx <= n - 1L]) * n
  }
}


map_hl <- function(k, idx) {
  idx2 <- seq_len(max(idx))
  if (k >= n2) {#up/down
    res1 <- cumsum(x[(k - n2 - idx2 + 1L)[k %% n >= idx2]]) 
    res2 <- cumsum(x[(k - n2 + idx2 + 1L)[k %% n + idx2 <= n - 1L]])
  } else {
    res1 <- cumsum(x[k - idx2[k %/% n - idx2 >= 0L    ] * n + 1L]) 
    res2 <- cumsum(x[k + idx2[k %/% n + idx2 <= n - 1L] * n + 1L])
  }
  
  if (min(idx) > 1) {
    return(c(res1[-seq_len(min(idx) - 1L)], res2[-seq_len(min(idx) - 1L)])) 
  }
  
  else return(c(res1, res2))
}


find_shortest_path <- function(idx = 1:3) {
  
  lookup <- sapply(seq_len(n2 * 2L) - 1L, \(k) map_k( k, idx))
  hl     <- sapply(seq_len(n2 * 2L) - 1L, \(k) map_hl(k, idx))
  
  q <- collections::priority_queue(c(0L, n2), priorities = 0L)
  
  dmg <- c(rep.int(1e5L, 2L*n2))
  dmg[c(0L, n2) + 1L] <- 0L
  
  while (q$size() > 0) {
    cur <- q$pop()
    cur_dmg <- dmg[cur + 1L]
    lu <- lookup[[cur + 1L]]
    hl_lu <- hl[[cur + 1L]]
    
    for (k in seq_along(lookup[[cur + 1L]])) {
      nd <- cur_dmg + hl_lu[k]
      if (dmg[lu[k] + 1L] > nd) {
        dmg[lu[k] + 1L] <- nd
        q$push(lu[k], priority = -nd)
      }
    }
  }
  return(min(dmg[(n2 - 1L) + 0:1*n2 + 1L]))
}

# part 1-----------
find_shortest_path()

# part 2---------
find_shortest_path(idx = 4:10)

