n <- 13L
x <- unname(unlist(read.fwf("Input/day17.txt", widths = rep(1, n))))

map_k <- function(k, dir, idx) {
  k2d <- k - dir * n^2
  if (dir %% 2L == 1L) {#up/down
  k2d + c(-(idx)[k %% n - idx >= 0L], (idx)[k %% n + idx <= n -1L] + 2*n^2)
  } else {
  k2d + c(
    -(idx)[k2d %/% n - idx >= 0L]*n + 3*n^2, 
    (idx)[k2d %/% n + idx <= (n - 1L)] * n + n^2
  )
  }
}

map_sc <- function(k, dir, idx) {
  k2d <- k - dir * n^2
  if (dir %% 2L == 1L) {#up/down
    c(cumsum(x[(k2d - idx + 1L)[k %% n - idx >= 0L]]), 
      cumsum(x[(k2d + idx + 1L)[k %% n + idx <= n - 1L]]))
  } else {
  c(cumsum(x[k2d - (idx)[k2d %/% n - idx >= 0L]*n + 1L]), 
    cumsum(x[k2d + (idx)[k2d %/% n + idx <= (n - 1L)] * n + 1L]))
  }
  
}


find_shortest_path <- function(idx = 1:3) {
  
  lookup <- sapply(seq_len(n^2 * 4L) - 1L, \(k) map_k( k, k %/% n^2, idx))
  sc     <- sapply(seq_len(n^2 * 4L) - 1L, \(k) map_sc(k, k %/% n^2, idx))
  
  q <- collections::priority_queue(c(0L, 3L*n*n), priorities = 0L)
  
  dmg <- c(rep.int(100000L, 4*n^2))
  dmg[c(0L, 3L*n*n) + 1L] <- 0L
  
  while (q$size() > 0) {
    cur <- q$pop()
    cur_dmg <- dmg[cur + 1L]
    lu <- lookup[[cur + 1L]]
    sc_lu <- sc[[cur + 1L]]
    
    for (k in seq_along(lookup[[cur + 1L]])) {
      nd <- cur_dmg + sc_lu[k]
      if (dmg[lu[k] + 1L] > nd) {
        dmg[lu[k] + 1L] <- nd
        q$push(lu[k], priority = -nd)
      }
    }
  }
  return(min(dmg[(n^2 - 1L) + 0:3*n^2 + 1L]))
}

find_shortest_path()

## part 2---------
find_shortest_path(idx = 4:10)


#325 too low