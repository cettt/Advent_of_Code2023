data17 <- unname(unlist(read.fwf("Input/day17.txt", widths = rep(1L, 141L))))
n2 <- length(data17)
n <- as.integer(sqrt(n2))

find_k <- function(k, mn = 1L, mx = 3L, idx = seq_len(mx)) {
  if (k >= n2) {# right/left
    adj1 <- k - n2 - idx[k %% n >= idx]
    adj2 <- k - n2 + idx[k %% n + idx <= n - 1L]
  } else {
    adj1 <- k + n2 - idx[k %/% n >= idx] * n
    adj2 <- k + n2 + idx[k %/% n + idx <= n - 1L] * n
  }
  
  sc1 <- cumsum(c(0L, data17[adj1 %% n2 + 1L]))[-seq_len(mn)]
  sc2 <- cumsum(c(0L, data17[adj2 %% n2 + 1L]))[-seq_len(mn)]
  list(c(c(0L, adj1)[-seq_len(mn)], c(0L, adj2)[-seq_len(mn)]), c(sc1, sc2))
}

find_shortest_path <- function(mn, mx) {
  
  lookup <- sapply(seq_len(n2 * 2L) - 1L, \(k) find_k(k, mn, mx))
  adj <- lookup[1L, ]
  heat_loss <- lookup[2L, ]
  
  q <- collections::priority_queue(c(1L, n2 + 1L), priorities = 0L)
  
  dmg <- c(rep.int(1e5L, 2L*n2))
  dmg[c(0L, n2) + 1L] <- 0L
  
  while (q$size() > 0L) {
    cur <- q$pop()
    if (cur %% n2 == 0L) break
    cur_dmg <- dmg[cur]
    lu <- adj[[cur]] + 1L
    hl_lu <- heat_loss[[cur]]
    
    for (k in seq_along(lu)) {
      nd <- cur_dmg + hl_lu[k]
      if (dmg[lu[k]] > nd) {
        dmg[lu[k]] <- nd
        q$push(lu[k], priority = -nd)
      }
    }
  }
  return(min(dmg[n2 + 0:1 * n2]))
}

# part 1-----------
find_shortest_path(1L, 3L)

# part 2---------
find_shortest_path(4L, 10L)
