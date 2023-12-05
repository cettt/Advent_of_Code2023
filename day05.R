data05 <- strsplit(readLines("Input/day05.txt"), " ")[readLines("Input/day05.txt") != ""]

seeds <- as.double(data05[[1]][-1])

alm <- lapply(data05[-1], \(x) if (grepl("\\D", x[1])) NULL else as.double(x)) #almanac
alm <- lapply(split(alm, cumsum(sapply(alm, sum) == 0)), \(x) do.call(rbind, x[-1]))
#change alm format to: start | end | range
alm <- lapply(alm, \(x) cbind(x[,2], x[,2] + x[,3] - 1L, x[,1] - x[,2])[order(x[,2]),]) 

fill_alm <- function(m) {#fill alm to contain the whole range from 0 to 1e10
  for (k in 2:nrow(m)) 
    if (m[k, 1] > m[k - 1, 2] + 1) m <- rbind(m, c(m[k - 1, 2] + 1, m[k, 1] - 1, 0))
  
  rbind(m,  c(max(m[,2]) + 1, 1e10, 0))
}

alm <- lapply(alm, fill_alm)

#function to split every seed range into multiple parts such that every part is inside one interval
split_seeds <- function(s_int, m) {
  x <- sort(c(m[, 1], m[, 2]))
  y <- c(s_int[1] - 1L, x[x >= s_int[1] & x < s_int[2]], s_int[2])
  cbind(head(y, -1) + 1L, y[-1])
}

find_location <- function(si) { 
  
  for (m in alm) {
    si <- do.call(rbind, apply(si, 1, split_seeds, m = m, simplify = F))
    for (l in seq_along(si[,1])) {
      si[l,] <- si[l,] + m[(si[l,1] <= m[,2]) & (m[,1] <= si[l, 2]), 3]
    }
  }
  min(si)
}

#part1--------
find_location(cbind(seeds, seeds))

#part2----
idx <- seq_along(seeds) %% 2 == 1
find_location(cbind(seeds[idx], seeds[idx] + seeds[!idx] - 1L))

