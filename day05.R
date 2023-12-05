data05 <- strsplit(readLines("Input/day05.txt"), " ")

seeds <- as.double(data05[[1]][-1])
#almanac
alm <- split(data05[-1], cumsum(sapply(data05[-1], length) == 0))
alm <- lapply(alm, \(x) matrix(as.numeric(Reduce(c, x[-(1:2)])), ncol = 3, byrow = T))

#change alm format to: start | end | offset
alm <- lapply(alm, \(x) cbind(x[,2], x[,2] + x[,3] - 1L, x[,1] - x[,2])[order(x[,2]),]) 

fill_alm <- function(m) {#fill alm to contain the whole range from 0 to 1e10
  for (k in 2:nrow(m)) 
    if (m[k, 1] > m[k - 1, 2] + 1) m <- rbind(m, c(m[k - 1, 2] + 1, m[k, 1] - 1, 0))
  
  rbind(m, c(max(m[,2]) + 1, 1e10, 0)) #add one more row to m which covers range until 10**10
}

alm <- lapply(alm, fill_alm)

#function first splits  seed range into multiple parts such that every part is inside one interval
#afterwards, we add the offset to map it
map_seeds <- function(s_int, m) {
  res <- m[s_int[1] <= m[,2] & m[,1] <= s_int[2], , drop = FALSE]
  res[cbind(c(1, nrow(res)), 1:2)] <- s_int
  res[,1:2] + res[,3]
}


find_location <- function(sr) { #find closest location given a matrix with seed ranges
  for (m in alm) sr <- do.call(rbind, apply(sr, 1, map_seeds, m = m, simplify = F))
  return(min(sr))
}

#part1--------
find_location(cbind(seeds, seeds))

#part2----
idx <- seq_along(seeds) %% 2 == 1
find_location(cbind(seeds[idx], seeds[idx] + seeds[!idx] - 1))
