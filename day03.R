data03 <- readLines("Input/day03.txt")

get_pos <- function(pat) {
  p <- gregexpr(pat, data03)
  x <- sapply(seq_along(p), \(k) rbind(k, p[[k]], p[[k]] + attr(p[[k]], "match.length") - 1L))
  pos <- do.call(cbind, x)
  pos[, pos[2, ] > 0L]
}

dp <- get_pos("\\d+") #digit positions
sp <- get_pos("[^0-9\\.]")[1:2, ] #symbol positions

dig <- as.integer(unlist(regmatches(data03, gregexpr("\\d+", data03)))) #digits
sym <- unlist(regmatches(data03, gregexpr("[^0-9\\.]", data03))) #symbols

res <- c(part1 = 0L, part2 = 0L)
for (k in seq_along(sp[1,])) {
  idx <- rowSums(sapply(2:3, \(i) colSums((dp[-i, ] - sp[, k])^2L)) < 4L) > 0L
  res[1]<- res[1] + sum(dig[idx])
  if (sum(idx) == 2L & sym[k] == "*") res[2] <- res[2] + prod(dig[idx])
}

#part1 and 2-----
res

  
#using stringr------
# get_pos <- function(pat) {
#   p <- str_locate_all(data03, pat)
#   t(do.call(rbind, lapply(seq_along(data03), \(k) if (nrow(p[[k]]) > 0) cbind(k, p[[k]]))))
# }
