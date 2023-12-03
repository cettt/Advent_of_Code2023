  data03 <- readLines("Input/day03.txt")
  
  dig <- as.integer(unlist(regmatches(data03, gregexpr("\\d+", data03))))
  sym <- unlist(regmatches(data03, gregexpr("[^0-9\\.]", data03)))
  
  get_pos <- function(pat) {
    p <- gregexpr(pat, data03)
    x <- sapply(seq_along(p), \(k) rbind(k, p[[k]], p[[k]] + attr(p[[k]], "match.length") - 1L))
    res <- do.call(cbind, x)
    res[,res[2,] > 0L]
  }
  
  dig_pos <- get_pos("\\d+")
  sym_pos <- get_pos("[^0-9\\.]")[1:2, ]
  
  
  part1 <- 0L
  part2 <- 0L
  for (k in seq_along(sym_pos[1,])) {
    idx <- rowSums(sapply(2:3, \(i) colSums((dig_pos[-i, ] - sym_pos[,k])^2)) < 4L)
    part1 <- part1 + sum(dig[idx > 0L])
    if (sum(idx > 0L) == 2L & sym[k] == "*") part2 <- part2 + prod(dig[idx > 0])
  }
  
  part1
  part2
