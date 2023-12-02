x <- readLines("Input/day02.txt")
extr_col <- \(clr) sapply(regmatches(x, gregexpr(clr, x, perl = T)), \(z) max(as.integer(z)))

res <- sapply(paste0("\\d+(?= ", c("r", "g", "b"), ")"), extr_col)
#part1------
sum(which(apply(res, 1, \(x) max(x - 12:14) <= 0L)))

#part2--------
sum(apply(res, 1, prod))



#using stringr-----
# extr_col <- \(clr) sapply(str_extract_all(x, clr), \(z) max(as.integer(z)))
