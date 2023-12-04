data04 <- strsplit(gsub("Card +\\d+: +", "", readLines("Input/day04.txt")), " \\| ")

count_cards <- function(x) {
  w_num <- lapply(strsplit(x, " +"), as.integer)
  sum(w_num[[1]] %in% w_num[[2]])
}

n_mt <- sapply(data04, count_cards) # number of matches

#part1--------
sum(ifelse(n_mt == 0L, 0L, 2^(n_mt - 1L)))

#part2-------
n <- rep.int(1L, length(data04)) #number of cards
for (k in seq_along(n[-1])) {
  n[k + seq_len(n_mt[k])] <- n[k + seq_len(n_mt[k])] + n[k]
}

sum(n)
