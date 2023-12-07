data07 <- read.table("Input/day07.txt", sep = " ")

sc1 <- setNames(0:12, c(2:9, "T", "J", "Q", "K", "A"))

val_cards <- function(x, sc = sc1, part1 = TRUE) {
  y <- factor(if (part1) x else x[x != "J"])
  k <- if (part1) 0L else sum(x == "J")
  tb <- tabulate(factor(y))
  sum(c(5L - max(length(tb), 1), max(tb) + k, sc[x]) * 13L^(6:0))
}

#part1---------
sum(rank(sapply(strsplit(data07[,1], ""), val_cards)) * data07[,2])

#part2------
sc2 <- setNames(0:12, c("J", 2:9, "T", "Q", "K", "A"))
sum(rank(sapply(strsplit(data07[,1], ""), val_cards, sc = sc2, part1 = F)) * data07[,2])
