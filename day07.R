data07 <- read.table("Input/day07.txt", sep = " ")

sc1 <- setNames(0:12, c(2:9, "T", "J", "Q", "K", "A"))
sc2 <- setNames(0:12, c("J", 2:9, "T", "Q", "K", "A"))

val_cards <- function(x) {
  tb <- tabulate(factor(x[x != "J"]))
  k <- sum(x == "J")
  res1 <- c(min(5L - length(tb), 4L) - sign(k), max(tb, k), sc1[x]) 
  colSums(cbind(res1, c(res1[1] + sign(k), max(tb) + k, sc2[x])) * 13L^(6:0))
}

#part1 and 2---------
colSums(apply(sapply(strsplit(data07[,1], ""), val_cards), 1, rank) * data07[,2])


#explanation:
# we transform each hand to 7 digit 13-adic number, where
#  1st digit: 5 minus number of distinct cards
#  2nd digit: number of times any of the most frequent card appears.
#  3-7 digit: score of that card (scores going from 0 to 12)
#   for example the hand AAAA7 becomes  3|4|12|12|12|12|5 
#    because there are two different cards (A and 7) and 5-2 =3 
#     and the most common card appears four times (A)
#
# for part 2 it is always best to choose the Jokers to be any of the most occurring other cards
#  e.g if the hand is 2277J than the J can be either 2 or 7 (it does not matter!)
# the digits for part1 and part2 can be obtained by 
#     only considering the cards without Jokers and the number of jokers.
#  Therefore let 
#        k be the number of Jokers (0 <= k <= 5), 
#        n be the number of observations of the most frequent card (without jokers) (0 <= n <= 5),
#        and m be the number of distinct cards (without jokers) (0 <= m <= 5)
#  For example: 2277J ==> k = 1, n = 2, m = 2
#               JJJJA ==> k = 4, n = 1, m = 1
#               JJJJJ ==> k = 5, n = 0, m = 0
#               23456 ==> k = 0, n = 1, m = 5
# Then, for part 1 we get that
#   1st digit = min(5 - m, 4) - sign(k) 
#     which is always equal to 5 minus the number of distinct cards (including jokers!)
#   2nd digit = max(n, k)
#     which is always the number of observations of the most frequent card (with jokers!)
# For part 2 we get that
#   1st digit = min(5 - m, 4) 
#     which is always equal to 5 minus the number of distinct cards (where Jokers are counted as one of the other cards)
#   2nd digit = n + k
#     which is always the number of observations of the most frequent card 