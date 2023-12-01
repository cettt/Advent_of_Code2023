x <- read.table("Input/day01.txt")[,1]

#part 1----
sum(sapply(strsplit(gsub("\\D", "", x), ""), \(y) as.integer(c(y[1], y[length(y)])) * c(10, 1)))


#part2---------
d <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
d2 <- c("o1e", "t2o", "t3e", "f4r", "f5e", "s6x", "s7n", "e8t", "n9e")

for (k in 1:9) x <- gsub(d[k], d2[k], x)
sum(sapply(strsplit(gsub("\\D", "", x), ""), \(y) as.integer(c(y[1], y[length(y)])) * c(10, 1)))
