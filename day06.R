data06 <- gsub("\\D*\\s+", " ", readLines("Input/day06.txt"))
d <- sapply(strsplit(data06, " "), \(x) as.integer(x[-1]))

break_record <- \(d) {
  det <- sqrt(d[1]^2 / 4 - d[2])
  floor(d[1] / 2 + det) - ceiling(d[1] / 2 - det) + 1
}

# part1-----
prod(apply(d, 1, break_record))

# part2------
break_record(as.numeric(apply(d, 2, paste, collapse = "")))
