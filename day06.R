data06 <- read.table("Input/day06.txt", sep = "")[,-1]

mv_bot <- \(d) sqrt(d[1]^2/4 - d[2]) |> (\(.) floor(d[1]/2 + .) - ceiling(d[1]/2 - .) + 1)()

# part1-----
prod(apply(data06, 2, mv_bot))

# part2------
mv_bot(as.numeric(apply(data06, 1, paste, collapse = "")))
