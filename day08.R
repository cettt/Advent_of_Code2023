data08 <- readLines("Input/day08.txt")

lr <- unname(c("L" = 2L, "R" = 3L)[strsplit(data08[1], "")[[1]]])
gr <- do.call(rbind, strsplit(data08[-(1:2)], "[ \\= \\(,)]+"))

go <- function(cur, .end = "Z$") {
  
  res <- integer()
  for (k in 0:1e6) {
    cur <- gr[gr[,1] %in% cur, lr[k %% length(lr) + 1L]]
    if (any(grepl(.end, cur))) {
      res <- c(res, k + 1L)
      cur <- cur[!grepl(.end, cur)]
      if (length(cur) == 0L) break
    }
  }
  res
}
#part1----
go("AAA", "ZZZ")

#part2-------
sprintf("%.f", Reduce(pracma::Lcm, go(grep("A$", gr[,1], value = T))))

