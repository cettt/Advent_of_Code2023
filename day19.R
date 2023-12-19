data19 <- readLines("Input/day19.txt")

inst <- data19[cumsum(data19 == "") == 0]
nms <- gsub("^([a-z]+)\\{.*", "\\1", inst)
inst <- strsplit(gsub("^[a-z]+\\{", "", gsub("}", "", inst)), ",")
names(inst) <- nms
vals <- data19[cumsum(data19 == "") == 1][-1]

vals <- strsplit(gsub("[{}]", "", vals), ",")

f <- function(v) {
  e <- environment() 
  sapply(v, \(x) assign(gsub("\\=.*", "", x), as.integer(gsub("\\D+", "", x)), envir = e))
  
  cur <- "in"
  while (!cur %in% c("A", "R")) {
    inst2 <- inst[cur][[1]] 
    for (xx in inst2) {
      if (!grepl("\\d", xx)) {
        cur <- xx
        break
      }
      x2 <- gsub(":.*", "", xx)
      if (eval(parse(text = x2))) {
        cur <- gsub(".*:", "", xx)
        break
      }
    }
  }
  return(cur)
}

#part1------
sum(sapply(vals[sapply(vals, f) == "A"], \(y) sum(as.integer(gsub("\\D", "", y)))))

#part2-----
