data15 <- strsplit(readLines("Input/day15.txt"), ",")[[1]]

run_hash <- function(x) {
  cur <- 0L
  for (k in utf8ToInt(x)) cur <- ((cur + k)*17L) %% 256L
  return(cur)
}


sum(sapply(data15, run_algo))

#part2-----------
box <- lapply(1:256, \(x) character())

for (x in data15) {
  print(x)
  lab <- gsub("[^a-z]", "", x)
  b <- run_hash(lab) + 1L
  lab_regex <- paste0("^", lab, "[^a-z]")
  
  if (grepl("=", x)) {
    if (any(grepl(lab_regex, box[[b]]))) {
      box[[b]][grepl(lab_regex, box[[b]])] <- x
    } else {
      box[[b]] <- c(box[[b]], x)
    }
  } else {
   box[[b]] <- box[[b]][!grepl(lab_regex, box[[b]])] 
  }
}

sum(1:256 * sapply(box, \(x) sum(as.integer(gsub("\\D+", "", x)) * seq_along(x))))
