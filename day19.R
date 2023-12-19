data19 <- readLines("Input/day19.txt")

inst <- data19[cumsum(data19 == "") == 0]
nms <- gsub("^([a-z]+)\\{.*", "\\1", inst)
inst <- setNames(strsplit(gsub("^[a-z]+\\{", "", gsub("}", "", inst)), ","), nms)

parts <- paste0("c(", gsub("[{}]", "", data19[cumsum(data19 == "") == 1][-1]), ")")

#part1------
snd_workflow <- function(part) {
  list2env(as.list(eval(parse(text = part))), environment())
  cur <- "in"
  while (!cur %in% c("A", "R")) {
    flw <- inst[cur][[1]] 
    flw <- c(flw[-length(flw)], paste0("TRUE:", flw[length(flw)]))
    dest <- gsub(".*:", "", flw)
    cur <- dest[Position(\(z) eval(parse(text = z)), gsub(":.*", "", flw))]
  }
  return(if (cur == "A") x + m + a + s else 0L)
}

sum(sapply(parts, snd_workflow))

#part2-----
res <- 0
q <- list("in" = matrix(rep(c(0L, 4001L), 4), 2, dimn = list(c(">", "<"), c("x", "m", "a", "s"))))
f_hlp <- list(">" = max, "<" = min) #functions to update intervals
adj <- c(">" = 1L, "<" = -1L) #adjustments to update intervals

while (length(q)) { #bfs search
  cur <- q[[1]] #set current node to first in queue
  inst2 <- inst[names(q[1])][[1]] 
  for (stp in inst2) {
    ne <- cur #new edge
    
    dest <- gsub(".*:", "", stp)
    if (grepl("\\d", stp)) {
      col <- substr(stp, 1L, 1L) #column
      op  <- if (substr(stp, 2L, 2L) == "<") 2L else 1L #operator
      tsh <- as.integer(gsub("\\D", "", stp)) #threshold
      ne[op, col] <- f_hlp[[op]](ne[op, col], tsh) #update interval of new edge
      cur[3L - op, col] <- f_hlp[[3L - op]](cur[3L - op, col], tsh + adj[op]) #update interval of current edge
    }
    if (dest == "A") res <- res + prod(ne[2L, ] - ne[1L, ] - 1)
    else if (dest != "R") q <- c(q, setNames(list(ne), dest))
    
  }
  q <- q[-1]
}

sprintf("%.f", res)
