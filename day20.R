x <- readLines("Input/day20.txt")

sw <- gsub(" ->.*", "", x)
ff <- gsub("%", "", grep("%", sw, value = TRUE))
cj <- gsub("&", "", grep("&", sw, value = TRUE))

sw <- setNames(strsplit(gsub(".* -> ", "", x), ", "), gsub("[&%]", "", sw))
cj_list <- setNames(lapply(cj, \(n) names(sw)[sapply(sw, \(x) any(x == n))]), cj)
cj_list <- lapply(cj_list, \(x) setNames(rep.int(0L, length(x)), x))

ff_state <- setNames(rep(0L, length(ff)), ff)
snd <- integer()
snd_from <- character()
k <- 1L
res <- c(0L, 0L)
res2 <- integer()
tar <- names(cj_list[[names(which(sapply(sw, \(a) any(a == "rx"))))]])
rnd <- 1L

while (length(res2) < length(tar)) {
  snd <- c(c("broadcaster" = 0L))
  snd_from <- c("button")
  k <- 1L
  while (k <= length(snd)) {
  # while (k <= 1) {
    cur <- snd[k]
    new <- sw[[names(cur)]]
    if (names(cur) %in% ff) {
      if (cur == 0L) {
        new_sig <- 1L - ff_state[names(cur)]
        ff_state[names(cur)] <- new_sig
        snd <- c(snd, setNames(rep(new_sig, length(new)), new))
        snd_from <- c(snd_from, rep(names(cur), length(new)))
      }
      
    } else if (names(cur) %in% cj) {
      cj_list[[names(cur)]][snd_from[k]] <- cur
      new_sig <- if (all(cj_list[[names(cur)]]) == 1L) 0L else 1L
      snd <- c(snd, setNames(rep(new_sig, length(new)), new))
      snd_from <- c(snd_from, rep(names(cur), length(new)))
      if (names(cur) %in% tar & new_sig == 1L) {
        res2 <- c(res2, setNames(rnd, names(cur)))
      }
      
    } else { # broadcaster
      snd <- c(snd, setNames(rep.int(0L, length(new)), new))
      snd_from <- c(snd_from, rep("broadcaster", length(new)))
    }
    k <- k + 1L
  }
  res <- res + table(snd)
  if (rnd == 1000) part1 <- prod(res)
  rnd <- rnd + 1L
}

#part1------
part1

#part2--------
sprintf("%.f", Reduce(pracma::Lcm, res2))

