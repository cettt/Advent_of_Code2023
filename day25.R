data25 <- strsplit(readLines("Input/day25.txt"), ":? ")
mat <- do.call(rbind, lapply(data25, \(a) cbind(a[1], a[-1])))
gr <-  unique(as.character(mat))

bfs <- function(q, mat, part1 = TRUE, j = 0L) {
  
  while (length(q) < length(gr)) {
    new_nd <- c(mat[mat[,1] %in% q, 2], mat[mat[,2] %in% q, 1])
    if (length(new_nd) == 0L) break
    mat <- mat[!(mat[,1] %in% q & mat[,2] %in% q), ]
    q <- unique(c(q, new_nd))
    j <- j + 1L
  }
  if (part1) return(j) else return(length(q))
}

dist <- sort(sapply(gr, bfs, mat = mat))
dist2 <- dist[dist == min(dist)]

m2 <- mat[!(mat[,1] %in% names(dist2) & mat[,2] %in% names(dist2)),]

l1 <- bfs(gr[1], m2, FALSE)
l1 * (length(gr) - l1)
