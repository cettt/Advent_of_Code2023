data14 <- as.matrix(read.fwf("Input/day14.txt", widths = rep(1, 100), comment.char = ""))
cl <- as.integer(col(data14))
vec <- integer(length(data14)) # dummy vector

mat_rot <- Reduce(\(x, y) t(x[nrow(x):1, ]), rep(list(data14), 3L), init = data14, acc = T)
idx_rot <- order(t(matrix(seq_along(data14), 100L)[nrow(data14):1, ]))

find_length <- function(x) {
  sapply(seq_along(x), \(k) Position(\(y) y == "#", c(x[k:((cl[k] - 1L) * 100L + 1L)], "#")) - 2L)
}

find_idx <- function(n, rot) {
  delta <- c(-1L, -100L, 1L, 100L)[rot]
  lapply(seq_along(n), \(k) if (n[k] > 0L) seq.int(k, by = delta, length.out = n[k] + 1L)[-1])
}

#for each rotation of the field compute the maximum number of fields a stone could roll forward
n_list <- lapply(mat_rot, find_length) 
idx_list <- lapply(n_list, \(n) lapply(seq_along(n), \(k) if (n[k] > 0L) (k - n[k]):(k - 1L)))


tilt <- function(x_int, rot = 1L) {
  idx <- idx_list[[rot]]
  vec[x_int] <- 1L
  x_int - pmax(n_list[[rot]][x_int], 0L) + sapply(x_int, \(k) sum(vec[idx[[k]]]))
}

#part1-----
x <- .Internal(which(data14 == "O"))
sum(100L - (tilt(x) - 1L) %% 100L)

#part2-----
tmp <- c() # used to detect cycles
load_vec <- c()
counter <- 0L
for (k in 1:200) {
  #tilt and rotate matrix four times
  x <- idx_rot[tilt(idx_rot[tilt(idx_rot[tilt(idx_rot[tilt(x, 1L)], 2L)], 3L)], 4L)]
  
  tmp <- c(tmp, sum(t(arrayInd(x, dim(data14))) * c(ncol(data14), 1L)))
  load_vec <- c(load_vec, sum(100L - (x - 1L) %% 100L))
  
  if (tmp[k] %in% tmp[-k]) {
    if (cyc > 0L)  {
      counter <- if (tmp[k - cyc] == tmp[k]) counter + 1L else 0L
    } else cyc <- diff(which(tmp == tmp[k]))[1]
    
    if (counter > 9L) break
  } else {
    cyc_counter <- 0L
    cyc <- 0L
  }
}

load_vec[k - cyc + which((1e9 - (k - cyc + 1L):k) %% cyc == 0L)]

