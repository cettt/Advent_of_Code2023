data24 <- sapply(strsplit(readLines("Input/dvy24.txt"), "[,@] "), as.numeric)

bnd <- c(2, 4) * 1e14

intsct <- function(x1, x2, part1 = TRUE) {
  b1 <- c(x1[5] * x1[1] - x1[4] * x1[2], x2[5] * x2[1] - x2[4] * x2[2])
  A <- -x1[5] * x2[4] + x2[5] * x1[4] 
  
  if (A == 0) return(FALSE)
  
  A1 <-  b1[2] * x1[4] - b1[1] * x2[4]
  A2 <-  b1[2] * x1[5] - b1[1] * x2[5]

  res <- c(A1, A2) / A
  t1 <- (res[1] - x1[1]) / x1[4]
  t2 <- (res[1] - x2[1]) / x2[4]
  
  if (part1)  {
    return(all(res >= bnd[1] & res <= bnd[2]) & t1 >= 0 & t2 >= 0) 
  } else {
    return(isTRUE(all.equal(t1, t2)) & t2 >= 0)
  }
}

res <- 0L
for (i in seq_along(data24[1,-1])) {
  for (j in (i + 1L):ncol(data24)) res <- res + intsct(data24[,i], data24[,j])
}

#part1-------
res

#part2-------
slv_2d <- function(vx, vy, check = TRUE) {#solve part2 if x and velocity are given
  
  A <- matrix(c(
      1L, 0L, vx - data24[4L, 1L], 0L,
      0L, 1L, vy - data24[5L, 1L], 0L,
      1L, 0L, 0L, vx - data24[4, 2L], 
      0L, 1L, 0L, vy - data24[5, 2L]
      ), 4L, 4L, byrow = TRUE)
  
  b <- c(data24[1:2, 1], data24[1:2,2])
  
  if (det(A) == 0L) return(FALSE)
  
  res <- tryCatch(solve(A, b), error = \(e) integer())
  if (length(res) == 0L) return(FALSE)
  if (any (res[3:4] < 0)) return(FALSE)
  
  for (k in 3:ncol(data24)) {
    check_k <- intsct(data24[,k], c(res[1:2], 0, vx, vy, 0), FALSE)
    if (!check_k)  return(FALSE)
  }
  if (!check) return(list(x = res[1], y = res[2], t1 = res[3], t2 = res[4])) else return(TRUE)
}


  
all_vxvy <- expand.grid(vx = (-300):300, vy = (-300):300)

for (k in 1:nrow(all_vxvy)) {
  if (slv_2d(all_vxvy$vx[k], all_vxvy$vy[k], TRUE)) {
    list2env(all_vxvy[k, ], environment())
    break
  }
}

vx <- all_vxvy$vx[k]
vy <- all_vxvy$vy[k]
list2env(slv_2d(vx, vy, FALSE), environment())


vz <- ((data24[3,2] + t2 * data24[6, 2]) - (data24[3,1] + t1 * data24[6, 1])) / (t2 - t1)
z <- data24[3, 1] + t1 * data24[6, 1] - t1 * vz


sprintf("%.f", x + y + z)
