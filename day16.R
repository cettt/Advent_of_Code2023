data16 <-  as.matrix(read.fwf("Input/day16.txt", width = rep(1, 110)))

mir <- apply(which(data16 != ".", arr.ind = TRUE), 1, \(x) x[2] + x[1] * 1i)
names(mir) <- data16[data16!= "."]
splt_mir <- mir[names(mir) %in% c("|", "-")]


energize <- function(beam, dir = c(-1, 1) * if (isTRUE(splt == "|")) 1i else 1, splt = NULL) {
  
  en_vec <- beam
  fin <- complex()
  
  if (!is.null(splt)) {
    beams_dir <- list(c(beam, dir[1]), c(beam, dir[2]))
  } else {
    beams_dir <- list(c(beam, dir))
  }
  
  
  hist <- beam
  
  while (length(beams_dir)) {
    run <- TRUE
    beam <- beams_dir[[1]][1]
    dir <- beams_dir[[1]][2]
    beams_dir <- beams_dir[-1]
    
    while (run) {
      mir2 <- if (Re(dir) != 0) mir[Im(mir) == Im(beam)] else mir[Re(mir) == Re(beam)]
      dist <- Position(\(x) x %in% mir2, beam + (seq_len(110)) * dir, nomatch = NA_integer_)
      run <- FALSE
      
      if (is.na(dist)) {#if we hit a boundary
        en_vec <- c(en_vec, beam + (seq_len(110)) * dir)
        
      } else { # if we don't hit a boundary
        en_vec <- c(en_vec, beam + seq_len(dist) * dir) #energize
        beam <- beam + (dist)*dir # next mirror
        new_mir <- names(mir2[mir2 == beam])
        
        if (new_mir %in% c("\\", "/")) { #check for mirrors
          dir <- (Im(dir) + Re(dir) * 1i) * if (new_mir == "\\") 1 else -1
          run <- TRUE
        } else if (all(hist != beam)) {
          hist <- c(hist, beam)
          if ((new_mir == "-" & Re(dir) != 0) | (new_mir == "|" & Im(dir) != 0)) {
            run <- TRUE
          } else {
            fin <- c(fin, beam)
            if (is.null(splt)) {
              while (length(beam)) {
                hist <- c(hist, beam[1])
                lst <- splt_list[beam[1] == splt_mir][[1]]
                en_vec <- c(en_vec, lst[[2]])
                beam <- c(beam[-1], setdiff(lst[[1]], hist))
              }
            }
          }
        }
      }
    }
  }
  
  a <- unique(en_vec)
  if (is.null(splt)) return(sum(Re(a) > 0 & Re(a) < 111 & Im(a) > 0 & Im(a) < 111))
  
  list(fin, a[Re(a) > 0 & Re(a) < 111 & Im(a) > 0 & Im(a) < 111])
}


splt_list <- lapply(seq_along(splt_mir), \(k) energize(splt_mir[k], splt = names(splt_mir)[k]))

#part1-------
bnds <- cbind(c(1:110 * 1i, 1:110, 111i + 1:110, 111 + 1:110*1i), rep(c(1, 1i, -1i, -1), each = 110))

res <- apply(bnds, 1, \(x) energize(x[1], dir = x[2]))

res[1]

max(res)
