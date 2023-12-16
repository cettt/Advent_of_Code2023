x <-  as.matrix(read.fwf("Input/day16.txt", width = rep(1, 110)))
mir <- apply(which(x != ".", arr.ind = TRUE), 1, \(x) x[2] + x[1] * 1i)

names(mir) <- x[x!= "."]


energize <- function(beam, dir) {
  beams_dir <- list(c(beam, dir))
  en_vec <- beam + dir
  hist <- complex()
  
  
  while (length(beams_dir)) {
    beam <- beams_dir[[1]][1]
    dir <- beams_dir[[1]][2]
    beams_dir <- beams_dir[-1]
    
    mir2 <- if (Re(dir) != 0) mir[Im(mir) == Im(beam)] else mir[Re(mir) == Re(beam)]
    
    dist <- Position(\(x) x %in% mir2, beam + (seq_len(110)) * dir, nomatch = NA_integer_)
    
    if (is.na(dist)) {#if we hit a boundary
      en_vec <- c(en_vec, beam + (seq_len(110)) * dir)
      
    } else { # if we don't hit a boundary
      new_beam <- beam + (dist)*dir # next mirror
      en_vec <- c(en_vec, beam + seq_len(dist) * dir) #energize
      new_mir <- names(mir2[mir2 == new_beam])
      
      if (new_mir %in% c("\\", "/")) { #check for mirrors
        new_dir <- (Im(dir) + Re(dir) * 1i) * if (new_mir == "\\") 1 else -1
        beams_dir <- c(beams_dir, list(c(new_beam, new_dir)))
        
      } else if ((new_mir == "-" & Re(dir) != 0) | (new_mir == "|" & Im(dir) != 0)) {
        #check for non-splitting splinter
        beams_dir <- c(beams_dir, list(c(new_beam, dir)))
      } else if (all(hist != new_beam)) {
        hist <- c(hist, new_beam)
        beams_dir <- c(beams_dir, list(c(new_beam, 1i*dir)), list(c(new_beam, -1i*dir)))
      }
    }
  }
  
  a <- unique(en_vec)
  length(a[Re(a) > 0 & Re(a) < 111 & Im(a) > 0 & Im(a) < 111])
}

all_pos <- rbind(
  cbind(1:110 * 1i, 1),
  cbind(111 + 1:110 * 1i, -1),
  cbind(1:110, 1i),
  cbind(1:110 + 111*1i, -1i)
)

res <- apply(all_pos, 1, \(x) energize(x[1], x[2]))

#part 1---------
res[1]

#part2------
max(res)
