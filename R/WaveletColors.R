WaveletColors <- function(n =100){
  h <- atan(seq(-3, 3, length.out = 100))
  h <- h - min(h)
  h <- 0.68*h/max(h)
  return(invisible(rev(hsv(h,1,1))))
}


