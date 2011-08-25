WindowBartlett <- function(n){
  w <- 2*(0:((n-1)/2))/(n-1)
  if(n%%2 ==1) w <- c(w, w[rev(1:((n-1)/2))])
  if(n%%2 !=1) w <- c(w, w[rev(1:(n/2))])
  return(invisible(w))
}
