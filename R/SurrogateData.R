SurrogateData <- function(x, method = "white.noise"){
  if(method == "white.noise")  x.sur <- rnorm(length(x)) 
  if(method == "shuffle")      x.sur <- sample(x, length(x)) 
  if(method == "Fourier.rand") x.sur <- FourierRand(x) 
  if(method == "AR")           x.sur <- AR(x) 
  return(invisible(x.sur))
}
