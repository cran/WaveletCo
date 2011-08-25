COI <- function(start = start, dt = dt, nc = nc, nr = nr, Period = Period){

  axis.1 <- seq(from = start, by = dt, length.out = nc)
  axis.2 <- log2(Period)

  ff <- 1.033044	# Fourier factor;
  coi <- ff * dt * c(1E-5, 1:((nc+1)/2-1), rev((1:(nc/2-1))), 1E-5)
  coi.x <- c(axis.1[c(1, 1)] - dt*0.5, axis.1, axis.1[c(nc, nc)] + dt*0.5)
  logyint <- axis.2[2] - axis.2[1]
  yl <- c(log2(Period[nr]) + 0.5*logyint, log2(Period[1]) - 0.5*logyint)
  yr <- rev(yl)
  coi.y <- c(yl, log2(coi), yr)
	
# output
  output <- list(x = coi.x, y = coi.y, axis.1 = axis.1, axis.2 = axis.2)
  return(invisible(output))
}
