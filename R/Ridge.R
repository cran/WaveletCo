Ridge <- function(power, band = 5, scl = 0.1){
  alt <- scl * max(power);
  no.row <- nrow(power)
  no.col <- ncol(power)
  ridge <- matrix(0, nrow = no.row, ncol = no.col)

  for(m in 1:no.col){
    for(n in 1:no.row){
      r1 <- n - band; if(r1 < 1) r1 = 1
      r2 <- n + band; if(r2 > no.row) r2 = no.row
      if(max(power[r1:r2, m]) == power[n, m] & power[n, m] > alt){
        ridge[n, m] = 1 
      }
    }
  }

  return(invisible(ridge))
}

