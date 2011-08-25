wc.angle <- function(WC = WC, p = 2){
  Angle <- WC$Angle
  if(p == 0) Angle[WC$WCo < 0.975] <- NA
  if(p == 1) Angle[WC$WCo.sig > 0.05] <- NA
  if(p == 2) Angle[-which(WC$WCo.sig < 0.05 | WC$WCo > 0.975)] <- NA
  A.row <- seq(1, NROW(Angle), round(NROW(Angle)/30))
  A.col <- seq(1, NCOL(Angle), round(NCOL(Angle)/40))
  size  <- min(par()$pin[2]/30, par()$pin[1]/40)
  ratio <- par()$pin[2]/par()$pin[1]
  Angle[-A.row,] <- NA; Angle[,-A.col] <- NA;

# plot arrows
  for(i in 1:NROW(Angle)){
    for(j in 1:NCOL(Angle)){
      if(!is.na(Angle[i,j])){	
        x <- WC$axis.1[j]
        y <- log2(WC$Period)[i]
        arrow(x, y, l = size, w = .3*size, alpha = Angle[i,j])
      }
    }
  }
	
}
