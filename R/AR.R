AR <- function(x, order = 1){
  ar.ls <- pacf(x, plot = FALSE)$acf[1:order]
  x.sur <- arima.sim(model=list(ar = ar.ls), n = length(x))
  return(invisible(x.sur))
}
