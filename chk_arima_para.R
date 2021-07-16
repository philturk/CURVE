chk_arima_para <- function(bed_stat_dat){
  ## find the Optimized ARIMA Parameters
  lapply(0:2, function(d){
    auto.arima(bed_stat_dat$hosp, seasonal = FALSE, stepwise = FALSE,
               approximation = FALSE, d=d, max.p = 2, max.q = 2)

  }) -> fit_all

  i <- which.min(c(fit_all[[1]]$aicc,fit_all[[2]]$aicc, fit_all[[3]]$aicc))
  
  return(fit_all[[i]])
  
}