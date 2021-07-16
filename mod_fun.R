cal_arima <- function(p, d, q, bed_stat_dat){
  ## calculate hospital census results from ARIMA based on the given p, d, q
  ## 
  
  fit_hosp <- Arima(bed_stat_dat$hosp, order = c(p,d,q))
  
  fit_icu_1 <- auto.arima(bed_stat_dat$icu, seasonal = FALSE, stepwise = FALSE, 
                    approximation = FALSE)
  
  fit_icu_2 <- auto.arima(bed_stat_dat$Logit_I, seasonal = FALSE, stepwise = FALSE, 
                          approximation = FALSE)
  
  fit_vent_1 <- auto.arima(bed_stat_dat$vent, seasonal = FALSE, stepwise = FALSE, 
                           approximation = FALSE)
  
  fit_vent_2 <- auto.arima(bed_stat_dat$Logit_V, seasonal = FALSE, stepwise = FALSE, 
                           approximation = FALSE)
  
  result <- list(hosp = fit_hosp, 
                 icu1 = fit_icu_1, 
                 icu2 = fit_icu_2, 
                 vent1 = fit_vent_1, 
                 vent2 = fit_vent_2)
  
  return(result)
}