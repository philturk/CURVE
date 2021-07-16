rm(list=ls())
require(shiny)
require(shinydashboard)
require(plotly)
require(shinyWidgets)
require(shinyjs)
require(dplyr)
require(forecast)
require(lubridate)
require(tseries)
require(rlist)

## import function of optimized ARIMA Parameters when "Optimized ARIMA Parameters" toggle is on
source("chk_arima_para.R")
## import function of calculating ARIMA results for hospital beds, ICU beds, and Ventilators estimations.
source("mod_fun.R")

storeWarn<- getOption("warn")
options(warn = -1) 

## app/model info
app.version <- "3.0-5.26.2020"
model_version <- "2020-05-26"

## read beds data 
bed_stat_dat <- read.csv("bed_stat.csv")
## read parameters for bed capacity
para <- read.csv("parameters.csv")
para2 <- para %>% select(item, label, stackgroup, fillcolor) %>% unique() %>% mutate(
  y = paste0("~",item)
)

bed_stat_dat$date <- mdy(bed_stat_dat$date)
bed_stat_dat$snapshot <- mdy_hm(bed_stat_dat$snapshot)
para$start_date <- mdy(para$start_date)

## define hospital census, ICU and ventilator probability and their logits. 
bed_stat_dat <- bed_stat_dat %>% mutate(hosp = all_bed + virtual_acute,
                                        Prop_I = icu/hosp, Logit_I = log(Prop_I/(1 - Prop_I)),
                                        Prop_V = vent/icu, Logit_V = log(Prop_V/(1 - Prop_V))
                                        )

bed_stat_dat$Logit_V[bed_stat_dat$Logit_V == Inf] <- log((13/14)/(1- (13/14)))

## calulate icu and vent ci for charts: 
## 1. Observed COVID-19 ICU Proportion of Hospitalization with 95% CI
## 2. Observed COVID-19 Vent Proportion of ICU Admission with 95% Confidence Interval
bed_stat_dat$p_hat_icu <- round(bed_stat_dat$icu/bed_stat_dat$hosp,2)
bed_stat_dat$se_hat_icu <- sqrt(bed_stat_dat$p_hat_icu*(1 - bed_stat_dat$p_hat_icu)/bed_stat_dat$hosp)
bed_stat_dat$p_hat_icu_ci <- round(1.96*bed_stat_dat$se_hat_icu,2)
bed_stat_dat$p_hat_vent <- round(bed_stat_dat$vent/bed_stat_dat$icu,2)
bed_stat_dat$se_hat_vent <- sqrt(bed_stat_dat$p_hat_vent*(1 - bed_stat_dat$p_hat_vent)/bed_stat_dat$icu)
bed_stat_dat$p_hat_vent_ci <- round(1.96*bed_stat_dat$se_hat_vent,2)

## prepare data for Bed Capacity chart.
date1 <- min(para$start_date)
date.end <- max(bed_stat_dat$date)
total_days <- as.integer(date.end - date1)

lapply(unique(para$item), function(i){
  tmp1 <- para %>% filter(item == i)
  result <- vector()
  if(nrow(tmp1) == 1) {
    btd <- as.integer(tmp1$start_date - date1)
    result <- data.frame(
      A = c(rep(0, btd), rep(tmp1$number, total_days - btd + 1))
    )
    names(result) <- i
  } else {
    tmp1 <- tmp1 %>% arrange(start_date)
    num <- c(0, tmp1$number)
    btd <- (c(date1, tmp1$start_date,date.end) - lag(c(date1,tmp1$start_date,date.end))) %>% as.numeric("days") %>% na.omit() %>% as.integer()
    btd[length(btd)] <- btd[length(btd)] + 1
    A <- vector()
    for(j in 1:length(btd)){
      A <- c(A, rep(num[j], btd[j]))
    }
    result <- data.frame(A)
    names(result) <- i
  }
  return(result)
}) %>% list.cbind() -> demand_dat_all_bed
demand_dat_all_bed$date <- seq(date1, date.end, by = "days")
## end of Bed Capacity chart

# ## bed stats date:
index.i <- nrow(bed_stat_dat)
bed_stat_date <- bed_stat_dat$snapshot[index.i]

#grater Charlotte area population
d_S = 2544041

## default ARIMA parameters
default_p <- 2
default_d <- 2
default_q <- 2

## default projecting days
default_h <- 7

## initial ARIMA results
arima_result <- cal_arima(p = default_p, d = default_d, q = default_q, bed_stat_dat =bed_stat_dat)

