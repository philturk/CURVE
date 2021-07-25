server <- function(input, output, session) { 

  update_all_data<-reactiveValues(
                                  arima_result = arima_result,
                                  hosp_list = NA,
                                  ICU_list = NA,
                                  Vent_list = NA,
                                  demand_dat_all_bed = demand_dat_all_bed
  )
  
    config <- reactive({
   
      p <- input$hosp_p
      d <- input$hosp_d
      q <- input$hosp_q
      
      dat.date <- input$dat.date

      result <- cal_arima(p,d,q,bed_stat_dat %>% filter(date <= dat.date))
      
      update_all_data$arima_result <- result
      
      ## update hospital census
      fit <- result$hosp
      h <- input$slider_h
      
      dat2 <- bed_stat_dat %>% filter(date <= dat.date)
      
      p02_fcast <- forecast(fit, h = h)
      
      upper <- fitted(fit) + 2*sqrt(fit$sigma2)
      lower <- fitted(fit) - 2*sqrt(fit$sigma2)
      poly01 <- data.frame(date = as.Date(c(dat2$date, rev(dat2$date))), 
                           Bounds = c(upper, rev(lower)))
      p01_fcast <- forecast(fit, h = h, level = c(95))
      poly02 <- data.frame(date = c(last(dat2$date) + days(1:h), rev(last(dat2$date) + days(1:h))), 
                           Bounds = c(p01_fcast$upper, rev(p01_fcast$lower)))
      p01_graph <- data.frame(date = c(dat2$date, last(dat2$date) + days(1:h)),
                              Hosp = c(dat2$hosp, rep(NA, h)),
                              Fit = c(ifelse(fitted(fit) < 0, 0, fitted(fit)), ifelse(p02_fcast$mean < 0, 0, p02_fcast$mean)))
      
      
      p02_fcast$lower <- ifelse(p02_fcast$lower <= 0, 0, p02_fcast$lower)
      poly02_80 <- data.frame(date = c(last(dat2$date) + days(1:h), rev(last(dat2$date) + days(1:h))), 
                              Bounds = c(p02_fcast$upper[,1], rev(p02_fcast$lower[,1])))
      poly02_95 <- data.frame(date = c(last(dat2$date) + days(1:h), rev(last(dat2$date) + days(1:h))), 
                              Bounds = c(p02_fcast$upper[,2], rev(p02_fcast$lower[,2])))
      hosp_holder <- data.frame(date = c(dat2$date, last(dat2$date) + days(1:h)),
                                Hosp = c(dat2$hosp, rep(NA, h)),
                                H_Fit_cur = c(ifelse(fitted(fit)<0, 0, fitted(fit)), ifelse(p02_fcast$mean < 0, 0, p02_fcast$mean)),
                                H_Fit_worst = c(ifelse(fitted(fit)<0, 0, fitted(fit)), ifelse(p02_fcast$upper[,1] < 0, 0, p02_fcast$upper[,1]))
                                )
      
      hosp_list <- list(hosp_holder = hosp_holder, poly01 = poly01,  poly02_80 = poly02_80, poly02_95 = poly02_95)
      
      update_all_data$hosp_list <- hosp_list
      
      ## update ICU census
      fit1 <- result$icu1
      fit2 <- result$icu2
      
      upper <- fitted(fit1) + 2*sqrt(fit1$sigma2)
      lower <- fitted(fit1) - 2*sqrt(fit1$sigma2)
      poly03 <- data.frame(date = c(dat2$date, rev(dat2$date)), 
                           Bounds = c(upper, rev(lower)))
      p03_graph <- data.frame(date = c(dat2$date, last(dat2$date) + days(1:h)),
                              ICU = c(dat2$icu, rep(NA, h)),
                              Fit = c(ifelse(fitted(fit1)<0, 0, fitted(fit1)), rep(NA, h)))
      
      
      ICU_fcast <- forecast(fit2, h = h, level = c(95))
      
      k <- exp(ICU_fcast$mean)/(1 + exp(ICU_fcast$mean))
      
      na_1 <- attributes(k)$tsp[1]
      na_2 <- attributes(k)$tsp[2]
      
      temp03 <- full_join(hosp_holder, dat2, by = "date")
      
      ICU_holder <- temp03 %>% mutate(I_Fit_cur = c(rep(NA, na_1-1), H_Fit_cur[na_1:na_2]*k),
                                      I_Fit_worst = c(rep(NA,na_1-1), H_Fit_worst[na_1:na_2]*k))
      
      ICU_list <- list(ICU_holder = ICU_holder, poly03 = poly03)
      update_all_data$ICU_list <- ICU_list
      
      ## update Vent census
      fit3 <- result$vent1
      fit4 <- result$vent2
      
      upper <- fitted(fit3) + 2*sqrt(fit3$sigma2)
      lower <- fitted(fit3) - 2*sqrt(fit3$sigma2)
      poly04 <- data.frame(date = c(dat2$date, rev(dat2$date)), 
                           Bounds = c(upper, rev(lower)))
      p04_graph <- data.frame(date = c(dat2$date, last(dat2$date) + days(1:h)),
                              Vent = c(dat2$vent, rep(NA, h)),
                              Fit = c(ifelse(fitted(fit3)<0, 0, fitted(fit3)), rep(NA, h)))
      
      
      V_fcast <- forecast(fit4, h = h, level = c(95))
      
      k <- exp(V_fcast$mean)/(1 + exp(V_fcast$mean)) 
      
      na_1 <- attributes(k)$tsp[1]
      na_2 <- attributes(k)$tsp[2]
      
      V_holder <- ICU_holder %>% mutate(V_Fit_cur = c(rep(NA, na_1 - 1), I_Fit_cur[na_1:na_2]*k),
                                        V_Fit_worst = c(rep(NA, na_1 - 1), I_Fit_worst[na_1:na_2]*k))
      
      Vent_list <- list(V_holder = V_holder, poly04 = poly04)
      update_all_data$Vent_list <- Vent_list
      
      ## update bed capacity
      tmp2 <- tail(demand_dat_all_bed, n = 1)
      lapply(names(tmp2), function(i){
        result <- vector()
        if(i == "date") {
          result <- data.frame(
            A = date(seq((tmp2[1,i] + days(1)), (tmp2[1,i] + days(h)), by = "days"))
          )
          names(result) <- i
        } else {
          result <- data.frame(
            A = rep(tmp2[,i], h)
          )
          names(result) <- i
        }
        return(result)
      }) %>% list.cbind() -> tmp3
      
      demand_dat_all_bed2 <- bind_rows(demand_dat_all_bed, tmp3)
      demand_dat_all_bed2$date <- date(demand_dat_all_bed2$date)
 
      update_all_data$demand_dat_all_bed <- demand_dat_all_bed2
      
    })
    
    ## chart of bed capacity
    hosp_cen_stack <- reactive({
      options(warn = -1) 
      
      demand_dat_all_bed <- update_all_data$demand_dat_all_bed
      tmp01 <- update_all_data$hosp_list$hosp_holder[,c("date", "H_Fit_cur", "H_Fit_worst")]
      
      all_data2 <- left_join(demand_dat_all_bed, tmp01, by = "date")
      pred_non_covid <- all_data2$pred_non_covid
      nominal_case <- all_data2$H_Fit_cur
      worst_case <- all_data2$H_Fit_worst

      case1 <- ifelse(input$select_case == "nominal", "~nominal_case","~worst_case")
      case1 <- as.formula(case1)
      case_label <- ifelse(input$select_case == "nominal", "Nominal Scenario", "Worst Scenario")

      ## stackgroup one (area chart)
      para3 <- para2 %>% filter(stackgroup == "one")
      fig <- plot_ly(all_data2, x = ~date, y = as.formula(para3[1,"y"]), name = para3[1,"label"], mode = 'none', type = 'scatter', stackgroup = para3[1,"stackgroup"], fillcolor = para3[1,"fillcolor"])
      for(i in 2:nrow(para3)){
        fig <- fig %>% add_trace(y = as.formula(para3[i, "y"]), name = para3[i, "label"], stackgroup = para3[i, "stackgroup"], fillcolor = para3[i, "fillcolor"])
      }
      para4 <- para2 %>% filter(stackgroup == "two")
      ## stackgroup two (bar chart)
      for(i in 1:nrow(para4)){
        fig <- fig %>% add_trace(y = as.formula(para4[i, "y"]), name = para4[i, "label"], type = "bar", marker = list(color = para4[i, "fillcolor"]), stackgroup = para4[i, "stackgroup"])
      }
      
      fig <- fig %>% add_trace(y = case1, name = paste0("Total Occupied \nCovid Projected (",case_label,")"),type='bar', marker=list(color = 'rgba(255, 0, 0,0.7)'), stackgroup = 'two')
      fig <- fig %>% layout(yaxis = list(title="number of patients"), barmode = 'stack')
      
      fig

    })

    
    ## hospitalization census chart
    ts_hosp_react <- reactive({
      
      hosp_holder <- update_all_data$hosp_list$hosp_holder
      poly01 <- update_all_data$hosp_list$poly01
      poly02_80 <- update_all_data$hosp_list$poly02_80
      poly02_95 <- update_all_data$hosp_list$poly02_95

      h <- input$slider_h
      dat.date <- input$dat.date

      fig <- plot_ly(hosp_holder, x = ~date, y = ~H_Fit_cur, name = paste0("Covid-19 Hospital Census Time Series Model,\n",h,"-Day Forecast CRI Region as of ",format(dat.date, "%B %d, %Y")), mode = 'lines', type = 'scatter')
      fig <- fig %>% add_markers(data = bed_stat_dat, x = ~date, y=~hosp, name = "Observed Hospital Census")
      fig <- fig %>% add_polygons(data = poly01, x=~date, y=~Bounds, color = I("#00BFC4"), opacity = 0.5, name = "95% Prediction Interval for In-Sample One-Step Forecasts")
      fig <- fig %>% add_polygons(data = poly02_80, x=~date, y=~Bounds, color = I("#00BFC4"), opacity = 0.5, name = "80% Prediction Interval for Out-of-Sample Forecasts ")
      fig <- fig %>% add_polygons(data = poly02_95, x=~date, y=~Bounds, color = I("#F8766D"), opacity = 0.5, name = "95% Prediction Interval for Out-of-Sample Forecasts ")

      fig <- fig %>% layout(yaxis = list(title="number of patients", showspikes = TRUE),
                            xaxis = list(showspikes = T)
      )
      
      fig
    })
    
    
    ## icu census chart
    ts_icu_react <- reactive({
        
        h <- input$slider_h
        dat.date <- input$dat.date
        
        ICU_holder <- update_all_data$ICU_list$ICU_holder
        poly03 <- update_all_data$ICU_list$poly03

        fig <- plot_ly(ICU_holder, x = ~date, y = ~I_Fit_cur, name = "Nominal Scenario", mode = 'lines', type = 'scatter')
        fig <- fig %>% add_trace(y = ~I_Fit_worst, name = "Worst Scenario")
        fig <- fig %>% add_markers(data = bed_stat_dat, x = ~date, y=~icu, color = I("red"), name = "Observed ICU Census")
        fig <- fig %>% add_polygons(data = poly03, x=~date, y=~Bounds, color = I("#00BFC4"), opacity = 0.5, name = "95% Prediction Interval for In-Sample One-Step Forecasts")
        fig <- fig %>% layout(yaxis = list(title="number of patients", showspikes = TRUE),
                              xaxis = list(showspikes = T)
        )

        fig
    })
    
    ## vent census chart
    ts_vent_react <- reactive({
      
      h <- input$slider_h
      dat.date <- input$dat.date
      
      V_holder <- update_all_data$Vent_list$V_holder
      poly04 <- update_all_data$Vent_list$poly04
      
      fig <- plot_ly(V_holder, x = ~date, y = ~V_Fit_cur, name = "Nominal Scenario", mode = 'lines', type = 'scatter')
      fig <- fig %>% add_trace(y = ~V_Fit_worst)
      fig <- fig %>% add_markers(data = bed_stat_dat, x = ~date, y=~vent, color = I("red"), name = "Observed ICU Census")
      fig <- fig %>% add_polygons(data = poly04, x=~date, y=~Bounds, color = I("#00BFC4"), opacity = 0.5, name = "95% Prediction Interval for In-Sample One-Step Forecasts")
      fig <- fig %>% layout(yaxis = list(title="number of patients", showspikes = TRUE),
                            xaxis = list(showspikes = T)
      )

      fig
      
    })

    output$slider_day_output <- renderUI({
      end.index <- nrow(all_data_everything)
      numinputs <- list(sliderInput("slider_day", label = "Choose Days:", min = 7, max = end.index-day0.index-1, value = n_day))
      shinyjs::hidden(numinputs)
    })
    
    observeEvent(input$dat.date, {
      config()
    })
    
    observeEvent(input$para_setting,{
      config()
    })
    
    observeEvent(input$slider_h,{
      config()
    }, ignoreInit = T)
    
    observeEvent(input$switch_auto_arima,{
      if(input$switch_auto_arima){
        dat.date <- input$dat.date
        fit_hosp <- chk_arima_para(bed_stat_dat %>% filter(date <= dat.date))

        p <- length(fit_hosp$model$phi)
        d <- length(fit_hosp$model$Delta)
        q <- length(fit_hosp$model$theta)
      
        
        updateNumericInput(session, inputId = "hosp_p", value = p)
        updateNumericInput(session, inputId = "hosp_d", value = d)
        updateNumericInput(session, inputId = "hosp_q", value = q)
        
        shinyjs::disable("hosp_p")
        shinyjs::disable("hosp_d")
        shinyjs::disable("hosp_q")

      } else {
        shinyjs::enable("hosp_p")
        shinyjs::enable("hosp_d")
        shinyjs::enable("hosp_q")
        
      }
    })#,ignoreInit = T)
    
    ###############################################
    ### charts output
    
    output$ts_hosp_figure <- renderPlotly({
      ts_hosp_react()
    })
    
    output$ts_icu_figure <- renderPlotly({
      ts_icu_react()
    })
    
    output$ts_vent_figure <- renderPlotly({
      ts_vent_react()
    })
    
    output$hosp_cen_stack_figure <- renderPlotly({
      hosp_cen_stack()
    })
    
    output$icu_obs_ci <- renderPlotly({
      all_data2 <- bed_stat_dat[,c("date", "p_hat_icu", "se_hat_icu", "p_hat_icu_ci")]
      fig <- plot_ly(all_data2, x = ~date, y=~p_hat_icu,
                     name = "ICU Proportion of Hopspitalization with 95% Confidence Interval",
                     type = 'scatter',
                     mode = 'lines+markers',
                     color = "#f7a635",
                     error_y = list(array=~p_hat_icu_ci, color = '#333333'))
      fig <- fig %>% layout(yaxis = list(title="ICU Propotion"))
      fig
    })

    output$vent_obs_ci <- renderPlotly({
      all_data2 <- bed_stat_dat[,c("date", "p_hat_vent", "se_hat_vent", "p_hat_vent_ci")]
      fig <- plot_ly(all_data2, x = ~date, y=~p_hat_vent,
                     name = "Vent Proportion of ICU Admission with 95% Confidence Interval",
                     type = 'scatter',
                     mode = 'lines+markers',

                     error_y = list(array=~p_hat_vent_ci, color = '#333333'))
      fig <- fig %>% layout(yaxis = list(title="Vent Propotion"))
      fig
    })


    observeEvent(input$show, {
      showModal(modalDialog(
        title = "Greater Charlotte Area",
        paste("The greater Charlotte Area includes the counties of Anson, Cabarrus, Catawba, Cleveland, Gaston, Iredell, Lincoln, Mecklenburg,
Rowan, Stanly, and Union. Altogether, the population size is estimated to be 2,544,041.",
              "Hospitals: CMC,	University City,	Mercy,	Pineville,	Kings Mountain,	Stanly,	Lincoln,	Union,	Anson,	Cabarrus, and	Cleveland"),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    
    
    
## end   
} 
