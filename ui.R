library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title=HTML("Atrium Health COVID-19 Hospital Impact Forecasting--Greater Charlotte Area"), titleWidth = "800"),

  ## sidebar
  dashboardSidebar(
    tags$head(tags$style(HTML('.logo {
                              background-color: #008C95 !important;
                              }
                              .navbar {
                              background-color: #008080 !important;
                              }
                                /* main sidebar 
                              .skin-blue .main-sidebar {
                              background-color: #58595B;
                              }*/
                              
                              '))),
    useShinyjs(),
    
    HTML("<img src='AH_logo.png' align='middle'><p></p>"),
    valueBox(d_S, "Greater Charlotte Area Population", icon = icon("users"), width=12, color="red"),
    actionButton("show", HTML("Click for definition of </br>Greater Charlotte Area")), 
    
    dateInput("dat.date", "Date:", value = "2020-09-26"),
    
    h4("ARIMA Model Parameters for Hospital Census:"),
    
    prettySwitch(
      inputId = "switch_auto_arima",
      label = "Optimized ARIMA Parameters", 
      status = "success",
      fill = TRUE),
    
    numericInput('hosp_p', value = default_p, label = "p:", min = 0, step = 1),
    numericInput('hosp_d', value = default_d, label = "d:", min = 0, step = 1),
    numericInput('hosp_q', value = default_q, label = "q:", min = 0, step = 1),
    actionButton("para_setting","Submit Parameters"),
    
    sliderInput("slider_h", label = "No. of days forecast:", min = 7, max = 90, value = default_h, step = 1),

    ## beds stats panel
    br(),
    hr(),
    h4(paste("Update:",bed_stat_date)),
    h4("Copyright 2021 Atrium Health. All rights reserved.")
  ),
  dashboardBody(
    fluidRow(
      column(
        width=12,
        box(
          title="Covid-19 Hospital Census Time Series Model",
          width=12,
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          tabsetPanel(
            tabPanel("All Beds",  plotlyOutput('ts_hosp_figure')),
            tabPanel("ICU", plotlyOutput('ts_icu_figure')),
            tabPanel("Vent", plotlyOutput('ts_vent_figure'))
          )
        )
       )
     ),
    fluidRow(
      column(
        width=12,
        box(
          
          width=12,
          status = "danger", solidHeader = TRUE,
          collapsible = TRUE,
          title="Bed Capacity",##"COVID-19 Hospital Census by Day",
          fluidRow(
          column(
            width = 2,
            selectInput("select_case", "", choices =c("Nominal Scenario"="nominal","Worst Scenario" = "worst") )
          )
          ),
          fluidRow(
          column(
            width = 12,
            plotlyOutput('hosp_cen_stack_figure')
          )
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width=6,
        box(
          width=12,
          status = "warning", solidHeader = TRUE,
          collapsible = TRUE,
          title="Observed COVID-19 ICU Proportion of Hospitalization with 95% Confidence Interval",
          plotlyOutput('icu_obs_ci')
        )
      ),

      column(
        width=6,
        box(
          width=12,
          status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          title="Observed COVID-19 Vent Proportion of ICU Admission with 95% Confidence Interval",
          plotlyOutput('vent_obs_ci')
        )
      )

    )
  )
)