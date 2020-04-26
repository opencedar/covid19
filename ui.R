input_df <- readRDS("state_historical_testing_df.Rds")
input_df$date <- lubridate::ymd(input_df$date)
min_date <- max(input_df$date)
max_date <- min(input_df$date)


fluidPage(
  
  
  titlePanel("COVID-19 Analytics"),
  
  tabsetPanel(   
    
    tabPanel("State-by-State R0 Estimates vs. Population", 
             sidebarLayout(
               sidebarPanel(
                 
                 sliderInput("lag", 
                             "Lag Period to Calculate R0",
                             min=2,
                             max=21,
                             value = 7),
                 sliderInput("mavg", 
                             "Moving Average Period", 
                             min=1, 
                             max=7, 
                             value = 3),
                 dateInput("calc_date", 
                           "As-of Date", 
                           value = max_date, 
                           max = max_date, 
                           min = min_date
                 )
               ),
               mainPanel(
                 "Estimating State R0 and Future Course",
                 plotOutput("active_r0")
               )
             )
    )
    
  )
)