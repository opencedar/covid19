require(tidyverse)

input_df <- read.csv("https://covidtracking.com/api/v1/states/daily.csv", stringsAsFactors = FALSE) 

date_input <-  input_df %>%
  mutate(date = lubridate::ymd(input_df$date)) %>%
  select('date') %>%
  summarise(min = min(date), max = max(date))

min_date <- date_input$min
max_date <- date_input$max


fluidPage(
  titlePanel("COVID-19 U.S. State-by-State Analytics"),
          tabsetPanel(
            tabPanel("State-by-State R Estimates vs. Population",
              sidebarLayout(
                sidebarPanel(
                  "Inputs",
                   dateInput(
                     "calc_date",
                     "As-of Date",
                     value = max_date,
                     max = max_date,
                     min = min_date
                   ),
                  sliderInput(
                    "lag",
                    "Lag Period to Calculate R",
                    min = 2,
                    max = 21,
                    value = 7,
                    step = 1
                  ),
                  sliderInput(
                    "mavg",
                    "Moving Average Period",
                    min = 1,
                    max = 7,
                    value = 3,
                    step = 1
                  ),
                  tags$hr(),
                  "Documentation at ",
                  tags$a(href="https://github.com/opencedar/covid19", "https://github.com/opencedar/covid19"),
                  tags$br(),
                  "Data sourced from ",
                  tags$a(href="https://www.covidtracking.com", "www.covidtracking.com")
                ),
                mainPanel(
                  plotOutput("active_r0", height = '800px'),
                  "Black horizontal / vertical lines indicate national totals on this date."
                  )
              )
            ),
            
            tabPanel("State-by-State Testing Estimates vs. % Pop Tested",
                     sidebarLayout(
                       sidebarPanel(
                         "Inputs",
                         dateInput(
                           "calc_date_t",
                           "As-of Date",
                           value = max_date,
                           max = max_date,
                           min = min_date
                         ),
                         sliderInput(
                           "lag_t",
                           "Lag Period to Calculate R0",
                           min = 2,
                           max = 21,
                           value = 7,
                           step = 1
                         ),
                         sliderInput(
                           "mavg_t",
                           "Moving Average Period",
                           min = 1,
                           max = 7,
                           value = 3,
                           step = 1
                         ),
                         
                         tags$hr(),
                         "Documentation at ",
                         tags$a(href="https://github.com/opencedar/covid19", "https://github.com/opencedar/covid19"),
                         tags$br(),
                         "Data sourced from ",
                         tags$a(href="https://www.covidtracking.com", "www.covidtracking.com")
                       ),
                       mainPanel(
                         
                         plotOutput("testing", height = '800px'),
                         "Black horizontal / vertical lines indicate national totals on this date."
                       )
                     )
            )
            
            
          ))