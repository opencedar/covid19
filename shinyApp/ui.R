require(tidyverse)

input_df <-
  readRDS("state_historical_testing_df.Rds")

date_input <-  input_df %>%
  mutate(date = lubridate::ymd(input_df$date)) %>%
  select('date') %>%
  summarise(min = min(date), max = max(date))

min_date <- date_input$min
max_date <- date_input$max


fluidPage(
  titlePanel("COVID-19 Analytics"),
          tabsetPanel(
            tabPanel("State-by-State R0 Estimates vs. Population",
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
                    "Lag Period to Calculate R0",
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
                  actionButton("refresh", "Refresh Data")
                ),
                mainPanel(
                  plotOutput("active_r0", height = '800px')
                  )
              )
            )
          ))