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
                           "Lag Period",
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
            ),
            
            tabPanel("About These Visualizations",
                     fluidPage(
                       tags$h2("About these visualizations"),
                       "These visualizations were built out of frustration for the lack of any real metrics about how ready a state might or might not be for a gradual re-opening from COVID-19 lockdowns of Spring 2020. Fortunately, the COVID Tracking Project has now amassed critical state-by-state data on testing rates that can be used to create baselines over time and form relative judgments.",
                       tags$br(),
                       tags$h3("Tab 1: State-by-State R Estimate vs. Population"),
                       "The first tab shows two key metrics. R is the virus's estimated reproduction rate. This is *not* the famous R0, but is rather the increase or decrease in new tested cases over a number of days. The default number of days is seven, but this can be adjusted.",
                       "This is compared to the percentage of a state's population that is currently infected, based on nasal swabs only—an obvious underestimate, but at least a consistent one.",
                       tags$br(),
                       "These two numbers together show the potential for serious disease in the next week to two weeks; for the simple reason that the virus's growth will be determined by the base infected population multiplied by the growth rate.",
                       "One thing that confounds both of these numbers is testing rate. A state doing more testing now than a week ago will show a higher R; thus, R is adjusted upwards or downwards to take testing volume changes into account.",
                       tags$h4("Key point: A state with a low R and a low percentage of population infected (bottom left) is more ready to reopen."),
                       tags$br(),
                       tags$h3("Tab 2: State-by-State Testing Estimates vs. % Pop Tested"),
                       "The second tab details state's testing success and the percentage of positive cases. The horizontal axis shows the trailing percentage of that state's population that has been tested.",
                       "The vertical axis shows the percentage of tests in that same trailing period that have come back as positive. A state with a low percentage of positive tests, and a high percentage of population tested, is more ready to gradually reopen.",
                       tags$h4("Key point: A state with a low percentage of positive tests and a high percentage of population tested (bottom right) is more ready to reopen.",
                               tags$hr(),
                               "Documentation at ",
                               tags$a(href="https://github.com/opencedar/covid19", "https://github.com/opencedar/covid19"),
                               tags$br(),
                               "Data sourced from ",
                               tags$a(href="https://www.covidtracking.com", "www.covidtracking.com"))
                     )
            )
            
            
          ))