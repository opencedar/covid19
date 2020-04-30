require(tidyverse)
library(DT)

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
                               tags$br(),
                               tags$h3("Tab 3: Data Table"),
                               "The raw data on this page can be used to see where the states stack up on the four measures on the previous two tabs. ",
                               "The score metrics are simple multiplication / division of the two source metrics. ",
                               "reopen_score_r0 is adj_r0 * active_per_pop * 1000. reopen_score_test is percent_positive / tests_per_pop. ",
                               tags$h4("High scores are bad, low scores are good."),
                                       
                               
                               tags$hr(),
                               "Documentation at ",
                               tags$a(href="https://github.com/opencedar/covid19", "https://github.com/opencedar/covid19"),
                               tags$br(),
                               "Data sourced from ",
                               tags$a(href="https://www.covidtracking.com", "www.covidtracking.com"))
                     )
            ),
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
                  tags$a(href="https://www.covidtracking.com", "www.covidtracking.com"),
                  tags$hr(),
                  "This visualization compares R (the virus reproduction rate) adjusted for testing volume changes on the vertical axis with the percent of population infected (using nasal swabs, not antibody tests) on the x-axis.  ",
                  "States to the lower left have a combination of low infection rates and low virus reproduction rates—the desired combination for gradual reopening. ",
                  "The two lines represent the points at which states can expect to see 0.1% and 1.0% of their population infected over the next n days (the lag chosen above). ",
                  "The black horizontal and vertical lines indicate the national average for each metric. ",
                  tags$h4("Lower left is good, upper right is bad.")
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
                         tags$a(href="https://www.covidtracking.com", "www.covidtracking.com"),
                         tags$hr(),
                         "This visualization compares the percent of tests (nasal swab) that are positive on the vertical axis with the percent of the population tested over the last n days on the horizontal axis.  ",
                         "States to the lower right have a combination of low test positive rates and high testing volume, a good combination for gradual reopening. ",
                         "The black horizontal and vertical lines indicate the national average for each metric. ",
                         tags$h4("Lower right is good, upper left is bad.")
                       ),
                       mainPanel(
                         
                         plotOutput("testing", height = '800px'),
                         "Black horizontal / vertical lines indicate national totals on this date."
                       )
                     )
            ),
            tabPanel("Data Table",
                     sidebarLayout(
                       sidebarPanel(
                         "Inputs",
                         dateInput(
                           "calc_date_d",
                           "As-of Date",
                           value = max_date,
                           max = max_date,
                           min = min_date
                         ),
                         sliderInput(
                           "lag_d",
                           "Lag Period",
                           min = 2,
                           max = 21,
                           value = 7,
                           step = 1
                         ),
                         sliderInput(
                           "mavg_d",
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
                         tags$a(href="https://www.covidtracking.com", "www.covidtracking.com"),
                         tags$hr(),
                         "The raw data to the right can be used to see where the states stack up on the four measures on the previous two tabs. ",
                         "The score metrics are simple multiplication / division of the two source metrics. ",
                         "reopen_score_r0 is adj_r0 * active_per_pop * 1000. reopen_score_test is percent_positive / tests_per_pop. Raw scores aren't shown as they aren't interpretable; relative ranks between states are shown.",
                         tags$h4("Low scores are good. States arranged most to least ready to open based on a composite of the two scores.")
                       ),
                       mainPanel(
                         
                         DT::DTOutput("table"),
                         "Data in a tabular format."
                       )
                     )
            )
            
            
           
            
            
          ))