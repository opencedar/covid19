covid_ets <- read.csv("covid_ets.csv", stringsAsFactors = FALSE)
country_input <- as.character(unique(covid_ets$Country.Region))
state_input <- as.character(unique(covid_ets$Province.State))


fluidPage(
  
  
  titlePanel("COVID-19 Analytics"),
  
  tabsetPanel(               
    tabPanel("Time Series", h2("Time Series by Country, Region"), 
             sidebarLayout(
               sidebarPanel(
                 
                 #selectInput("countries", "Countries", 
                  #           choices = country_input, multiple = FALSE),
                 selectInput("states", "States-Provinces",
                             choices = state_input, multiple = FALSE, selected = "California"),
                 actionButton("ts_action", "Submit", icon = NULL, width = NULL)
               ),
               mainPanel(
                 "Explore country and regional COVID-19 time series for confirmed, deaths, and recovered cases",
                 h6("Andy Hasselwander, 2020"),
                 plotOutput("ts_plot")
               )
             )
      )
    )
  )