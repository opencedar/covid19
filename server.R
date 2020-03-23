library(shiny)
library(tidyverse)

covid_ets <- read.csv("covid_ets.csv")
covid_graph <- covid_ets %>% gather(metric, cases, confirmed:recovered)

shinyServer(function(input, output) {

  data <- eventReactive(input$ts_action, {
    covid_graph %>% filter(#Country.Region == input$country, 
      Province.State == input$states)
    })
  
  output$ts_plot <- renderPlot({
    ggplot(data(), aes(x=date, y=cases, color = metric, group = metric)) +
      geom_line(aes(color = metric), size =1) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
    
  })