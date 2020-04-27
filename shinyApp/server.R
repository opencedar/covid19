
library(tidyverse)
library(zoo)
library(scales)
library(ggrepel)
library(gganimate)
#source("functions.R")



#### LOAD DATA AND INITIAL (ONE-TIME) TRANSFORMS
state_historical_testing_df <- readRDS("state_historical_testing_df.Rds")

state_historical_testing_df$date <- lubridate::ymd(state_historical_testing_df$date)

state_historical_testing_df <- state_historical_testing_df %>%  mutate(instant_positive_rate = positiveIncrease / totalTestResultsIncrease)

state_historical_testing_df$positiveIncrease[is.na(state_historical_testing_df$positiveIncrease)] <- 0
state_historical_testing_df$totalTestResultsIncrease[is.na(state_historical_testing_df$totalTestResultsIncrease)] <- 0
state_historical_testing_df$date <- lubridate::ymd(state_historical_testing_df$date)

state_pop <- readRDS("state_pop.Rds")
state_pop <- state_pop %>%  
  filter(ages == "total", year == 2013) 

state_historical_testing_df <- state_historical_testing_df %>% 
  inner_join(state_pop, by = 'state') 

state_historical_testing_df$death[is.na(state_historical_testing_df$death)] <- 0
state_historical_testing_df$recovered[is.na(state_historical_testing_df$recovered)] <- 0

active_per_pop = seq(.0001,.02, by = .0001)
r0 = .001 / active_per_pop

point_one_percent = data.frame(active_per_pop, r0)

active_per_pop = seq(.0001,.02, by = .0001)
r0 = .01 / active_per_pop

one_percent = data.frame(active_per_pop, r0)

shinyServer(function(input, output) {
  
refresh <- observeEvent(
  eventExpr = input$refresh,
  handlerExpr =  {
    state_historical_testing_df <- read.csv("https://covidtracking.com/api/v1/states/daily.csv", stringsAsFactors = FALSE) 
    saveRDS(state_historical_testing_df, "state_historical_testing_df.Rds")
  }
)

df_reactive <- reactive({ 
  df_reactive <- state_historical_testing_df %>%
    group_by(state) %>% 
    arrange(date) %>%
    mutate(
      positiveIncrease_ma = rollapply(positiveIncrease,input$mavg ,mean,align='right',fill=NA),
      positiveIncrease_ma_l = dplyr::lag(positiveIncrease_ma, input$lag, order_by = date),
      r0 = positiveIncrease_ma / positiveIncrease_ma_l,
      r0_trailing = dplyr::lag(r0, input$lag, order_by = date),
      r0_growth = (r0 / r0_trailing)-1,
      tests_ma = rollapply(totalTestResultsIncrease, input$mavg, mean, align = 'right',fill=NA),
      tests_ma_trailing = dplyr::lag(tests_ma, input$lag, order_by=date),
      tests_trend = tests_ma / tests_ma_trailing,
      adj_r0 = r0 / tests_trend,
      adj_r0_growth = (adj_r0 / r0_trailing)-1,
      percent_positive_ma = positiveIncrease_ma / tests_ma,
      active_cases = positive - recovered - death,
      active_cases = ifelse(active_cases <=0 , 100, active_cases),
      active_per_pop = active_cases / population,
      adj_r0 = ifelse(adj_r0 <=0, 0.01, adj_r0)) %>% 
    filter(date == input$calc_date)

  return(df_reactive)
})

df_animate <- reactive({ 
  df_animate <- state_historical_testing_df %>%
    group_by(state) %>% 
    arrange(date) %>%
    mutate(
      positiveIncrease_ma = rollapply(positiveIncrease,input$mavg ,mean,align='right',fill=NA),
      positiveIncrease_ma_l = dplyr::lag(positiveIncrease_ma, input$lag, order_by = date),
      r0 = positiveIncrease_ma / positiveIncrease_ma_l,
      r0_trailing = dplyr::lag(r0, input$lag, order_by = date),
      r0_growth = (r0 / r0_trailing)-1,
      tests_ma = rollapply(totalTestResultsIncrease, input$mavg, mean, align = 'right',fill=NA),
      tests_ma_trailing = dplyr::lag(tests_ma, input$lag, order_by=date),
      tests_trend = tests_ma / tests_ma_trailing,
      adj_r0 = r0 / tests_trend,
      adj_r0_growth = (adj_r0 / r0_trailing)-1,
      percent_positive_ma = positiveIncrease_ma / tests_ma,
      active_cases = positive - recovered - death,
      active_cases = ifelse(active_cases <=0 , 100, active_cases),
      active_per_pop = active_cases / population,
      adj_r0 = ifelse(adj_r0 <=0, 0.01, adj_r0)) 
  
  return(df_animate)
})


output$active_r0 <- renderPlot({
  data <- df_reactive()
  ggplot(data, aes(x=active_per_pop)) +
  geom_point(shape=1, show.legend = FALSE, (aes(y=adj_r0, color = state))) + 
  ggtitle(label = "Adjusted R0 by State vs. Current Active Cases", subtitle = paste0("Adjusted R0 Takes Testing Increase Into Account on ",input$calc_date)) +
  
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                    decimal.mark = '.'), limits = c(0,3)) +
  geom_line(data = point_one_percent, aes(y=r0, x = active_per_pop), color = "darkgreen") +
  geom_line(data = one_percent, aes(y=r0, x = active_per_pop), color = "darkred") +
  scale_x_continuous(labels = scales::percent_format(accuracy = .01), limits = c(0.00005,.015), trans = 'log') +
  geom_label_repel(data = data, aes(x=active_per_pop, y=adj_r0, label = state), size = 4) + 
  theme(legend.position = "none") +
  labs(y = paste0("R0 over ", input$lag, " days handicapped for testing performance"), x = "Active cases as a % of population (log)") +
  geom_text(aes(x = .0002, y =2.5, label = paste0("Green: Based on current trajectory\n expect additional \n 0.1% of population infected \nin ", input$lag, " days")), size = 5, color = "darkgreen") +
  geom_text(aes(x = .005, y =2.5, label = paste0("Red: Based on current trajectory\n expect additional \n 1.0% of population infected \nin ", input$lag, " days")), size = 5, color = "darkred")

})

output$animated_r0 <- renderPlot({
  data <- df_animate()
  ggplot(data, aes(x=active_per_pop)) +
    geom_point(shape=1, show.legend = FALSE, (aes(y=adj_r0, color = state))) + 
    transition_time(date) +
    shadow_wake(wake_length = 0.1, alpha = FALSE) +
    ggtitle(label = "Adjusted R0 by State vs. Current Active Cases", 
            subtitle = paste0("Adjusted R0 Takes Testing Increase Into Account on ",{frame_time})
            ) +
    
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                      decimal.mark = '.'), limits = c(0,3)) +
    geom_line(data = point_one_percent, aes(y=r0, x = active_per_pop), color = "darkgreen") +
    geom_line(data = one_percent, aes(y=r0, x = active_per_pop), color = "darkred") +
    scale_x_continuous(labels = scales::percent_format(accuracy = .01), limits = c(0.00005,.015), trans = 'log') +
    geom_label_repel(data = data, aes(x=active_per_pop, y=adj_r0, label = state), size = 4) + 
    theme(legend.position = "none") +
    labs(y = paste0("R0 over ", input$lag, " days handicapped for testing performance"), x = "Active cases as a % of population (log)") +
    geom_text(aes(x = .0002, y =2.5, label = paste0("Green: Based on current trajectory\n expect additional \n 0.1% of population infected \nin ", input$lag, " days")), size = 5, color = "darkgreen") +
    geom_text(aes(x = .005, y =2.5, label = paste0("Red: Based on current trajectory\n expect additional \n 1.0% of population infected \nin ", input$lag, " days")), size = 5, color = "darkred")
}) 

})


