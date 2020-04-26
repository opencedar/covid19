
library(tidyverse)
library(zoo)
library(scales)
library(ggrepel)
#source("functions.R")


#### LOAD DATA AND INITIAL (ONE-TIME) TRANSFORMS
state_historical_testing_df <- readRDS("state_historical_testing_df.Rds")

state_historical_testing_df$date <- lubridate::ymd(state_historical_testing_df$date)

state_historical_testing_df <- state_historical_testing_df %>%  mutate(instant_positive_rate = positiveIncrease / totalTestResultsIncrease)
state_current_testing_df <- state_current_testing_df %>% mutate(positive_rate = positive/  totalTestResults)

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
r0_7 = .001 / active_per_pop

point_one_percent = data.frame(active_per_pop, r0_7)

active_per_pop = seq(.0001,.02, by = .0001)
r0_7 = .01 / active_per_pop

one_percent = data.frame(active_per_pop, r0_7)


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
      adj_r0 = ifelse(adj_r0 <=0, 0.01, adj_r0)
    ) %>% 
    filter('date' == input$calc_date)

})


active_r0 <- renderPlot({
  df <- df_reactive()
  ggplot(data = df, aes(x=active_per_pop)) 
  geom_point(shape=1, show.legend = FALSE, (aes(y=adj_r0_7, color = state))) + 
  ggtitle(label = "Adjusted R0 by State vs. Current Active Cases", subtitle = paste0("Adjusted R0 Takes Testing Increase Into Account on ",input$date)) +
  
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                    decimal.mark = '.'), limits = c(0,3)) +
  geom_line(data = point_one_percent, aes(y=r0_7, x = active_per_pop), color = "darkgreen") +
  geom_line(data = one_percent, aes(y=r0_7, x = active_per_pop), color = "darkred") +
  scale_x_continuous(labels = scales::percent_format(accuracy = .01), limits = c(0.00005,.0125), trans = 'log') +
  geom_label_repel(data=  df, aes(x=active_per_pop, y=adj_r0_7, label = state), size = 2) + 
  theme(legend.position = "none") +
  labs(y = "R0 over 7 days handicapped for testing performance", x = "Active cases as a % of population (log)") +
  geom_text(aes(x = .0002, y =2.5, label = "Green: Based on current trajectory\n expect additional \n 0.1% of population infected \nin one week"), size = 3, color = "darkgreen") +
  geom_text(aes(x = .005, y =2.5, label = "Red: Based on current trajectory\n expect additional \n 1.0% of population infected \nin one week"), size = 3, color = "darkred")

})


