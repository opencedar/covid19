

mavg = 7
lag = 7
calc_date = Sys.Date()

library(tidyverse)
library(zoo)
library(scales)
library(ggrepel)
library(gganimate)
library(gifski)
library(gapminder)
#source("functions.R")



#### LOAD DATA AND INITIAL (ONE-TIME) TRANSFORMS
state_historical_testing_df <- read.csv("https://covidtracking.com/api/v1/states/daily.csv", stringsAsFactors = FALSE) 

state_historical_testing_df$date <- lubridate::ymd(state_historical_testing_df$date)

state_historical_testing_df <- state_historical_testing_df %>%  mutate(instant_positive_rate = positiveIncrease / totalTestResultsIncrease)

state_historical_testing_df$positiveIncrease[is.na(state_historical_testing_df$positiveIncrease)] <- 0
state_historical_testing_df$totalTestResultsIncrease[is.na(state_historical_testing_df$totalTestResultsIncrease)] <- 0
state_historical_testing_df$date <- lubridate::ymd(state_historical_testing_df$date)

state_pop <- readRDS("state_pop.Rds")
state_pop <- state_pop %>%  
  filter(ages == "total", year == 2013) 

us_pop <- unlist(state_pop %>%
  filter(state == "USA") %>%
  select(population) )

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

df_reactive <- state_historical_testing_df %>%
  group_by(state) %>% 
  arrange(date) %>%
  mutate(
    positiveIncrease_ma = rollapply(positiveIncrease,mavg ,mean,align='right',fill=NA),
    positiveIncrease_ma_l = dplyr::lag(positiveIncrease_ma, lag, order_by = date),
    r0 = positiveIncrease_ma / positiveIncrease_ma_l,
    r0_trailing = dplyr::lag(r0, lag, order_by = date),
    r0_growth = (r0 / r0_trailing)-1,
    tests_ma = rollapply(totalTestResultsIncrease, mavg, mean, align = 'right',fill=NA),
    tests_ma_trailing = dplyr::lag(tests_ma, lag, order_by=date),
    tests_trend = tests_ma / tests_ma_trailing,
    adj_r0 = r0 / tests_trend,
    adj_r0_growth = (adj_r0 / r0_trailing)-1,
    percent_positive_ma = positiveIncrease_ma / tests_ma,
    active_cases = positive - recovered - death,
    active_cases = ifelse(active_cases <=0 , 100, active_cases),
    active_per_pop = active_cases / population,
    adj_r0 = ifelse(adj_r0 <=0, 0.01, adj_r0)
  ) %>% 
  filter(date == calc_date)


# data <- df_reactive
# ggplot(data) +
#   geom_point(shape=1, show.legend = FALSE, (aes(x = active_per_pop, y=adj_r0, color = state))) + 
#   ggtitle(label = "Adjusted R0 by State vs. Current Active Cases", subtitle = paste0("Adjusted R0 Takes Testing Increase Into Account on ",calc_date)) +
#   # 
#   scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
#                                                      decimal.mark = '.'), limits = c(0,3)) +
#    geom_line(data = point_one_percent, aes(y=r0, x = active_per_pop), color = "darkgreen") +
#   geom_line(data = one_percent, aes(y=r0, x = active_per_pop), color = "darkred") +
#   scale_x_continuous(labels = scales::percent_format(accuracy = .01), limits = c(0.00005,.015), trans = 'log') +
#   geom_label_repel(data = data, aes(x=active_per_pop, y=adj_r0, label = state), size = 2) + 
#   theme(legend.position = "none") +
#   labs(y = paste0("R0 over ", lag, " days handicapped for testing performance"), x = "Active cases as a % of population (log)") 
  #geom_text(aes(x = .0002, y =2.5, label = paste0("Green: Based on current trajectory\n expect additional \n 0.1% of population infected \nin ", lag, " days")), size = 3, color = "darkgreen") 
  # geom_text(aes(x = .005, y =2.5, label = paste0("Red: Based on current trajectory\n expect additional \n 1.0% of population infected \nin ", lag, " days")), size = 3, color = "darkred")
  
  
  
  
  
  
  df_animate<- state_historical_testing_df %>%
  group_by(state) %>% 
  arrange(date) %>%
  mutate(
    positiveIncrease_ma = rollapply(positiveIncrease,mavg ,mean,align='right',fill=NA),
    positiveIncrease_ma_l = dplyr::lag(positiveIncrease_ma, lag, order_by = date),
    r0 = positiveIncrease_ma / positiveIncrease_ma_l,
    r0_trailing = dplyr::lag(r0, lag, order_by = date),
    r0_growth = (r0 / r0_trailing)-1,
    tests_ma = rollapply(totalTestResultsIncrease, mavg, mean, align = 'right',fill=NA),
    tests_ma_trailing = dplyr::lag(tests_ma, lag, order_by=date),
    tests_trend = tests_ma / tests_ma_trailing,
    adj_r0 = r0 / tests_trend,
    adj_r0_growth = (adj_r0 / r0_trailing)-1,
    percent_positive_ma = positiveIncrease_ma / tests_ma,
    active_cases = positive - recovered - death,
    active_cases = ifelse(active_cases <=0 , 100, active_cases),
    active_per_pop = active_cases / population,
    adj_r0 = ifelse(adj_r0 <=0, 0.01, adj_r0),
    tests_per_pop = tests_ma/ population
  )
  
  df_animate_total <- df_animate %>%
    group_by(date) %>%
    summarise(
      positiveIncrease_ma_t = sum(positiveIncrease_ma, na.rm=TRUE),
      positiveIncrease_ma_l_t = sum(positiveIncrease_ma_l, na.rm=TRUE),
      tests_ma_t = sum(tests_ma, na.rm=TRUE),
      tests_ma_trailing_t = sum(tests_ma_trailing, na.rm=TRUE),
      active_cases_t = sum(active_cases, na.rm=TRUE),
    ) %>% mutate (
      r0_t = positiveIncrease_ma_t / positiveIncrease_ma_l_t,
      tests_trend_t = tests_ma_t / tests_ma_trailing_t,
      adj_r0_t = r0_t / tests_trend_t,
      population_t = us_pop,
      tests_per_pop_t = tests_ma_t / population_t,
      active_per_pop_t =active_cases_t / population_t,
      active_per_pop_t = ifelse(active_per_pop_t <=0, 0.0001, active_per_pop_t),
      adj_r0_t = ifelse(adj_r0_t <=0, 0.0001, adj_r0_t),
      percent_positive_ma_t = positiveIncrease_ma_t / tests_ma_t
    )
  
data <- df_animate %>% inner_join(df_animate_total, by = 'date')
  
  
data <- data %>% filter(date >= lubridate::ymd("20200310"))



p <- data %>% 
  ggplot() +
  geom_point(shape=1, show.legend = FALSE, (aes(x = active_per_pop, y=adj_r0, color = state))) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                    decimal.mark = '.'), limits = c(0,3)) +
  geom_line(data = point_one_percent, aes(y=r0, x = active_per_pop), color = "darkgreen") +
  xlab("Active cases per population") +
  ylab(paste0("Adjusted R moving average: ", lag, " days")) +
  geom_line(data = one_percent, aes(y=r0, x = active_per_pop), color = "darkred") +
  geom_hline(aes(yintercept = adj_r0_t), color = "black", size = 1) +
  geom_vline(aes(xintercept = active_per_pop_t), color = "black", size = 1) +
  scale_x_continuous(labels = scales::percent_format(accuracy = .01), limits = c(0.00005,.015), trans = 'log') +
  geom_label(aes(x=active_per_pop, y=adj_r0, label = state, color =state), size = 3) + 
  theme(legend.position = "none") 
  

 a <-  p + 
    transition_time(date) +
    labs(title = "Date: {frame_time}") 
  
 animate(a, duration = 30, fps = 20, width = 600, height = 600, end_pause = 50, renderer = gifski_renderer())
 anim_save("20200426_states_covid_r0.gif")
 
 
 #Test rate
 p2 <- ggplot(data, aes(x=tests_per_pop)) +
   geom_point(shape=1, show.legend = FALSE, (aes(y=percent_positive_ma, color = state))) +
   scale_y_continuous(labels = scales::percent_format(accuracy = 1,
                                                     decimal.mark = '.'), limits = c(0,.6)) +
   xlab("Tests per population") +
   ylab(paste0( lag, " day moving avg positive test rate")) +
   scale_x_continuous(labels = scales::percent_format(accuracy = .1), limits = c(0,.0025)) +
   geom_hline(aes(yintercept = percent_positive_ma_t), color = "black", size = 1) +
   geom_vline(aes(xintercept = tests_per_pop_t), color = "black", size = 1) +
   geom_label(data = data, aes(x=tests_per_pop, y=percent_positive_ma, label = state, color =state), size = 3) + 
   theme(legend.position = "none") 
 
 
 a2 <-  p2 + 
   transition_time(date) +
   labs(title = "Date: {frame_time}") 
 
 animate(a2, duration = 30, fps = 20, width = 600, height = 600,  end_pause = 50, renderer = gifski_renderer())
 anim_save("20200426_states_covid_testing.gif")
  
  



  
  
  
  