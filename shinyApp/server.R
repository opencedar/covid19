
library(tidyverse)
library(zoo)
library(scales)



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

shinyServer(function(input, output) {
  

  
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
    
    df_total <- df_reactive %>%
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
    
    data <- df_reactive %>% inner_join(df_total, by = 'date')
    
    return(data)
  })
  
  
  output$active_r0 <- renderPlot({
    data <- df_reactive()
    ggplot(data, aes(x=active_per_pop)) +
      geom_point(shape=1, show.legend = FALSE, (aes(y=adj_r0, color = state))) + 
      ggtitle(label = "Adjusted R by State vs. Current Active Cases", subtitle = paste0("Adjusted R Takes Testing Increase Into Account on ",input$calc_date)) +
      
      scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                        decimal.mark = '.'), limits = c(0,3)) +
      geom_line(data = point_one_percent, aes(y=r0, x = active_per_pop), color = "darkgreen") +
      geom_line(data = one_percent, aes(y=r0, x = active_per_pop), color = "darkred") +
      scale_x_continuous(labels = scales::percent_format(accuracy = .01), limits = c(0.00005,.015), trans = 'log') +
      geom_hline(aes(yintercept = adj_r0_t), color = "black", size = 1) +
      geom_vline(aes(xintercept = active_per_pop_t), color = "black", size = 1) +
      geom_label(data = data, aes(x=active_per_pop, y=adj_r0, label = state), size = 5, alpha = 0.8) + 
      theme(legend.position = "none",
            axis.text = element_text(size = 16),
            axis.title = element_text(size = 16),
            title = element_text(size = 18)) +
      labs(y = paste0("R0 over ", input$lag, " days handicapped for testing performance"), x = "Active cases as a % of population (log)") +
      geom_text(aes(x = .0002, y =2.5, label = paste0("Green: Based on current trajectory\n expect additional \n 0.1% of population infected \nin ", input$lag, " days")), size = 5, color = "darkgreen") +
      geom_text(aes(x = .005, y =2.5, label = paste0("Red: Based on current trajectory\n expect additional \n 1.0% of population infected \nin ", input$lag, " days")), size = 5, color = "darkred")
    
  })
  
  
  
  df_reactive_t <- reactive({ 
    df_reactive <- state_historical_testing_df %>%
      group_by(state) %>% 
      arrange(date) %>%
      mutate(
        positiveIncrease_ma = rollapply(positiveIncrease,input$mavg_t ,mean,align='right',fill=NA),
        positiveIncrease_ma_l = dplyr::lag(positiveIncrease_ma, input$lag_t, order_by = date),
        r0 = positiveIncrease_ma / positiveIncrease_ma_l,
        r0_trailing = dplyr::lag(r0, input$lag_t, order_by = date),
        r0_growth = (r0 / r0_trailing)-1,
        tests_ma = rollapply(totalTestResultsIncrease, input$mavg_t, mean, align = 'right',fill=NA),
        tests_ma_trailing = dplyr::lag(tests_ma, input$lag_t, order_by=date),
        tests_trend = tests_ma / tests_ma_trailing,
        adj_r0 = r0 / tests_trend,
        adj_r0_growth = (adj_r0 / r0_trailing)-1,
        percent_positive_ma = positiveIncrease_ma / tests_ma,
        active_cases = positive - recovered - death,
        active_cases = ifelse(active_cases <=0 , 100, active_cases),
        active_per_pop = active_cases / population,
        tests_per_pop = tests_ma / population,
        adj_r0 = ifelse(adj_r0 <=0, 0.01, adj_r0)) %>% 
      filter(date == input$calc_date_t)
    
    df_total <- df_reactive %>%
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
    
    data <- df_reactive %>% inner_join(df_total, by = 'date')
    
    return(data)
  })
  
  output$testing <- renderPlot({
    data <- df_reactive_t()
    ggplot(data, aes(x=tests_per_pop)) +
      geom_point(shape=1, show.legend = FALSE, (aes(y=percent_positive_ma, color = state))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1,
                                                         decimal.mark = '.'), limits = c(0,.6)) +
      xlab("Tests per population") +
      ylab(paste0( input$lag_t, " day moving avg positive test rate")) +
      scale_x_continuous(labels = scales::percent_format(accuracy = .01), limits = c(0,.0025)) +
      geom_hline(aes(yintercept = percent_positive_ma_t), color = "black", size = 1) +
      geom_vline(aes(xintercept = tests_per_pop_t), color = "black", size = 1) +
      geom_label(data = data, aes(x=tests_per_pop, y=percent_positive_ma, label = state), size = 5, alpha = .8) + 
      theme(legend.position = "none",
            axis.text = element_text(size = 16),
            axis.title = element_text(size = 16),
            title = element_text(size = 18)) +
      ggtitle(label = "Percent Positive Tests vs. Testing Volume", subtitle = paste0("Percent Positive Tests and Testing Volume on a ", input$mavg_t, "-day moving average period as of ", input$calc_date_t)) 
    
  })
  
})

