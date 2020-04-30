
library(tidyverse)
library(zoo)
library(scales)
library(DT)



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

active_per_pop = seq(100, 200000, by = 100)
r0 = 1000 / active_per_pop

point_one_percent = data.frame(active_per_pop, r0)

active_per_pop = seq(100,200000, by = 100)
r0 = 10000 / active_per_pop

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
        active_per_pop = (active_cases / population)*1000000,
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
        tests_per_pop_t = (tests_ma_t / population_t)* 1000000,
        active_per_pop_t = (active_cases_t / population_t) *1000000,
        active_per_pop_t = ifelse(active_per_pop_t <=0, 10, active_per_pop_t),
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
      scale_x_continuous(labels = scales::comma_format(), limits = c(20, 25000), trans = 'log10') +
      geom_hline(aes(yintercept = adj_r0_t), color = "black", size = 1) +
      geom_vline(aes(xintercept = active_per_pop_t), color = "black", size = 1) +
      geom_label(data = data, aes(x=active_per_pop, y=adj_r0, label = state), size = 5, alpha = 0.7) + 
      theme(legend.position = "none",
            axis.text = element_text(size = 16),
            axis.title = element_text(size = 16),
            title = element_text(size = 18)) +
      labs(y = paste0("R0 over ", input$lag, " days handicapped for testing performance"), x = "Active cases per million population (log)") +
      geom_text(aes(x = 100, y =2.5, label = paste0("Green: Based on current trajectory\n expect additional \n 0.1% of population infected \nin ", input$lag, " days")), size = 5, color = "darkgreen") +
      geom_text(aes(x = 15000, y =2.5, label = paste0("Red: Based on current trajectory\n expect additional \n 1.0% of population infected \nin ", input$lag, " days")), size = 5, color = "darkred") +
      geom_label(aes(x = 25, y = adj_r0_t, label = paste0("National R: ", round(adj_r0_t,2))), size = 5, alpha = 0.7) + 
      
      geom_label(data = data, aes(x = active_per_pop_t, y = 3, label = paste0("National Active Per Million: ", scales::comma(active_per_pop_t))), size = 5, alpha = 0.7) 
    
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
        active_per_pop = (active_cases / population) *1000000,
        tests_per_pop = (tests_ma / population)*1000000,
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
        tests_per_pop_t = (tests_ma_t / population_t) *1000000,
        active_per_pop_t =(active_cases_t / population_t)*1000000,
        active_per_pop_t = ifelse(active_per_pop_t <=0, 1, active_per_pop_t),
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
      xlab(paste0("Tests per Million, Past ", input$lag_t, " Days")) +
      ylab(paste0( input$lag_t, " day moving avg positive test rate")) +
      scale_x_continuous(labels = scales::comma_format(), limits = c(0,2500)) +
      geom_hline(aes(yintercept = percent_positive_ma_t), color = "black", size = 1) +
      geom_vline(aes(xintercept = tests_per_pop_t), color = "black", size = 1) +
      geom_label(data = data, aes(x=tests_per_pop, y=percent_positive_ma, label = state), size = 5, alpha = .5) + 
      theme(legend.position = "none",
            axis.text = element_text(size = 16),
            axis.title = element_text(size = 16),
            title = element_text(size = 18)) +
      geom_label(aes(x = 2200, 
                     y = percent_positive_ma_t + .03, 
                     label = paste0("National % Positive: ", 
                                    percent(percent_positive_ma_t,
                                    .1))),
                      size = 5, 
                      alpha = .5) + 
      geom_label(aes(x = tests_per_pop_t,
                     y = .60,
                     label = paste0("National Tests Per Million: ",
                                    comma(tests_per_pop_t))),
                     size = 5, 
                     alpha = .5)  +
      ggtitle(label = "Percent Positive Tests vs. Tests Per Million", 
              subtitle = paste0("Percent Positive Tests and Testing Volume on a ", 
                                input$mavg_t, 
                                "-day moving average period as of ", 
                                input$calc_date_t)
              ) 
    
  })
  
  # df_reactive_d <- reactive({ 
  #   df_reactive <- state_historical_testing_df %>%
  #     group_by(state) %>% 
  #     arrange(date) %>%
  #     mutate(
  #       positiveIncrease_ma = rollapply(positiveIncrease,input$mavg_d ,mean,align='right',fill=NA),
  #       positiveIncrease_ma_l = dplyr::lag(positiveIncrease_ma, input$lag_d, order_by = date),
  #       r0 = positiveIncrease_ma / positiveIncrease_ma_l,
  #       r0_trailing = dplyr::lag(r0, input$lag_d, order_by = date),
  #       r0_growth = (r0 / r0_trailing)-1,
  #       tests_ma = rollapply(totalTestResultsIncrease, input$mavg_d, mean, align = 'right',fill=NA),
  #       tests_ma_trailing = dplyr::lag(tests_ma, input$lag_d, order_by=date),
  #       tests_trend = tests_ma / tests_ma_trailing,
  #       adj_r0 = r0 / tests_trend,
  #       adj_r0_growth = (adj_r0 / r0_trailing)-1,
  #       percent_positive_ma = positiveIncrease_ma / tests_ma,
  #       active_cases = positive - recovered - death,
  #       active_cases = ifelse(active_cases <=0 , 100, active_cases),
  #       active_per_pop = active_cases / population,
  #       tests_per_pop = tests_ma / population,
  #       adj_r0 = ifelse(adj_r0 <=0, 0.01, adj_r0)) %>% 
  #     filter(date == input$calc_date_d)
  #   
  #   
  #   return(data)
  # })
  
  output$table <- DT::renderDT(
    as.data.frame(
      state_historical_testing_df %>%
        group_by(state) %>% 
        arrange(date) %>%
        mutate(
          positiveIncrease_ma = rollapply(positiveIncrease,input$mavg_d ,mean,align='right',fill=NA),
          positiveIncrease_ma_l = dplyr::lag(positiveIncrease_ma, input$lag_d, order_by = date),
          r0 = positiveIncrease_ma / positiveIncrease_ma_l,
          r0_trailing = dplyr::lag(r0, input$lag_d, order_by = date),
          r0_growth = (r0 / r0_trailing)-1,
          tests_ma = rollapply(totalTestResultsIncrease, input$mavg_d, mean, align = 'right',fill=NA),
          tests_ma_trailing = dplyr::lag(tests_ma, input$lag_d, order_by=date),
          tests_trend = tests_ma / tests_ma_trailing,
          adj_r0 = r0 / tests_trend,
          adj_r0_growth = (adj_r0 / r0_trailing)-1,
          percent_positive = positiveIncrease_ma / tests_ma,
          active_cases = positive - recovered - death,
          active_cases = ifelse(active_cases <=0 , 100, active_cases),
          active_per_pop = (active_cases / population)*1000000,
          tests_per_pop = (tests_ma / population)*1000000,
          adj_r0 = ifelse(adj_r0 <=0, 0.01, adj_r0),
          reopen_score_r = round(adj_r0 * active_per_pop * 1000, 4),
          reopen_score_test = percent_positive / tests_per_pop,
          active_per_pop = round(active_per_pop, 0),
          tests_per_pop = round(tests_per_pop, 0),
          adj_r = round(adj_r0, 2),
          percent_tests_positive  = round(percent_positive, 4)) %>%
        filter(date == input$calc_date_d) %>%
        ungroup() %>%
        arrange(reopen_score_r) %>% 
        mutate(reopen_score_r_rank = row_number(reopen_score_r)) %>%
        arrange(reopen_score_test) %>%
        mutate(reopen_score_test_rank = row_number(reopen_score_test))%>%
        mutate(composite_rank = (reopen_score_test_rank + reopen_score_r_rank)/2,
               composite_rank = row_number(composite_rank)
               ) %>%
        arrange(composite_rank) %>%
        select(state, composite_rank, reopen_score_r_rank,  adj_r, active_per_pop, reopen_score_test_rank, tests_per_pop, percent_tests_positive),
      options = list(
        pageLength = 20)
    )
  )
  
  
})

