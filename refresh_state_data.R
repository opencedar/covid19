
state_historical_testing_df <- read.csv("https://covidtracking.com/api/v1/states/daily.csv", stringsAsFactors = FALSE) 


saveRDS(state_historical_testing_df, "state_historical_testing_df.Rds")




