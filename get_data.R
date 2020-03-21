#Get data
require(tidyverse)
####HOPKINS DATABASE####

# Get deaths, confirmed, recovered by country, region by date. These are three separate databases 

#------------------------------------------------------------------------------------------------

deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
deaths_thin <- deaths %>% gather("date", "deaths", 5:ncol(deaths))
deaths_thin$date <- substring(deaths_thin$date, 2)
deaths_thin$date <- lubridate::mdy(deaths_thin$date)

confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
confirmed_thin <- deaths %>% gather("date", "confirmed", 5:ncol(confirmed))
confirmed_thin$date <- substring(confirmed_thin$date, 2)
confirmed_thin$date <- lubridate::mdy(confirmed_thin$date)

recovered <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
recovered_thin <- deaths %>% gather("date", "recovered", 5:ncol(recovered))
recovered_thin$date <- substring(recovered_thin$date, 2)
recovered_thin$date <- lubridate::mdy(recovered_thin$date)

covid_ets <- merge(deaths_thin, confirmed_thin, on= colnames(confirmed)[1:5])
covid_ets <- merge(covid, recovered_thin, on = colnamse(covid)[1:5])

write.csv(covid_ets, "covid_ets.csv")


