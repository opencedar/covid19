#Get data
require(tidyverse)
####HOPKINS DATABASE####

# Get deaths, confirmed, recovered by country, region by date. These are three separate databases 

#------------------------------------------------------------------------------------------------

deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv", stringsAsFactors = FALSE)
deaths_thin <- deaths %>% gather("date", "deaths", 5:ncol(deaths))
deaths_thin$date <- substring(deaths_thin$date, 2)
deaths_thin$date <- lubridate::mdy(deaths_thin$date)

confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv", stringsAsFactors = FALSE)
confirmed_thin <- confirmed %>% gather("date", "confirmed", 5:ncol(confirmed))
confirmed_thin$date <- substring(confirmed_thin$date, 2)
confirmed_thin$date <- lubridate::mdy(confirmed_thin$date)

recovered <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv", stringsAsFactors = FALSE)
recovered_thin <- recovered %>% gather("date", "recovered", 5:ncol(recovered))
recovered_thin$date <- substring(recovered_thin$date, 2)
recovered_thin$date <- lubridate::mdy(recovered_thin$date)

covid_ets <- merge(deaths_thin, confirmed_thin, on= colnames(confirmed_thin)[1:5])
covid_ets <- merge(covid_ets, recovered_thin, on = colnames(covid)[1:5])

state_key <- read.csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv", stringsAsFactors = FALSE)
state <- data.frame(State = state_key$State, Abbreviation = state_key$State)
state_key <- rbind(state_key, state)

x <- covid_ets
x$state_from_county <- str_extract(x$Province.State, '(A[LKSZR])|(C[AOT])|(D[EC])|(F[ML])|(G[AU])|(HI)|(I[DLNA])|(K[SY])|(LA)|(M[EHDAINSOT])|(N[EVHJMYCD])|(MP)|(O[HKR])|(P[WAR])|(RI)|(S[CD])|(T[NX])|(UT)|(V[TIA])|(W[AVIY])')
x <- merge(x, state_key, all.x=TRUE, all.y = FALSE, by.x = 'state_from_county', by.y = "Abbreviation")
x$Province.State[!is.na(x$State)] <- x$State[!is.na(x$State)] 

x <- x %>% select(colnames(covid_ets))

covid_ets_tidy <- x %>% group_by(Province.State, Country.Region, Lat, Long, date) %>% summarise(deaths = sum(deaths), confirmed = sum(confirmed), recovered= sum(recovered))


write.csv(covid_ets, "covid_ets.csv")

write.csv(test, "state_covid.csv")

write.csv(x, "state_covid_2.csv")


