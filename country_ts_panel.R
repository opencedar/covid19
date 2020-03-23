covid_country_ets <- covid_ets  %>% 
  group_by(Country.Region, date) %>% 
  summarise(confirmed = sum(confirmed), deaths = sum(deaths), recovered= sum(recovered)) %>% 
  mutate(
    #create daily percent growth variables
    confirmed_growth_rate = c(NA,diff(confirmed))/lag(confirmed, 1),
    death_growth_rate = c(NA,diff(deaths))/lag(deaths, 1),
    recovered_growth_rate = c(NA,diff(confirmed))/lag(confirmed, 1)
  ) %>%
  ungroup()

#replace NaNs w 0s
covid_country_ets$confirmed_growth_rate[is.nan(covid_country_ets$confirmed_growth_rate)] <- 0
covid_country_ets$death_growth_rate[is.nan(covid_country_ets$death_growth_rate)] <- 0
covid_country_ets$recovered_growth_rate[is.nan(covid_country_ets$recovered_growth_rate)] <- 0

#Only get countries with >1000 cases today
current_cases <- covid_country_ets %>% group_by(Country.Region) %>% summarise(current_cases = max(confirmed))
five_hundred <- current_cases$Country.Region[current_cases$current_cases >= 1000]

covid_country_ets <- covid_country_ets %>% filter(Country.Region %in% five_hundred)

#Create a panel data set
library(plm)
covid_plm <- pdata.frame(covid_country_ets, index=c("Country.Region", "date"))
#create diff variables
covid_plm$diff_confirmed <- diff(covid_plm$confirmed, 1)
covid_plm$diff_deaths <- diff(covid_plm$deaths, 1)
covid_plm$diff_recovered <- diff(covid_plm$recovered, 1)

#######


ggplot(covid_plm, aes(x=date, y=confirmed, group = Country.Region)) +
  geom_line() +
  facet_wrap(~ Country.Region) +
  ggtitle("Confirmed COVID19 Cases Through 3-23-20") +
  theme_void()

ggplot(covid_plm, aes(x=date, y=deaths, group = Country.Region)) +
  geom_line() +
  facet_wrap(~ Country.Region) +
  ggtitle("Cumulative COVID19 Deaths Through 3-23-20") +
  theme_void()

ggplot(covid_plm, aes(x=date, y=recovered, group = Country.Region)) +
  geom_line() +
  facet_wrap(~ Country.Region) +
  ggtitle("Cumulative COVID19 Recoveries Through 3-23-20") +
  theme_void()

ggplot(covid_plm, aes(x=date, y=recovered, group = Country.Region)) +
  geom_line() +
  facet_wrap(~ Country.Region) +
  ggtitle("Cumulative COVID19 Recoveries Through 3-23-20") +
  theme_void()









