require(tidyverse)

covid_graph <- covid_ets_tidy %>% gather(metric, cases, confirmed:recovered)
covid_graph_region <- covid_graph %>% filter( Province.State == "Maryland")

ggplot(covid_graph_region, aes(x=date, y=cases, color = metric)) +
  geom_line() +
  theme_minimal()
