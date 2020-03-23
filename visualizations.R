require(tidyverse)

covid_graph <- covid_ets_tidy %>% gather(metric, cases, recovered:confirmed)
covid_graph_region <- covid_graph %>% filter(Country.Region == "US", Province.State == "Maryland")

ggplot(covid_graph_region, aes(x=date, y=cases)) +
  geom_line(aes(color = metric), size =1) +
  theme_minimal()
