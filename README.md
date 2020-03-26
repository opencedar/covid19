# covid19

A repository to explore COVID-19 data using econometric time-series (ETS) analysis.

The upstream source data are hosted at the 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE; https://github.com/CSSEGISandData/COVID-19

The data munging pulls down the Hopkins daily data sets and creates a nicely-formatted time series data set.

The Rmarkdown document (`country_ts_panel.Rmd`) knits a tabular and visual summary of the current state of COVID19 worldwide, with a specific drill-through on the United States, state-by-state. In particular, two models are estimated:

* A log-log model of lag-7-new-cases on new cases (elasticity) is estimated for each country and state
* A meta-model estimating this elasticity by log(days_since_50_cases) to predict future exponential growth.

## Future Enhancements

* Extending the two models to predict future new cases by country and state

I can't commit to updating the core time-series data set daily (`covid_ets.csv`) but the data frame can be assembled using `getdata.R` or the `country_ts_panel.Rmd` Rmarkdown document, which currently produces "sparkline" type visualizations for States and Countries.
