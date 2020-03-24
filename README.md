# covid19

A repository to explore COVID-19 data using econometric time-series (ETS) analysis.

The upstream source data are hosted at the 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE; https://github.com/CSSEGISandData/COVID-19

I can't commit to updating the core time-series data set daily (`covid_ets.csv`) but the data frame can be assembled using `getdata.R` or the `country_ts_panel.Rmd` Rmarkdown document, which currently produces "sparkline" type visualizations for States and Countries.
