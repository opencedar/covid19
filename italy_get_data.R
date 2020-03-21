require(tidyverse)

italy <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv")


headers <- c("datetime", "country", "recovering_w_symptoms", "intensive_therapy", "total_hospitalized", "house_isolation", 
             "total_positive", "new_positive", "reccovered", "dead", "total_cases", "tested")
colnames(italy) <- headers
italy$datetime <- lubridate::ymd_hms(italy$datetime)


#italy_province <- jsonlite::fromJSON("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-province.json")
