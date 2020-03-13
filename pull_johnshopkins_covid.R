library(tidyverse)
library(anytime)
library(janitor)

jh_melt_format <-
    function(dat) {
        dat %>%
            pivot_longer(
                cols = c(-`Province/State`, -`Country/Region`, -`Lat`, -`Long`),
                names_to = "obs_date",
                values_to = "cases"
            ) %>%
            mutate(obs_date = as.Date(obs_date, format = "%m/%d/%y"))
    }


confirmed <-
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv" %>%
    url() %>%
    read_csv() %>%
    jh_melt_format() %>%
    filter(cases > 0)

deaths <-
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv" %>%
    url() %>%
    read_csv() %>%
    jh_melt_format() %>%
    filter(cases > 0)

recovered <-
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv" %>%
    url() %>%
    read_csv() %>%
    jh_melt_format() %>%
    filter(cases > 0)


confirmed %>%
    filter(`Province/State` == "Colorado")

deaths %>%
    filter(`Province/State` == "Colorado")

recovered %>%
    filter(`Province/State` == "Colorado")

