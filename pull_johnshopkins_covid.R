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
            mutate(obs_date = as.Date(obs_date, format = "%m/%d/%y")) %>%
            clean_names()
    }

new_cases <-
    function(c) {
        pmax(c - if_else(is.na(lag(c)), 0 , lag(c)), 0)
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



(
    confirmed_colo <-
        confirmed %>%
        filter(province_state == "Colorado") %>%
        mutate(new_cases = new_cases(cases),
               category = "Cases")
)

(
    deaths_colo <-
        deaths %>%
        filter(province_state == "Colorado") %>%
        mutate(new_cases = new_cases(cases),
               category = "Deaths")
)

(
    recovered_colo <- recovered %>%
        filter(province_state == "Colorado") %>%
        mutate(new_cases = new_cases(cases),
               category = "Recoveries")
)

list(confirmed_colo,
     deaths_colo,
     recovered_colo) %>%
    bind_rows() %>%
    ggplot() +
    geom_bar(
        aes(x = obs_date,
            y = new_cases,
            fill = category),
        stat = "identity",
        alpha = .5,
        position = position_dodge(width = 0)
    )

