# Stub fake predictions script

# Time series data for nationwide, counties and states
covid_TS_counties_long.cases <- read_csv("data/csv/time_series/covid_TS_counties_long.cases.csv")
covid_TS_state_long.cases <- read_csv("data/csv/time_series/covid_TS_state_long.cases.csv")
covid_TS_US_long.cases <- read_csv("data/csv/time_series/covid_TS_US_long.cases.csv")

# Counties
covid_TS_counties_long.pred <- covid_TS_counties_long.cases %>%
  group_by(countyFIPS) %>%
  top_n(n = 1, wt = date) %>% 
  slice(rep(1:n(), each = 7)) %>%
  mutate(row_num = row_number()) %>%
  mutate(date = date + row_num*86400) %>%
  select(-row_num)

covid_TS_counties_long.cases <- covid_TS_counties_long.cases %>%
  rbind.data.frame(covid_TS_counties_long.pred)

# States
covid_TS_state_long.pred <- covid_TS_state_long.cases %>%
  group_by(State) %>%
  top_n(n = 1, wt = date) %>% 
  slice(rep(1:n(), each = 7)) %>%
  mutate(row_num = row_number()) %>%
  mutate(date = date + row_num*86400) %>%
  select(-row_num)

covid_TS_state_long.cases <- covid_TS_state_long.cases %>%
  rbind.data.frame(covid_TS_state_long.pred)

# National
covid_TS_US_long.pred <- covid_TS_US_long.cases %>%
  top_n(n = 1, wt = date) %>% 
  slice(rep(1:n(), each = 7)) %>%
  mutate(row_num = row_number()) %>%
  mutate(date = date + row_num*86400) %>%
  select(-row_num)

covid_TS_US_long.cases <- covid_TS_US_long.cases %>%
  rbind.data.frame(covid_TS_US_long.pred)

write.csv(covid_TS_counties_long.cases, "data/csv/time_series/covid_TS_counties_long.cases.csv")
write.csv(covid_TS_state_long.cases, "data/csv/time_series/covid_TS_state_long.cases.csv")
write.csv(covid_TS_US_long.cases, "data/csv/time_series/covid_TS_US_long.cases.csv")
