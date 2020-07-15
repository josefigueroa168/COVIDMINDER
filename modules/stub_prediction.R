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
  mutate(cases = cases + row_num*diff) %>%
  mutate(deaths = deaths + row_num*d_diff) %>%
  mutate(p_cases = p_cases + row_num*p_diff) %>%
  mutate(p_deaths = p_deaths + row_num*p.d_diff) %>%
  select(-row_num)

# States
covid_TS_state_long.pred <- covid_TS_state_long.cases %>%
  group_by(State) %>%
  top_n(n = 1, wt = date) %>% 
  slice(rep(1:n(), each = 7)) %>%
  mutate(row_num = row_number()) %>%
  mutate(date = date + row_num*86400) %>%
  mutate(cases = cases + row_num*diff) %>%
  mutate(deaths = deaths + row_num*d_diff) %>%
  mutate(p_cases = p_cases + row_num*p_diff) %>%
  mutate(p_deaths = p_deaths + row_num*p.d_diff) %>%
  select(-row_num)

# National
covid_TS_US_long.pred <- covid_TS_US_long.cases %>%
  top_n(n = 1, wt = date) %>% 
  slice(rep(1:n(), each = 7)) %>%
  mutate(row_num = row_number()) %>%
  mutate(date = date + row_num*86400) %>%
  mutate(cases = cases + row_num*diff) %>%
  mutate(deaths = deaths + row_num*d_diff) %>%
  mutate(p_cases = p_cases + row_num*p_diff) %>%
  mutate(p_deaths = p_deaths + row_num*p.d_diff) %>%
  select(-row_num)

write_csv(covid_TS_counties_long.pred, "data/csv/time_series/covid_TS_counties_long.pred.csv")
write_csv(covid_TS_state_long.pred, "data/csv/time_series/covid_TS_state_long.pred.csv")
write_csv(covid_TS_US_long.pred, "data/csv/time_series/covid_TS_US_long.pred.csv")
