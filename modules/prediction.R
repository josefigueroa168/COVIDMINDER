# Prediction code with SARIMA model (R version)
n_days <- 14
covid_TS_counties_long.cases <- read_csv("data/csv/time_series/covid_TS_counties_long.cases.csv")

county.train <- covid_TS_counties_long.cases %>%
  group_by(countyFIPS) %>%
  top_n(n_days, wt=date)

fit <- forecast::auto.arima(y = county.train %>% group_by(countyFIPS))
forecast::forecast(fit, h=7)

fits <- county.train %>%
  group_by(countyFIPS) %>%
  dplyr::select(diff) %>%
  nest() %>%
  mutate(fit = map(data, forecast::auto.arima))

fits <- fits %>%
  mutate(forecast = map(fit, forecast::forecast))