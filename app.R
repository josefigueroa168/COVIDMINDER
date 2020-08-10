library(shiny)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Auto.Arima Predictions!!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  h3("Number of Days to Predict"),
                  min = 1,
                  max = 10,
                  value = 5),
      
      selectInput("select1", h3("Variable to Predict"), 
                  choices = list("cases", "deaths",
                                 "p_cases", 
                                 "p_deaths", "diff", 
                                 "p_diff", "d_diff", 
                                 "p.d_diff"), selected = "cases"),
      
      selectInput("select2", h3("State"), 
                  choices = list("AL", "AK", "AZ", "AR", "CA", 
                                 "CO", "CT", "DE", "DC", "FL",
                                 "GA", "HI", "ID", "IL", "IN", 
                                 "IA", "KS", "KY", "LA", "ME",
                                 "MD", "MA", "MI", "MN", "MS", 
                                 "MO", "MT", "NE", "NV", "NH",
                                 "NJ", "NM", "NY", "NC", "ND", 
                                 "OH", "OK", "OR", "PA", "RI",
                                 "SC", "SD", "TN", "TX", "UT", 
                                 "VT", "VA", "WA", "WV", "WI", 
                                 "WY"
                  ), selected = "AL"),
    
      # Input: Slider for top % counties to show ----
      sliderInput(inputId = "filter_c",
                  h3("What percent of counties to show"),
                  min = 0,
                  max = 100,
                  value = 100)
      
    ),
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      plotOutput("ts_plot", height = "1000px"),
      
      tableOutput("current_counties")
      #plotOutput("state_plot", height = "1000px")
      
    )
  )
)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


server <- function(input, output, session) {
  
  
  output$ts_plot <- renderPlot({
    
    library(forecast)
    library(dplyr)
    library(tidyverse)
    
    n_days = input$bins
    state = input$select2
    col = input$select1
    filter_c = input$filter_c
    
    
    if (filter_c == 0) {
      ts.plot()
    }
    
    
    covid_TS_counties_long.pred <- read_csv("data/csv/time_series/covid_TS_counties_long.pred.csv")

    data <- covid_TS_counties_long.pred
    data <- data[which(data$State == state),] 
    s_fip <- data$stateFIPS[1]
    #data[5] <- as.character(data[5])
    dates = (unique(data$date))
    counties <- unique(data$County)
    c_fips <- unique(data$countyFIPS)
    future_dates <- seq(as.Date(dates[length(dates)-1]),by='days',length=n_days)
    char_dates <- c(as.character(dates[1:length(dates)]))
      
    m <- matrix(0, ncol = length(dates) + 1, nrow = length(counties))
    colnames(m) <- c("County", char_dates)
    dates_data <- as_tibble(m)
    dates_data$County = counties
    dates_data <- column_to_rownames(dates_data, var = "County")
    
    
    for (i in 1:length(counties)) {
      county = counties[i]
      count_dat <- data %>%
        filter(County == county)
      i_dat <- count_dat %>%
        pull(col)
      date_dat <- count_dat %>%
        pull(date)
      for (j in 1:length(date_dat) ) {
        c_date = as.character(date_dat[j])
        date= date_dat[j]
        if (nrow(count_dat[which(count_dat$date == date),]) != 0) {
          dates_data[[c_date]][i] = count_dat[which(count_dat$date == date),][[col]]
        }
      }
    }
    
    t_dates_data <- as_tibble(t(dates_data))
    
    t_dates_data <- t_dates_data %>%
      replace(is.na(.), 0) %>%
      replace(. == Inf, 0) %>%
      replace(. == -Inf, 0)
    
    frames = matrix(nrow = n_days, ncol=length(colnames(t_dates_data)))
    
    for ( i in 1:length(colnames(t_dates_data)) ) {
      
      county = colnames(t_dates_data)[i]
      
      fit <- auto.arima(ts(t_dates_data[i]), 
                        max.p = 5,
                        max.q = 5,
                        max.d = 3)
      
      frame <- forecast(fit, n_days)
      frames[,i] <- frame$mean
    }
    sz <- as.integer( length(counties) * (filter_c / 100) )
    if (sz == 0 ) {
      ts.plot()
    }
    
    # save frames as a session variable to access when making the table below
    session$userData$fin_frames <- frames
    ##
    
    # Reorder these values from high to low in case we need to filter down according to sz
    frames <- t(frames)
    counties <- counties[order(rowMeans(frames), decreasing = T)]
    frames <- frames[order(rowMeans(frames), decreasing = T), ]
    frames <- t(frames)
    ####
    
    
    
    ts.plot(frames[,1:sz], col=1:sz,
            gpars=list(xlab="Days", ylab= col, lty=c(1:3)))      # needs a legend
    legend('topright', legend=counties[1:sz], lty=1, col=1:sz)
    
  }, height = 1000)
  
  
  
  output$current_counties  <- renderTable({
    
    col = input$select1 # this is here just so the table will update to new data
    n_days = input$bins
    state = input$select2
    filter_c = input$filter_c
    
    covid_TS_counties_long.pred <- read_csv("data/csv/time_series/covid_TS_counties_long.pred.csv")
    
    data <- covid_TS_counties_long.pred
    data <- data[which(data$State == state),]
    dates = (unique(data$date))
    counties <- unique(data$County)
    sz <- as.integer( length(counties) * (filter_c / 100) )
    
    future_dates <- seq(as.Date(dates[length(dates)-1]),by='days',length=n_days)
    frames <- t(session$userData$fin_frames)
    counties <- counties[order(rowMeans(frames), decreasing = T)]
    frames <- frames[order(rowMeans(frames), decreasing = T), ]
    
    ret <- as.data.table(frames[1:sz,])
    colnames(ret) <- as.character(future_dates)
    ret <- ret %>%
      mutate(County = counties[1:sz]) %>%
      dplyr::select(County, everything())
    
    
    ret
    
  })
  
  
  
  # STATE PLOT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$state_plot <- renderPlot({
    
    library(forecast)
    library(dplyr)
    library(tidyverse)
    
    n_days = input$bins
    col = input$select1
    covid_TS_state_long.cases <- read_csv("data/csv/time_series/covid_TS_state_long.cases.csv")
    
    data <- covid_TS_state_long.cases
    states = (unique(data$State))
    dates = (unique(data$date))
    future_dates <- seq(as.Date(dates[length(dates)-1]),by='days',length=n_days)
    char_dates <- c(as.character(dates[1:length(dates)]))
    
    m <- matrix(0, ncol = length(dates) + 1, nrow = length(states))
    colnames(m) <- c("State", char_dates)
    dates_data <- as_tibble(m)
    dates_data$State = states
    dates_data <- column_to_rownames(dates_data, var = "State")
    
    
    for (i in 1:length(states)) {
      state = states[i]
      state_dat <- data %>%
        filter(State == state)
      i_dat <- state_dat %>%
        pull(col)
      date_dat <- state_dat %>%
        pull(date)
      for (j in 1:length(date_dat) ) {
        c_date = as.character(date_dat[j])
        date= date_dat[j]
        if (nrow(state_dat[which(state_dat$date == date),]) != 0) {
          dates_data[[c_date]][i] = state_dat[which(state_dat$date == date),][[col]]
        }
      }
    }
    
    t_dates_data <- as_tibble(t(dates_data))
    
    t_dates_data <- t_dates_data %>%
      replace(is.na(.), 0) %>%
      replace(. == Inf, 0) %>%
      replace(. == -Inf, 0)
    
    frames = matrix(nrow = n_days, ncol=length(colnames(t_dates_data)))
    
    for ( i in 1:length(colnames(t_dates_data)) ) {
      
      state = colnames(t_dates_data)[i]
      
      fit <- auto.arima(ts(t_dates_data[i]), 
                        max.p = 5,
                        max.q = 5,
                        max.d = 3)
      
      frame <- forecast(fit, n_days)
      frames[,i] <- frame$mean
    }
    
    
    sz <- length(states) + 1
    
    ts.plot(frames, col=2:sz)      # needs a legend
    legend('topright', legend=states, lty=1, col=2:sz)
    
  }, height = 1000)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


shinyApp(ui, server)
