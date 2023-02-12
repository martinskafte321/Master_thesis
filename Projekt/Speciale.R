#Packages
library(lubridate)
library(tidyverse)
library(readxl)
library(tidyquant)

# Read data from CSV
sample = read_excel("data_sample.xlsx")

# Get the correct date format and cut the series by 2018. 
sample <- sample %>%
  mutate(
    aggregate_updated_ts = as_datetime(aggregate_updated_ts),
    aggregate_time_period_start = as_datetime(aggregate_time_period_start),
    aggregate_time_period_end = as_datetime(aggregate_time_period_end)) %>%
  filter(aggregate_time_period_start >= ' 2018-01-01') 

sample <- sample %>% mutate(
  ticker = case_when(
    match_identity == '39cb393d-f3d3-528e-ab66-b22050d84460' ~ "CBA.AX",
    match_identity == 'd5a91177-349d-55ee-a667-4516af4467f4' ~ "WOW.AX",
    match_identity == '279a4527-fb5d-5a0c-838a-dd13e2a23999' ~ "BAP",
    match_identity == 'ea2fdbd0-1172-5d41-a426-4eb38c5f04f5' ~ "H4W.F",
    match_identity == '74003781-f841-5f0c-99c7-98e40d5d3909' ~ "BZLA.F",
    match_identity == 'a67084e0-1e1c-5a07-88cf-32f7c8baa72c' ~ "BBD",
    match_identity == 'a30392de-53ce-5690-b8cf-3d53204bcc37' ~ "BBSEY",
    match_identity == '3b57318d-042f-540e-886a-dea0ddff5e19' ~ "INTR",
    match_identity == '5e53e468-75de-5418-9dd1-f9e6ce829677' ~ "BPAC11.SA",
    match_identity == 'd0b989f4-3148-5e18-a82a-0fdac24d7d15' ~ "ELET3.SA",
    match_identity == '212ed447-7625-5768-9d48-32b1ece96044' ~ "0A0Z.L",
    match_identity == 'a511241e-06f0-506b-85c7-b229cb59f140' ~ "BSBR",
    match_identity == 'a8961660-26c3-5e34-9c72-2f2dd2f829ec' ~ "BCE",
    match_identity == 'ac06fa91-ac21-5b0e-862b-13e6b4ce99d0' ~ "BMO",
    TRUE ~ "other"
  )
)

# Get stock market data
tickers = c("CBA.AX","WOW.AX","BAP","H4W.F","BZLA.F","BBD","BBSEY","INTR","BPAC11.SA","ELET3.SA","BSBR","BCE","BMO")

prices = tq_get(tickers,
                 get = "stock.prices",
                 from = "2018-01-01",
                 to = "2023-01-09"
)

all_returns <- prices %>%
  group_by(symbol) %>%
  mutate(ret = adjusted / lag(adjusted) - 1,
         mkt_cap = volume * close) %>%
  select(symbol, date, ret,close,adjusted,mkt_cap) %>%
  drop_na(ret)

# Join the sentiment data with the stock prices
data <- sample %>%
  left_join(all_returns, by = c("ticker" = "symbol","aggregate_time_period_start" = "date")) %>% na.omit()



# The function: "assign_portfolio" allows a specific variable (in this case momentum) and the number of portfolios to divide a dataset into portfolio sorts: 

assign_portfolio <- function(data, var, n_portfolios) {
  breakpoints <- data %>%
    summarize(breakpoint = quantile({{ var }}, 
                                    #quantile() produces - in this case - 10 quantiles to split the data into, by a sequences from 0 to 1 by the number of portfolios. Thus creating a breakpoint for which we can split the portfolios into. 
                                    probs = seq(0, 1, length.out = n_portfolios + 1),
                                    na.rm = TRUE #Removes all NA's
    )) %>%
    pull(breakpoint) %>%
    as.numeric()
  
  data %>%
    mutate(portfolio = findInterval({{ var }}, #Given a vector of breakpoints, we find the interval containing each element of breakpoints
                                    breakpoints,
                                    all.inside = TRUE 
    )) %>%
    pull(portfolio) #Returns the portfolio number for each security
}



# Calling assign_portfolios to sort stocks into 10 portfolios using the momentum as dependent:
portfolios = data %>% 
  select(aggregate_time_period_start,ticker,sentiment_positive_mean,ret) %>%
  mutate(sentiment_positive_mean = as.numeric(sentiment_positive_mean)) %>%
  group_by(aggregate_time_period_start) %>%
  mutate(
    month = as.Date(aggregate_time_period_start),
    portfolio = assign_portfolio( #calling the function we created earlier to sort stocks into 10 pf's using momentum column in assign_portfolios dataset and add portfolio column to data_for_sorting
      data = cur_data(), #use current set, i.e. assign_portfolios
      var = sentiment_positive_mean,
      n_portfolios = 3
    ),
    portfolio = as.factor(portfolio))


portfolios %>% 
  group_by(portfolio) %>%
  summarise(
    mean = mean(sentiment_positive_mean #arithmetic average of the positive sentiment score
    )
  ) %>% arrange(mean)


strategy_portfolio = portfolios %>% 
  mutate(portfolio = as.numeric(portfolio)) %>%
  group_by(aggregate_time_period_start) %>%
  mutate(breakpoint_low = 1,
         breakpoint_high = 3, # We take the portfolios of highest and lowest momentum as a subset to evaluate by creating breakpoints. 
         portfolio = case_when(portfolio <= breakpoint_low ~ "low",
                               portfolio >= breakpoint_high ~ "high")) %>%
  # The two portfolios are renamed as low and high to distinquish between one another.
  group_by(aggregate_time_period_start,portfolio) %>%
  summarise(ret = mean(ret)) # Value weighted return by high or low grouped momentum values. 

performance <- strategy_portfolio %>%
  pivot_wider(names_from = portfolio, values_from = ret) %>%
  mutate(high_low = high - low) %>% select(aggregate_time_period_start,low,high,high_low)

