#Packages
library(tidyverse)
library(lubridate)
library(readxl)
library(timetk)
library(dplyr)
library(zoo)

# Set the path to the desired data location
setwd("C:/Users/Marti/OneDrive - University of Copenhagen/KU/Speciale/Data behandling")



###############################################################
############# Get stock market data ###########################
###############################################################

# Read stock data from:
ISIN <- read_excel("ISIN numbers.xlsx")
stock <- read_excel("SXXP_stock_data.xlsx")

# Clean up the stock market data: Remove the two first rows 
stocks <- stock %>%
  filter(!row_number() %in% c(1,2)) %>% 
  # Make the data set a long matrix instead of wide
  pivot_longer(!DATES, names_to = "firm",values_to = "value")  %>% 
  mutate(
    type = str_split_fixed(firm, "_", 2)[,2],
    ID = str_split_fixed(firm, "_", 2)[,1],
    date = as.Date(as.numeric(DATES), origin = "1899-12-30"),
    value = as.numeric(value)
    ) %>% 
  select(-DATES,-firm) %>%
  relocate(type, .after = ID) %>%
  relocate(date, .before = ID) %>%
  # And join the stock information with the ISIN number:
  left_join(ISIN, by = "ID") %>% rename("isin" = id_isin) %>%
  select(-ID) %>%
  relocate(value, .after = isin)


# Summarize the stocks to index by date and isin
stocks_weekly <- stocks %>%
  pivot_wider(
    names_from = type,
    values_from = value
  )


# Write to CSV to keep in nice format:
 # stocks_weekly %>%
 #     write.csv(file = "clean_stock_data_weekly.csv")


###############################################################
############# Get Sentiment data ##############################
###############################################################

# Read Sentiment data from CSV
sentiment <- read.csv("SXXP_sentiment.csv", sep = ",") %>%
# Get the correct date format and cut the series by 2018. 
  mutate(
    aggregate_time_period_start = as_datetime(aggregate_time_period_start)) %>%
  filter(aggregate_time_period_start >= ' 2018-01-01') %>%
  rename("date" = aggregate_time_period_start) %>%
  select(-c(aggregate_type,aggregate_key,aggregate_updated_ts,aggregate_time_period_end,day,week,month,year,match_identity)) 

# Remove unnecessary items:
remove(stock,ISIN)

# Compute the last business day of the week, then sum the amount of observations for that week
# Summarize sentiment data into weekly observations to match the stock market data: countable items are summed, averages are averaged (runtime: 1 min)
sentiment_weekly <- sentiment %>%
  mutate(date = ceiling_date(date, "week", week_start = getOption("lubridate.week.start", 5))) %>%
  group_by(date,isin) %>%
  summarise(
    observations = sum(observations),
    observations_unique_sentences = sum(observations_unique_sentences),
    observations_unique_articles  = sum(observations_unique_articles),
    observations_unique_sources   = sum(observations_unique_sources ),
    global_unique_sentences   = sum(global_unique_sentences ),
    global_unique_articles  = sum(global_unique_articles),
    global_unique_sources   = sum(global_unique_sources ),
    sentiment_negative_count   = sum(sentiment_negative_count ),
    sentiment_neutral_count   = sum(observations_unique_articles),
    sentiment_positive_count   = sum(sentiment_positive_count ),
    
    sentiment_negative_mean   = mean(sentiment_negative_mean ),
    sentiment_neutral_mean   = mean(sentiment_neutral_mean ),
    sentiment_positive_mean   = mean(sentiment_positive_mean ),
    # sentiment_negative_max   = mean(sentiment_negative_max ),
    # sentiment_neutral_max   = mean(sentiment_neutral_max ),
    # sentiment_positive_max   = mean(sentiment_positive_max ),
    sentiment_negative_weighted_mean   = mean(sentiment_negative_weighted_mean ),
    sentiment_positive_weighted_mean   = mean(sentiment_positive_weighted_mean ),
    sentiment_negative_weighted_max   = mean(sentiment_negative_weighted_max ),
    sentiment_positive_weighted_max   = mean(sentiment_positive_weighted_max ),
    
    sdg_1_negative_count = sum(sdg_1_negative_count),
    sdg_1_neutral_count = sum(sdg_1_neutral_count),
    sdg_1_positive_count = sum(sdg_1_positive_count),
    sdg_2_negative_count = sum(sdg_2_negative_count),
    sdg_2_neutral_count = sum(sdg_2_neutral_count),
    sdg_2_positive_count = sum(sdg_2_positive_count),
    sdg_3_negative_count = sum(sdg_3_negative_count),
    sdg_3_neutral_count = sum(sdg_3_neutral_count),
    sdg_3_positive_count = sum(sdg_3_positive_count),
    sdg_4_negative_count = sum(sdg_4_negative_count),
    sdg_4_neutral_count = sum(sdg_4_neutral_count),
    sdg_4_positive_count = sum(sdg_4_positive_count),
    sdg_5_negative_count = sum(sdg_5_negative_count),
    sdg_5_neutral_count = sum(sdg_5_neutral_count),
    sdg_5_positive_count = sum(sdg_5_positive_count),
    sdg_6_negative_count = sum(sdg_6_negative_count),
    sdg_6_neutral_count = sum(sdg_6_neutral_count),
    sdg_6_positive_count = sum(sdg_6_positive_count),
    sdg_7_negative_count = sum(sdg_7_negative_count),
    sdg_7_neutral_count = sum(sdg_7_neutral_count),
    sdg_7_positive_count = sum(sdg_7_positive_count),
    sdg_8_negative_count = sum(sdg_8_negative_count),
    sdg_8_neutral_count = sum(sdg_8_neutral_count),
    sdg_8_positive_count = sum(sdg_8_positive_count),
    sdg_9_negative_count = sum(sdg_9_negative_count),
    sdg_9_neutral_count = sum(sdg_9_neutral_count),
    sdg_9_positive_count = sum(sdg_9_positive_count),
    sdg_10_negative_count = sum(sdg_10_negative_count),
    sdg_10_neutral_count = sum(sdg_10_neutral_count),
    sdg_10_positive_count = sum(sdg_10_positive_count),
    sdg_11_negative_count = sum(sdg_11_negative_count),
    sdg_11_neutral_count = sum(sdg_11_neutral_count),
    sdg_11_positive_count = sum(sdg_11_positive_count),
    sdg_12_negative_count = sum(sdg_12_negative_count),
    sdg_12_neutral_count = sum(sdg_12_neutral_count),
    sdg_12_positive_count = sum(sdg_12_positive_count),
    sdg_13_negative_count = sum(sdg_13_negative_count),
    sdg_13_neutral_count = sum(sdg_13_neutral_count),
    sdg_13_positive_count = sum(sdg_13_positive_count),
    sdg_14_negative_count = sum(sdg_14_negative_count),
    sdg_14_neutral_count = sum(sdg_14_neutral_count),
    sdg_14_positive_count = sum(sdg_14_positive_count),
    sdg_15_negative_count = sum(sdg_15_negative_count),
    sdg_15_neutral_count = sum(sdg_15_neutral_count),
    sdg_15_positive_count = sum(sdg_15_positive_count),
    sdg_broad_negative_count  = sum(sdg_broad_negative_count),
    sdg_broad_neutral_count   = sum(sdg_broad_neutral_count),
    sdg_broad_positive_count = sum(sdg_broad_positive_count)
  
  )
  
# # Summarize sentiment data into weekly observations to match the stock market data: countable items are summed, averages are averaged (runtime: 1 min)
# sentiment_week <- sentiment %>% select(date,isin,observations) %>%
#   mutate(EoW = ceiling_date(date, "week", week_start = getOption("lubridate.week.start", 5))) %>%
#   group_by(EoW,isin) %>%
#   summarise_by_time(
#     .date_var = EoW,
#     .by        = "week",
#     .type = "ceiling",
#     observations = sum(observations),
#     observations_unique_sentences = sum(observations_unique_sentences),
#     observations_unique_articles  = sum(observations_unique_articles),
#     observations_unique_sources   = sum(observations_unique_sources ),
#     global_unique_sentences   = sum(global_unique_sentences ),
#     global_unique_articles  = sum(global_unique_articles),
#     global_unique_sources   = sum(global_unique_sources ),
#     sentiment_negative_count   = sum(sentiment_negative_count ),
#     sentiment_neutral_count   = sum(observations_unique_articles),
#     sentiment_positive_count   = sum(sentiment_positive_count ),
#     
#     sentiment_negative_mean   = mean(sentiment_negative_mean ),
#     sentiment_neutral_mean   = mean(sentiment_neutral_mean ),
#     sentiment_positive_mean   = mean(sentiment_positive_mean ),
#     # sentiment_negative_max   = mean(sentiment_negative_max ),
#     # sentiment_neutral_max   = mean(sentiment_neutral_max ),
#     # sentiment_positive_max   = mean(sentiment_positive_max ),
#     sentiment_negative_weighted_mean   = mean(sentiment_negative_weighted_mean ),
#     sentiment_positive_weighted_mean   = mean(sentiment_positive_weighted_mean ),
#     sentiment_negative_weighted_max   = mean(sentiment_negative_weighted_max ),
#     sentiment_positive_weighted_max   = mean(sentiment_positive_weighted_max ),
#     
#     sdg_1_negative_count = sum(sdg_1_negative_count),
#     sdg_1_neutral_count = sum(sdg_1_neutral_count),
#     sdg_1_positive_count = sum(sdg_1_positive_count),
#     sdg_2_negative_count = sum(sdg_2_negative_count),
#     sdg_2_neutral_count = sum(sdg_2_neutral_count),
#     sdg_2_positive_count = sum(sdg_2_positive_count),
#     sdg_3_negative_count = sum(sdg_3_negative_count),
#     sdg_3_neutral_count = sum(sdg_3_neutral_count),
#     sdg_3_positive_count = sum(sdg_3_positive_count),
#     sdg_4_negative_count = sum(sdg_4_negative_count),
#     sdg_4_neutral_count = sum(sdg_4_neutral_count),
#     sdg_4_positive_count = sum(sdg_4_positive_count),
#     sdg_5_negative_count = sum(sdg_5_negative_count),
#     sdg_5_neutral_count = sum(sdg_5_neutral_count),
#     sdg_5_positive_count = sum(sdg_5_positive_count),
#     sdg_6_negative_count = sum(sdg_6_negative_count),
#     sdg_6_neutral_count = sum(sdg_6_neutral_count),
#     sdg_6_positive_count = sum(sdg_6_positive_count),
#     sdg_7_negative_count = sum(sdg_7_negative_count),
#     sdg_7_neutral_count = sum(sdg_7_neutral_count),
#     sdg_7_positive_count = sum(sdg_7_positive_count),
#     sdg_8_negative_count = sum(sdg_8_negative_count),
#     sdg_8_neutral_count = sum(sdg_8_neutral_count),
#     sdg_8_positive_count = sum(sdg_8_positive_count),
#     sdg_9_negative_count = sum(sdg_9_negative_count),
#     sdg_9_neutral_count = sum(sdg_9_neutral_count),
#     sdg_9_positive_count = sum(sdg_9_positive_count),
#     sdg_10_negative_count = sum(sdg_10_negative_count),
#     sdg_10_neutral_count = sum(sdg_10_neutral_count),
#     sdg_10_positive_count = sum(sdg_10_positive_count),
#     sdg_11_negative_count = sum(sdg_11_negative_count),
#     sdg_11_neutral_count = sum(sdg_11_neutral_count),
#     sdg_11_positive_count = sum(sdg_11_positive_count),
#     sdg_12_negative_count = sum(sdg_12_negative_count),
#     sdg_12_neutral_count = sum(sdg_12_neutral_count),
#     sdg_12_positive_count = sum(sdg_12_positive_count),
#     sdg_13_negative_count = sum(sdg_13_negative_count),
#     sdg_13_neutral_count = sum(sdg_13_neutral_count),
#     sdg_13_positive_count = sum(sdg_13_positive_count),
#     sdg_14_negative_count = sum(sdg_14_negative_count),
#     sdg_14_neutral_count = sum(sdg_14_neutral_count),
#     sdg_14_positive_count = sum(sdg_14_positive_count),
#     sdg_15_negative_count = sum(sdg_15_negative_count),
#     sdg_15_neutral_count = sum(sdg_15_neutral_count),
#     sdg_15_positive_count = sum(sdg_15_positive_count),
#     sdg_broad_negative_count  = sum(sdg_broad_negative_count),
#     sdg_broad_neutral_count   = sum(sdg_broad_neutral_count),
#     sdg_broad_positive_count = sum(sdg_broad_positive_count)
# )

# Join the weekly sentiment data with the stock prices
all_data <- stocks_weekly %>%
  left_join(sentiment_weekly, by = c("date","isin")) #%>%
  # drop_na(observations) 

# Write to CSV to keep in nice format:
       all_data %>%
           write.csv(file = "all_data_weekly.csv")


  
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

source(".R")

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

