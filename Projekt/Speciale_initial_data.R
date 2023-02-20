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
  ) %>% na.omit()


# Write to CSV to keep in nice format:
  stocks_weekly %>%
      write.csv(file = "clean_stock_data_weekly.csv")


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


  # Read Sentiment data from CSV
  sentiment <- read.csv("nasdaq_global_large_cap.csv", sep = ",") %>%
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

# Add a variable that contains the sum of all, respectively, positive, negative and neutral signals wrt. SGD's: 
sentiment_weekly <- sentiment_weekly %>% group_by(date,isin) %>%
  mutate(
    negative_sum = sum(c_across(contains("sdg") & contains("negative_count"))),
    neutral_sum = sum(c_across(contains("sdg") & contains("neutral_count"))),
    positive_sum = sum(c_across(contains("sdg") & contains("positive_count")))
  ) 

# Join the weekly sentiment data with the stock prices
all_data_week <- stocks_weekly %>%
  left_join(sentiment_weekly, by = c("date","isin")) #%>%
  # drop_na(observations) 

# Write to CSV to keep in nice format:
       sentiment_weekly %>%
           write.csv(file = "nasdaq_global_data_weekly.csv")

