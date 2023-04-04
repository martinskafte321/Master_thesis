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
############# Get stock market data weekly ###########################
###############################################################

# Read stock data from:
ISIN <- read_excel("ISIN numbers.xlsx")
stock <- read_excel("SXXP_monthly.xlsx")

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
stocks_monthly <- stocks %>%
  pivot_wider(
    names_from = type,
    values_from = value
  ) %>% unnest(c(PX_LAST,MKT_CAP,W_RETURN)) %>% na.omit()


# Write to CSV to keep in nice format:
stocks_monthly %>%
      write.csv(file = "clean_stock_data_monthly.csv")
  

###############################################################
############# Get Sentiment data monthly ##############################
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
sentiment_monthly <- sentiment %>%
  group_by(isin) %>%
  mutate(date = ceiling_date(date + 1, "month") - 1) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  group_by(date,isin) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)  %>%
  group_by(isin) %>%
  mutate(date = lead(date)) 

# Join the weekly sentiment data with the stock prices
all_data_monthly <- stocks_monthly %>%
  left_join(sentiment_monthly, by = c("date","isin"))

# Write to CSV to keep in nice format:
all_data_monthly %>%
           write.csv(file = "all_data_month.csv")

       
       ###############################################################
       ############# Get stock market data daily ###########################
       ###############################################################
       
       # Read stock data from:
       ISIN <- read_excel("ISIN numbers.xlsx")
       stock <- read_excel("SXXP_daily.xlsx")
       
       
       # Clean up the stock market data: Remove the two first rows 
       stocks_clean <- stock %>%
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
       stocks_daily <- stocks_clean %>%
         pivot_wider(
           names_from = type,
           values_from = value
         )  %>% na.omit() 
       
       stocks_daily = stocks_daily %>% rbind(stocks_daily_2017)
       
       
       # Write to CSV to keep in nice format:
       stocks_daily %>%
         write.csv(file = "clean_stock_data_daily.csv")
       
       remove(ISIN,stock,stocks_clean)
      
       
       ###############################################################
       ############# Get Sentiment data daily ##############################
       ###############################################################
       
       # Read Sentiment data from CSV
       sentiment <- read.csv("SXXP_sentiment.csv", sep = ",") %>% 
         # Get the correct date format and cut the series by 2018. 
         mutate(
           aggregate_time_period_start = as_datetime(aggregate_time_period_start)) %>%
         filter(aggregate_time_period_start >= ' 2018-01-01') %>%
         rename("date" = aggregate_time_period_start) %>%
         select(-c(match_names,aggregate_type,aggregate_key,aggregate_updated_ts,aggregate_time_period_end,day,week,month,year,match_identity)) 
       
       
       # Add a variable that contains the sum of all, respectively, positive, negative and neutral signals wrt. SGD's: 
       sentiment_daily <- sentiment %>% 
         group_by(isin) %>%
         # Lead the sentiment data with respect to stock market data, so we can measure t = -1 news with t = 0 returns. 
         mutate(date = date(date),
                Day = weekdays(date), 
                date = case_when(Day == "lørdag" ~ date + days(2), 
                                 Day == "søndag" ~ date + days(1), 
                                 TRUE ~ date)
                ) %>% 
       select(-Day) %>%
         group_by(date,isin) %>%
         summarise_all(mean)
       
       
       # Write to CSV to keep in nice format:
       sentiment_daily %>%
         write.csv(file = "clean_sentiment_daily.csv")
       
       
       stocks <- read.csv("clean_stock_data_daily.csv", sep = ",") %>%
         select(-X)
       
       sentiment_daily_test <- read.csv("clean_sentiment_daily.csv", sep = ",") %>%
         select(-X)
       
       
       
       # Join the weekly sentiment data with the stock prices
       all_data_daily <- stocks %>% mutate(date = as.Date(date)) %>% 
         left_join(sentiment_daily %>% mutate(date = as.Date(date)), by = c("date","isin"))
       
       # Write to CSV to keep in nice format:
       all_data_daily %>%
         write.csv(file = "data_daily.csv")

       