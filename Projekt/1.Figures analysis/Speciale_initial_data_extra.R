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
############## ESG Risk Ratings ###############################
###############################################################

ESG <- read_excel("ESG_RR.xlsx", sheet = "Results") %>% 
  select(ISIN, ESG_risk_category)


ESG <- ISIN %>% select(ISIN = id_isin) %>% left_join(ESG, by = "ISIN") %>% mutate(isin = ISIN)

# Write to CSV to keep in nice format:
ESG %>%
  write.csv(file = "ESG_ratings.csv")

###############################################################
############# Get stock market data monthly ###################
###############################################################

# Read stock data from:
ISIN <- read_excel("world_index__ISIN.xlsx")
stock <- read_excel("SXXP_MONTHLY_EUR.xlsx")
free_float_data <- read_excel("free_float_sxxp.xlsx")

free_float = free_float_data %>%
  filter(!row_number() %in% c(1,2)) %>% 
  # Make the data set a long matrix instead of wide
  pivot_longer(!DATES, names_to = "firm",values_to = "free_float") %>%
  mutate(
    type = str_split_fixed(firm, "_", 2)[,2],
    ID = str_split_fixed(firm, "_", 2)[,1],
    date = as.Date(as.numeric(DATES), origin = "1899-12-30"),
    free_float = as.numeric(free_float)
  ) %>% 
  select(-DATES,-firm)

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
  # And join the stock information with the ISIN number:
  left_join(ISIN, by = "ID") %>% rename("isin" = id_isin)

# Summarize the stocks to index by date and isin
stocks_monthly <- stocks %>% 
  pivot_wider(
    names_from = type,
    values_from = value
  ) %>% unnest(c(PX_LAST,MKT_CAP,W_RETURN)) %>% 
  left_join(free_float, by = c("date","ID")) %>%
  na.omit() %>% 
  mutate(date = (ceiling_date(date, "month")-1),
         free_float_mkt_cap = MKT_CAP * (free_float/100)) %>%
  select(-c(ID,free_float,type))


# Write to CSV to keep in nice format:
stocks_monthly %>%
      write.csv(file = "clean_stock_data_monthly.csv")
  
###############################################################
############# Get Sentiment data monthly ##############################
###############################################################

# Read Sentiment data from CSV
sentiment <- read.csv("nasdaq_global_large_cap.csv", sep = ",") %>% 
# Get the correct date format and cut the series by 2018. 
  mutate(
    aggregate_time_period_start = as_datetime(aggregate_time_period_start)) %>%
  filter(aggregate_time_period_start >= ' 2018-01-01') %>%
  rename("date" = aggregate_time_period_start) %>%
  select(-c(aggregate_type,aggregate_key,aggregate_updated_ts,aggregate_time_period_end,day,week,month,year,match_identity,sentiment_negative_mean:sdg_broad_positive_count)) 

# Remove unnecessary items:
remove(stock,stocks,ISIN,free_float,free_float_data)

# Compute the last business day of the week, then sum the amount of observations for that week
# Summarize sentiment data into weekly observations to match the stock market data: countable items are summed, averages are averaged (runtime: 1 min)
sentiment_monthly <- sentiment %>%
  group_by(isin) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         date = ceiling_date(date + 1, "month")-1) %>%
  group_by(date,isin) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)  %>%
  group_by(isin)

# Join the monthly sentiment data with the stock prices
all_data_monthly <- stocks_monthly %>% mutate(date = as.Date(date)) %>% 
  left_join(sentiment_monthly %>% mutate(date = as.Date(date)), by = c("date","isin"))


# Write to CSV to keep in nice format:
all_data_monthly %>%
           write.csv(file = "all_data_month.csv")

       
###############################################################
############# Get stock market data daily ###########################
###############################################################
       
       # Read stock data from:
ISIN <- read_excel("world_index__ISIN.xlsx")
stock <- read_excel("world_index_daily.xlsx")
       
       
       # Clean up the stock market data: Remove the two first rows 
       stocks_clean <- stock %>%
         filter(!row_number() %in% c(1)) %>% 
         # Make the data set a long matrix instead of wide
         pivot_longer(!DATES, names_to = "firm",values_to = "value")  %>% 
         mutate(
           date = as.Date(as.numeric(DATES), origin = "1899-12-30"),
           value = as.numeric(value)) %>% 
         select(-DATES) %>%
         # And join the stock information with the ISIN number:
         left_join(ISIN, by = "firm") %>% rename("isin" = id_isin)
         
       
       # Write to CSV to keep in nice format:
       stocks_clean %>%
         write.csv(file = "clean_stock_data_daily_NASDAQ.csv")
       
       remove(ISIN,stock,stocks_clean)
      
       stocks_daily <- read.csv("clean_stock_data_daily.csv", sep = ",") %>% select(-X)
       
       ###############################################################
       ############# Get Sentiment data daily ##############################
       ###############################################################
       
       # Read Sentiment data from CSV
       sentiment <- read.csv("nasdaq_global_large_cap.csv", sep = ",")
       
       
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
         write.csv(file = "clean_sentiment_daily_nasdaq.csv")
       
    
       # Load the sentiment data:
       sentiment_daily <- read.csv("clean_sentiment_daily.csv", sep = ",") %>%
         select(-X)
       
       
       
       # Join the weekly sentiment data with the stock prices
       all_data_daily <- stocks_clean %>% mutate(date = as.Date(date)) %>% 
         left_join(sentiment_daily %>% mutate(date = as.Date(date)), by = c("date","isin"))
       
       # Write to CSV to keep in nice format:
       all_data_daily %>%
         write.csv(file = "data_daily_nasdaq.csv")

       
       
       