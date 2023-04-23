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
############# Get stock market data monthly ###################
###############################################################

# Read stock data from:
ISIN <- read_excel("ISIN numbers.xlsx")
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
sentiment <- read.csv("SXXP_sentiment.csv", sep = ",") %>% 
# Get the correct date format and cut the series by 2018. 
  mutate(
    aggregate_time_period_start = as_datetime(aggregate_time_period_start)) %>%
  filter(aggregate_time_period_start >= ' 2018-01-01') %>%
  rename("date" = aggregate_time_period_start) %>%
  select(-c(aggregate_type,aggregate_key,aggregate_updated_ts,aggregate_time_period_end,day,week,month,year,match_identity)) 

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
       ISIN <- read_excel("ISIN numbers.xlsx")
       stock <- read_excel("SXXP_daily_2017.xlsx")
       
       
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
       stocks_daily <- stocks_clean_2017 %>%
         pivot_wider(
           names_from = type,
           values_from = value
         )  %>% na.omit() 
       
       
       
       stocks_daily = stocks_daily %>% rbind(stocks_daily_2017)
       
       
       # Write to CSV to keep in nice format:
       stocks_daily %>%
         write.csv(file = "clean_stock_data_daily.csv")
       
       remove(ISIN,stock,stocks_clean)
      
       stocks_daily <- read.csv("clean_stock_data_daily.csv", sep = ",") %>% select(-X)
       
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
       
    
       # Load the sentiment data:
       sentiment_daily <- read.csv("clean_sentiment_daily.csv", sep = ",") %>%
         select(-X)
       
       
       
       # Join the weekly sentiment data with the stock prices
       all_data_daily <- stocks_daily %>% mutate(date = as.Date(date)) %>% 
         left_join(sentiment_daily %>% mutate(date = as.Date(date)), by = c("date","isin"))
       
       # Write to CSV to keep in nice format:
       all_data_daily %>%
         write.csv(file = "data_daily.csv")

       
       
       
       
       
       
       
       
       
       
       
       
# TEst

      stocks = stocks_daily %>% group_by(isin) %>%
         mutate(date = date(date),
                Day = weekdays(date),
                date = case_when(Day == "lørdag" ~ as.character(NA), 
                                 Day == "søndag" ~ as.character(NA), 
                                 TRUE ~ as.character(date))) %>%
         na.omit() %>%
         group_by(isin) %>%
         mutate(PX_LAST_lag = lag(PX_LAST,5)) %>%
         na.omit() %>%
         mutate(ret = PX_LAST/PX_LAST_lag - 1,
                date = as.Date(date)) %>%
        filter(Day  == 'mandag')
      
      index = stocks %>% group_by(date) %>%
        summarise(mean = weighted.mean(ret, MKT_CAP))
      
      
      eu_3factors_daily <- read.csv("C:/Users/Marti/OneDrive - University of Copenhagen/KU/Speciale/Data behandling/Europe_3_Factors_Daily.csv", sep = ",") %>% 
        transmute(date = as.Date(ymd(X), format = "%Y-%m-%d"),
                  mkt_excess = as.numeric(Mkt.RF)/100,
                  SMB = as.numeric(SMB)/100,
                  HML = as.numeric(HML)/100,
                  RF = as.numeric(RF)/100) %>%
        filter(date >= '2018-01-01')   
      
      mkt_excess = eu_3factors_daily %>% select(date,mkt_excess)
      mkt_excess_xts = xts(mkt_excess$mkt_excess, order.by = mkt_excess$date)   
      mkt_excess_ret= aggregate(mkt_excess_xts, by=week, FUN=dailytoweekly)
      colnames(mkt_excess_ret)[1]  <- "mkt_excess"
      
      
      SMB = eu_3factors_daily %>% select(date,SMB)
      SMB_xts = xts(SMB$SMB, order.by = mkt_excess$date)   
      SMB_ret= aggregate(SMB_xts, by=week, FUN=dailytoweekly)
      colnames(SMB_ret)[1]  <- "SMB"
      
      HML = eu_3factors_daily %>% select(date,HML)
      HML_xts = xts(HML$HML, order.by = mkt_excess$date)   
      HML_ret= aggregate(HML_xts, by=week, FUN=dailytoweekly)
      colnames(HML_ret)[1]  <- "HML"
      
      RF = eu_3factors_daily %>% select(date,RF)
      RF_xts = xts(RF$RF, order.by = mkt_excess$date)   
      RF_ret = aggregate(RF_xts, by=week, FUN=dailytoweekly)
      colnames(RF_ret)[1]  <- "RF"
      
      
      factors = data.frame(date=index(mkt_excess_ret), coredata(mkt_excess_ret)) %>% select(mkt_excess)
      
      factors = mkt_excess_ret %>%
        cbind(SMB_ret,HML_ret,RF_ret)
      
      index
      
      ff3 = data.frame(date=index(factors), coredata(factors))
      
      FF3 <- read.csv("FF3_weekly.csv", sep = ";") %>% select(-X) %>% 
        mutate(
          date = as.Date(date, format = "%d-%m-%Y"),
          mkt_excess = as.numeric(mkt_excess),
          SMB = as.numeric(SMB),
          HML = as.numeric(HML),
          RF = as.numeric(RF)) %>%
      left_join(index, by = "date") %>% mutate(mkt_excess_ret = mean - RF) 

      
      
      library(PerformanceAnalytics) # for Return.cumulative function
         library(quantmod) # loads xts which has apply.monthly function 
         library(zoo)
          z = xts(dat$W_RETURN, order.by = as.Date(dat$date))
      
       week <- function(x) format(x, '%Y-%W')
       dailytoweekly <- function(x) (prod(x+1)-1)
       
       aggregate(mkt_excess_xts, by=week, FUN=dailytoweekly)
       
       # sentiment
       sentiment = sentiment_daily %>% select(date,isin,sentiment_negative_count,sdg_not_relevant_negative_count) %>% mutate(date = as.Date(date)) %>%
         group_by(isin) %>%
       summarize_by_time(
         .date_var = date,
         .by = "week",
         sentiment_negative_count = sum(sentiment_negative_count),
         sdg_not_relevant_negative_count = sum(sdg_not_relevant_negative_count),
         .type = c("ceiling"),
         .week_start = 1
       )

       data = stocks %>% left_join(sentiment, by = c("date","isin")) %>% transmute(isin,date, 
                                                                                   norm_negative_sum = sentiment_negative_count - sdg_not_relevant_negative_count,
                                                                                   ret, MKT_CAP) %>% arrange(isin)
       
       data %>%
         write.csv(file = "data_weekly.csv")

                               