
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(stringr, tidyverse, readxl, lubridate, imputeTS, reshape, purrr, readxl, reshape)

# Initial setup  ----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
fillNA <- function(vr, st, yr, mn){
  # vr <- 'tmax'; st <- 35055010; yr <- 1995; mn <- 'Jan'
  df <- data %>%
    filter(variable == vr,
           stt == st,
           year == yr,
           month == mn)
  ts <- ts(pull(df, value))
  nn <- sum(is.na(ts))
  
  if(nn < 26){
    int <- na.interpolation(ts)
    int <- as.numeric(int)
    dfm <- data.frame(value = int) %>%
      mutate(variable = vr,
             stt = st,
             year = yr,
             month = mn,
             day = 1:nrow(.)) %>% 
      as_tibble() %>% 
      dplyr::select(variable, stt, year, month, day, value)
  } else {
    print('No interpolate, many NAs')
  }
  print('Done!')
  return(dfm)
}
myFilter <- function(vr, st, yr, mn){
  # vr <- 'tmax'; st <- 35055010; yr <- 1995; mn <- 'Jan'  
  dt <- data %>% 
    filter(variable == vr &
             stt == st &
             year == yr &
             month == mn)
  print('Done')
  return(dt)
}

# Load data ---------------------------------------------------------------
data <- read_csv('ideam/prc/ideam_all.csv')
data <- mutate(data, month = factor(month, levels = month.abb))

# Counting the NAs --------------------------------------------------------
nas <- data %>% 
  group_by(variable, stt, year, month) %>%
  summarize(count = sum(is.na(value))) %>% 
  ungroup()
ntr <- filter(nas, count < 26)
trb <- filter(nas, count >= 26)
write.csv(nas, 'ideam/int/count_all_nas.csv', row.names = FALSE)

# Extracting the rows of NAs from data ------------------------------------
data_nas <- inner_join(data, trb, by = c('variable', 'stt', 'year', 'month'))
write.csv(data_nas, 'ideam/int/data_nas.csv', row.names = FALSE)  

# Apply the function to fill NAs ------------------------------------------
nst <- data %>% 
  nest(-variable, -stt, -year, -month) %>% 
  mutate(result = pmap(list(variable, stt, year, month), .f = fillNA))
df_days <- bind_rows(nst$result)

write.csv(df_days, 'ideam/int/tbl_int_days.csv', row.names = FALSE)
write.csv(trb, 'ideam/int/trb_days.csv', row.names = FALSE)