
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(stringr, tidyverse, readxl, lubridate, imputeTS, reshape, purrr, readxl, reshape)

# Initial setup  ----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
fillNA <- function(vr, st, mn, dy){
  # vr <- 'tmax'; st <- 35055010; mn <- 'Jan'; dy <- 1
  print(paste(vr, st, mn, dy))
  df <- data %>% 
    filter(variable == vr,
           stt == st,
           month == mn, 
           day == dy)
  ts <- ts(pull(df, value))
  nn <- sum(is.na(ts))
  print('To make the conditional')
  if(nn < 18){
    int <- na.interpolation(ts)
    int <- as.numeric(int)
    dfm <- data.frame(value = int) %>%
      mutate(variable = vr,
             stt = st,
             year = 1995:2015,
             month = mn,
             day = dy) %>% 
      dplyr::select(variable, stt, year, month, day, value) %>%
      as_tibble()
  } else {
    print('No interpolate, many NAs')
  }
  print('Done!')
  return(dfm)
}

# Load data ---------------------------------------------------------------
data <- read_csv('ideam/prc/ideam_all.csv')
data <- mutate(data, month = factor(month, levels = month.abb))
trb <- read_csv('../_ideam/ideam/int/trb_days.csv')

# Counting the NAs --------------------------------------------------------
nas <- data %>% 
  group_by(variable, stt, month, day) %>% 
  summarize(count = sum(is.na(value))) %>%
  ungroup()
View(nas)
trb <- filter(nas, count >=18)

# Apply the function to fill NAs by year ----------------------------------
data_sub <- data %>% 
  filter(!year %in% c(1996, 2000, 2004, 2008, 2012) |
         !month == 'Feb' | 
         !day == 29)
data_su2 <- data %>% 
  filter(year %in% c(1996, 2000, 2004, 2008, 2012),
         month == 'Feb', 
         day == 29)

nst <- data_sub %>% 
  nest(-variable, -stt, -month, -day) %>% 
  mutate(result = pmap(list(variable, stt, month, day), .f = fillNA))
nst <- bind_rows(nst$result)

nst <- rbind(nst, data_su2)
write.csv(nst, 'ideam/int/tbl_int_years.csv', row.names = FALSE)

tst <- read_csv('ideam/int/tbl_int_years.csv')
tst %>% 
  filter(variable == 'tmin' &
           stt == 35095110 &
           year == 2004 &
           month == 'Feb') %>%
  View()





