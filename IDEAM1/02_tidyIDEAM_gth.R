
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(stringr, tidyverse, readxl, lubridate, purrr, readxl)

# Initial setup  ----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
tidyData <- function(yr){
  # yr <- 1995
  print(paste('To start', yr))
  
  tb <- filter(data, year == yr)
  mn <- filter(bss, Year == yr)
  
  # Months
  m30 <- filter(mn, Days == 30) %>% pull(2)
  m31 <- filter(mn, Days == 31) %>% pull(2)
  feb <- filter(mn, Month == 'Feb') %>% pull(3)
  
  # Filtering
  tb1 <- filter(tb, month %in% m31)
  tb2 <- filter(tb, month %in% m30,
                    day != 31)
  tb3 <- filter(tb, month == 'Feb')
  
  # February (conditional)
  if(feb == 28){
    tb3 <- filter(tb3, !day %in% c(29, 30, 31))
  } else {
    tb3 <- filter(tb3, !day %in% c(30, 31))
  }
  
  # Rbind
  fnl <- rbind(tb1, tb2, tb3) %>% 
    mutate(month = factor(month, levels = month.abb))
  print('Done!')
  return(fnl)
  
}

# Load data ---------------------------------------------------------------
data <- read_csv('tables_tidy_gth.csv') %>% dplyr::select(-mnth)
bss <- read_excel('bisiestos.xlsx')
yrs <- unique(data$year)

# Tidy table --------------------------------------------------------------
data <- map(.x = yrs, .f = tidyData)
data <- bind_rows(data)
write.csv(data, 'tables_tidy_gth_v2.csv', row.names = FALSE)

