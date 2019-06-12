

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, stringr, reshape, readxl)

# Initial setup  ----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
tidyTbl <- function(st){
  # st <- 35195020
  print(paste('To start', st))
  tb <- tbl %>% 
    filter(stt == st) %>%
    mutate(month = factor(month, levels = month.abb)) %>%
    arrange(year, month, day) %>% 
    spread(variable, value) %>% 
    dplyr::select(tmax, tmin) %>% 
    setNames(c('19950101',''))
  write.table(tb, paste0('ideam/swt/tmp_', st, '.csv'), row.names = F, col.names = T, sep = ',', quote = FALSE)
  return(tb)
}
review <- function(tb){
  # tb <- tbl_stt[[1]]
  tb <- tb %>% 
    setNames(c('max', 'min')) %>% 
    mutate(comparison = ifelse(max >= min, TRUE, FALSE))
  u <- unique(tb$comparison)
  return(u)
}

# Load data ---------------------------------------------------------------
tbl <- read_csv('ideam/int/tbl_int_all_all.csv')
bss <- read_excel('bisiestos.xlsx') %>% mutate(Month = factor(Month, levels = month.abb))
stt <- unique(tbl$stt)

# Apply the function ------------------------------------------------------
tbl_stt <- map(.x = stt, .f = tidyTbl)
sapply(tbl_stt, nrow)
uniques <- map(.x = tbl_stt, .f = review)







