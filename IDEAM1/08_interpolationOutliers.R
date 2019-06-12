
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, stringr, reshape, readxl, imputeTS)

# Initial setup  ----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
fillNA <- function(dy){
  # dy <- 1
  print(paste('To start', dy))
  y <- apr %>%
    filter(day == dy) %>% 
    pull(6) %>% 
    ts() %>%
    na.interpolation() %>%
    as.numeric() %>%
    data.frame(year = 1995:2015,
               day = dy,
               value = .)
  print('Done')
  return(y)
}
fillNAday <- function(x){
  # x <- 1
  y <- nas %>% filter(row_number() == x)
  z <- tb2 %>% 
    filter(variable == pull(y, 1) &
             stt == pull(y, 2) &
             year == pull(y, 3) &
             month == pull(y, 4)) %>% 
    pull(value) %>% 
    ts() %>%
    na.interpolation() %>% 
    as.numeric() %>%
    tibble(value = .) %>% 
    mutate(variable = pull(y, 1),
           stt = pull(y, 2),
           year = pull(y, 3), 
           month = pull(y, 4),
           day = 1:nrow(.)) %>% 
    dplyr::select(variable, stt, year, month, day, value)
  print('Done')
  return(z)
}
fillNAfeb <- function(x){
  # x <- 1
  y <- nas_feb %>% filter(row_number() == x)
  z <- zzzz %>% 
    filter(variable == pull(y, 1) &
             stt == pull(y, 2) &
             year == pull(y, 3) &
             month == pull(y, 4)) %>% 
    pull(value) %>% 
    ts() %>%
    na.interpolation() %>% 
    as.numeric() %>%
    tibble(value = .) %>% 
    mutate(variable = pull(y, 1),
           stt = pull(y, 2),
           year = pull(y, 3), 
           month = pull(y, 4),
           day = 1:nrow(.)) %>% 
    dplyr::select(variable, stt, year, month, day, value)
  print('Done')
  return(z)
}

# Load data ---------------------------------------------------------------
tbl <- read_csv('ideam/int/tbl_int_all.csv') %>% 
  filter(stt != 1)
bss <- read_excel('bisiestos.xlsx') %>% 
  mutate(Month = factor(Month, levels = month.abb))

# Troubbles and no troubbles ----------------------------------------------
trb <- tbl %>% 
  filter(variable == 'tmin' &
         value >= 30)

# April 1997 --------------------------------------------------------------
apr <- tbl %>% 
  filter(month == 'Apr' & 
         variable == 'tmin' & 
         stt == '35055010') %>% 
  mutate(value = ifelse(value >= 30, NA, value)) 
apr <- apr %>%   
  nest(-day) %>%
  mutate(result = map(.x = day, .f = fillNA)) %>% 
  dplyr::select(result) %>% 
  unnest() %>% 
  mutate(month = 'Apr',
         variable = 'tmin',
         stt = 35055010) %>% 
  dplyr::select(variable, stt, year, month, day, value)

tb1 <- anti_join(tbl, apr, by = c('variable', 'stt', 'year', 'month', 'day'))
tbl <- rbind(tb1, apr)

tb2 <- tbl %>% 
  mutate(value = ifelse(variable == 'tmin' & value >= 30, NA, value)) 
nas <- tb2 %>% 
  filter(is.na(value))
xx <- 1:nrow(nas)
yy <- map(.x = 1:nrow(nas), .f = fillNAday)
yy <- bind_rows(yy)
yy <- inner_join(yy, nas, by = c('variable', 'stt', 'year', 'month', 'day')) %>% 
  dplyr::select(-value.y) %>% 
  dplyr::rename(value = value.x)
zz <- anti_join(tb2, nas, by = c('variable', 'stt', 'year', 'month', 'day'))
zzz <- rbind(yy, zz)
zzz <- zzz %>% 
  mutate(month = factor(month, levels = month.abb)) %>% 
  arrange(variable, stt, year, month, day)

# Review
smm <- zzz %>% 
  group_by(variable, stt, year, month) %>% 
  summarize(count = n()) %>%
  ungroup() %>% 
  mutate(month = factor(month, levels = month.abb)) %>% 
  arrange(stt, year, month)
smm <- inner_join(smm, bss, by = c('year' = 'Year', 'month' = 'Month')) %>%
  mutate(comparison = count == Days)

# Y ahora los bisiestos taran ---------------------------------------------
bss_tofill <- smm %>% 
  filter(comparison == F) %>% 
  dplyr::select(variable, stt, year, month)
yyyy <- bss_tofill %>% 
  mutate(day = 29,
         value = NA)
zzzz <- inner_join(zzz, bss_tofill, by = c('variable', 'stt', 'year', 'month')) 
zzzz <- rbind(zzzz, yyyy)
nas_feb <- zzzz %>% filter(is.na(value))

yyyyy <- map(.x = 1:nrow(nas_feb), .f = fillNAfeb)
yyyyy <- bind_rows(yyyyy)
yyyyy <- yyyyy %>% filter(day == 29)

data <- rbind(zzz, yyyyy)

write.csv(data, 'ideam/int/tbl_int_all_all.csv', row.names = FALSE)

smm <- data %>% 
  group_by(variable, stt, year, month) %>% 
  summarize(count = n()) %>%
  ungroup() %>% 
  mutate(month = factor(month, levels = month.abb)) %>% 
  arrange(stt, year, month) %>%
  inner_join(., bss, by = c('year' = 'Year', 'month' = 'Month')) %>%
  mutate(comparison = count == Days)
unique(smm$comparison)

