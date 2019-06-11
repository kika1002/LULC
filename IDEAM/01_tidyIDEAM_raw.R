
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(stringr, tidyverse, readxl, purrr, readxl)

# Initial setup  ----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
Numextract <- function(string){
  unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)))
}
tidyData <- function(fle){
  # fle <- fls[[1]]
  print(paste0('To start ', fle))
  nme <- basename(fle)
  nmb <- Numextract(nme)
  var <- str_sub(nme, start = nchar(nme) - 8, end = nchar(nme) - 5)
  tbl <- read_excel(fle) %>% 
    setNames(c('day', month.abb)) %>% 
    mutate(variable = var,
           stt = nmb[1],
           year = nmb[2]) 
  tbl <- tbl %>% 
    gather(month, value, -stt, -variable, -year, -day) %>% 
    dplyr::select(variable, stt, year, month, day, value) %>%
    mutate(value = as.numeric(value))
  
  # Meses
  mnt <- bss %>% 
    filter(Year == nmb[2])
  m30 <- mnt %>% 
    filter(Days == 30) %>% 
    pull(2)
  m31 <- mnt %>% 
    filter(Days == 31) %>% 
    pull(2)
  feb <- mnt %>% 
    filter(Month == 'Feb') %>% 
    pull(3)
  
  # Filter by month (28, 30, 31)
  tb1 <- tbl %>% 
    filter(month %in% m31)
  tb2 <- tbl %>% 
    filter(month %in% m30,
           day != 31)
  tb3 <- tbl %>% 
    filter(month %in% 'Feb',
           !day %in% c(29, 30, 31))
  tbl <- rbind(tb1, tb2, tb3) %>%
    mutate(month = factor(month, levels = month.abb))
  print('Done!')
  return(tbl)
}

# Load data ---------------------------------------------------------------
fls <- list.files('ideam/raw', full.names = T, pattern = '.xlsx')
bss <- read_excel('bisiestos.xlsx')

# Tidy all the new IDEAM tables -------------------------------------------
dfm <- map(.x = fls, .f = tidyData)
dfm <- bind_rows(dfm)
write.csv(dfm, 'ideam/prc/ideam_prc.csv', row.names = FALSE)










