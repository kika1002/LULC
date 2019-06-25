
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, gtools, stplanr)

# Initial setup  ----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
readTbl <- function(fle, bsn){
  # fle <- fl; bsn <- 'cby'
  tbl <- grep(bsn, fle, value = TRUE) %>% 
    map(.x = ., .f = read_csv)
  print('Done!')
  return(tbl)
}

joinTbl <- function(cl){
  
  fl <- grep(cl, fls, value = TRUE) 
  
  # Read table
  t_cby <- readTbl(fle = fl, bsn = 'cby')
  t_tua <- readTbl(fle = fl, bsn = 'tua')
  
  # Join all the tables into only one
  t_cby <- Reduce(inner_join, t_cby)
  t_tua <- Reduce(inner_join, t_tua)
  
  # Order the table
  t_cby <- dplyr::select(t_cby, GID, x, y, UCS_F, everything())
  t_tua <- dplyr::select(t_tua, GID, x, y, UCS_F, everything())
  
  print('Done!')
  return(list(t_cby, t_tua))
  
}

# Load data ---------------------------------------------------------------
fls <- list.files('tbl', full.names = TRUE, pattern = '.csv$')
cls <- c('SND', 'CLY', 'SLT')

# Apply the function ------------------------------------------------------
tbls <- map(.x = cls, .f = joinTbl)
tbls <- flatten(tbls)

# Join all the tables -----------------------------------------------------
joinBsn <- function(n){
  ns <- sapply(1:length(tbls), function(k) nrow(tbls[[k]]))
  ps <- which(ns == n)
  tbl <- list(tbls[[ps[1]]], tbls[[ps[2]]], tbls[[ps[3]]])
  tbl <- Reduce(inner_join, tbl)
  return(tbl)
}

dfms <- map(.x = c(334, 1096), .f = joinBsn)

write.csv(dfms[[1]], './tbl/tua_txt.csv', row.names = FALSE)
write.csv(dfms[[2]], './tbl/tua_cby.csv', row.names = FALSE)







