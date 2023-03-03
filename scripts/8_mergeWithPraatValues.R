#Author: Simon Gonzalez
#email: simon.gonzalez@anu.edu.au

#Loads libraries
#========================================================
library(data.table)
library(tidyverse)
library(readxl)
library(emuR)
library(udpipe)
library(vowels)

#Function for filtering
#........................................................
`%nin%` <- Negate(`%in%`)

#loads the functions to be used in the identification of contexts
#........................................................
source('./functions/sd_calculation_function.R')

#reads the main file
dat <- fread('nanasu_data.csv') %>%
  arrange(row_index)

#reads the populated file from Praat
dat_append <- fread('nanasu_append.csv') %>%
  arrange(row_index) %>%
  select(-one_of(c('row_index', 'speaker', 'time')))

#combines datasets
df <- bind_cols(dat, dat_append)

#saves dataset
write.csv(df, 'df_matched.csv', row.names = FALSE)

