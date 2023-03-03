#Author: Simon Gonzalez
#email: simon.gonzalez@anu.edu.au

#Loads libraries
#========================================================
library(tidyverse)
library(emuR)
library(udpipe)
library(vowels)
library(parallel)
library(foreach)
library(doParallel)

#loads the functions to be used in the identification of contexts
#........................................................
source('./functions/sd_calculation_function.R')

#reads the main file
#........................................................
load('dat_with_values.RData')

#reassign dataframe
datin <- df

#Filters data to five vowels
dat <- datin %>%
  mutate(F1 = formant_1,
         F2 = formant_2) %>%
  filter(perc == 6) %>%
  filter(type == 'vowel') %>%
  mutate(duration_ms = dur * 1000) %>%
  mutate(segment = segment...8) %>%
  filter(segment %in% c('a', 'e', 'i', 'o', 'u'))

#Normalise vowels
#========================================================
#creates a context for each token
dat$context <- 1:nrow(dat)

#creates a data with the required columns to be input into the normalisation functions
dnorm <- dat[,c('speaker', 'segment', 'context', 'formant_1', 'formant_2', 'formant_1', 'formant_1', 'formant_2', 'formant_1'),]
names(dnorm) <- c('speaker_id', 'vowel_id', 'context', 'F1', 'F2', 'F3', 'F1_glide', 'F2_glide', 'F3_glide')
#Deletes the unnecessary columns
dnorm$F3 <- NA
dnorm$F1_glide <- NA
dnorm$F2_glide <- NA
dnorm$F3_glide <- NA

#Normalises formants using the Lobanov method
normed.vowels <- norm.lobanov(as.data.frame(dnorm))

#Rescales the values to Hz
rescaled <- scalevowels(normed.vowels)

#Renames columns to be compatible with the input data
names(normed.vowels) <- c('speaker', 'segment',       'context',    'F1_norm'    ,'F2_norm', 'F1b', 'F2b')
#Substes to the relevant columns
normed.vowels <- normed.vowels[,c('speaker', 'segment',       'context',    'F1_norm'    ,'F2_norm')]
normed.vowels <- normed.vowels %>%
  select(context, F1_norm, F2_norm)

#Renames columns to be compatible with the input data
names(rescaled) <- c('speaker', 'segment',       'context',    'F1_sc'    ,'F2_sc', 'F1b', 'F2b')
#Substes to the relevant columns
rescaled <- rescaled[,c('speaker', 'segment',       'context',    'F1_sc'    ,'F2_sc')]
rescaled <- rescaled %>%
  select(context, F1_sc, F2_sc)

#Merges the normalised values to the main data frame
dat_normalised <- dat %>%
  left_join(normed.vowels, by = 'context') %>%
  left_join(rescaled , by = 'context')

#Save dataframe
save(dat_normalised, file = 'dat_normalised_20220817.RData')
#...............................................................................
