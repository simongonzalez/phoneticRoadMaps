#Author: Simon Gonzalez
#email: simon.gonzalez@anu.edu.au

#Loads libraries
#========================================================
library(rPraat)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(readr)
library(parallel)
library(foreach)
library(doParallel)
library(tictoc)
library(tidyverse)
library(emuR)
library(udpipe)
library(vowels)

#loads the functions to be used in the identification of contexts
#........................................................
source('./functions/sd_calculation_function.R')
source('./functions/findSegments.R')
source('./functions/findTranscription.R')
source('./functions/findWords.R')
source('./functions/get_name_parameter.R')

#gets the file paths of the TextGrid files created in the previous stage, in the folder called: "tgs"
fls <- list.files('./tgs', full.names = T, pattern = '.TextGrid')

#Reads the textgrid files and gets the information for each segment
for(i in fls){

  #reads the textgrid file
  tg <- tg.read(i, encoding = as.character(guess_encoding(i)$encoding[1]))
  
  #gets the fullname
  fullname <- gsub('.TextGrid', '', basename(i))
  #splits the name into separate components
  namesSplit <- unlist(strsplit(fullname, '_'))

  #assigns sections
  tmpRegion <- 'NANASU'
  tmpGender <- namesSplit[2]
  tmpName <- namesSplit[1]
  tmpRec <- namesSplit[3]
  
  #gets locations of segments and their indexes
  tmpSegsLocs <- which(!tg[[2]]$label %in% c('', 'sp', 'sil'))
  tmpSegs <- str_squish(tg[[2]]$label[tmpSegsLocs])
  tmpSegsPrev <- tg[[2]]$label[tmpSegsLocs-1]
  tmpSegsFoll <- tg[[2]]$label[tmpSegsLocs+1]
  tmpSegsOnset <- tg[[2]]$t1[tmpSegsLocs]
  tmpSegsOffset <- tg[[2]]$t2[tmpSegsLocs]
  tmpSegsDur <- tmpSegsOffset - tmpSegsOnset
  
  #sets the number of the segment tier
  segmentTierLabel <- names(tg)[2]
  #sets the number of the word tier
  wordTierLabel <- names(tg)[1]
  
  #get words
  #........................................................
  wordlabel = NULL
  wordOnset = NULL
  wordOffset = NULL
  wordMid = NULL
  wordDur = NULL
  wordLoc <- NULL
  
  for(j in 1:length(tmpSegsLocs)){
    wordlabel[j] = str_squish(findWords(data = tg, segNumber = tmpSegsLocs[j], tierLabel = segmentTierLabel, wordTier = wordTierLabel)[[1]])
    wordOnset[j] = findWords(data = tg, segNumber = tmpSegsLocs[j], tierLabel = segmentTierLabel, wordTier = wordTierLabel)[[2]]
    wordOffset[j] = findWords(data = tg, segNumber = tmpSegsLocs[j], tierLabel = segmentTierLabel, wordTier = wordTierLabel)[[3]]
    wordMid[j] = findWords(data = tg, segNumber = tmpSegsLocs[j], tierLabel = segmentTierLabel, wordTier = wordTierLabel)[[4]]
    wordDur[j] = findWords(data = tg, segNumber = tmpSegsLocs[j], tierLabel = segmentTierLabel, wordTier = wordTierLabel)[[5]]
    wordLoc[j] = findWords(data = tg, segNumber = tmpSegsLocs[j], tierLabel = segmentTierLabel, wordTier = wordTierLabel)[[6]]
  }
  
  #get previous words
  prevWords <- tg[[wordTierLabel]]$label[wordLoc-1]
  
  #get following words
  follWords <- tg[[wordTierLabel]]$label[wordLoc+1]
  
  #stores the information in a dataframe
  tmpdf <- data.frame(fullname = i, speaker = fullname,
                      name = tmpName, gender = tmpGender, region = tmpRegion, rec = tmpRec,
                      previous = tmpSegsPrev, segment = tmpSegs, following = tmpSegsFoll, 
                      onset = tmpSegsOnset, offset = tmpSegsOffset, dur = tmpSegsDur,
                      previousWord = prevWords, word = wordlabel, followingWord = follWords, 
                      wordOnset, wordOffset, wordDur)
  
  #appends iterated dataframe
  if(i == fls[1]){
    df <- tmpdf
  }else{
    df <- rbind(df, tmpdf)
  }
  
}

#Add phonological descriptors
#========================================================

#identify the position of the segment in the word
df$position <- ifelse(df$onset == df$wordOnset, 'initial', 'medial')
df[df$offset == df$wordOffset, 'position'] <- 'final'
df[df$onset == df$wordOnset & df$offset == df$wordOffset, 'position'] <- 'both'

#assign type
df$type <- ifelse(df$segment %in% unlist(strsplit('a e è ë i ì o ò u ә', ' ')), 'vowel', 'consonant')

#assign phonological categories
#........................................................
df <- df %>%
  mutate(
    prevManner = case_when(
      previous %in% unlist(strsplit('b C d g k p t', ' ')) ~ 'stop',
      previous %in% unlist(strsplit('m n N', ' ')) ~ 'nasal',
      previous %in% unlist(strsplit('f s x S', ' ')) ~ 'fricative',
      previous %in% unlist(strsplit('l r R Y', ' ')) ~ 'liquid',
      previous %in% c('', ' ') ~ 'blank',
      TRUE ~ 'vowel'),
    manner = case_when(
      segment %in% unlist(strsplit('b C d g k p t', ' ')) ~ 'stop',
      segment %in% unlist(strsplit('m n N', ' ')) ~ 'nasal',
      segment %in% unlist(strsplit('f s x S', ' ')) ~ 'fricative',
      segment %in% unlist(strsplit('l r R Y', ' ')) ~ 'liquid',
      segment %in% c('', ' ') ~ 'blank',
      TRUE ~ 'vowel'),
    follManner = case_when(
      following %in% unlist(strsplit('b C d g k p t', ' ')) ~ 'stop',
      following %in% unlist(strsplit('m n N', ' ')) ~ 'nasal',
      following %in% unlist(strsplit('f s x S', ' ')) ~ 'fricative',
      following %in% unlist(strsplit('l r R Y', ' ')) ~ 'liquid',
      following %in% c('', ' ') ~ 'blank',
      TRUE ~ 'vowel'
    ),


    prevMannerMaj = case_when(
      previous %in% unlist(strsplit('b C d g k p t f s x S', ' ')) ~ 'obstruent',
      previous %in% unlist(strsplit('m n N l r R Y', ' ')) ~ 'sonorant',
      previous %in% c('', ' ') ~ 'blank',
      TRUE ~ 'vowel'
    ),
    mannerMaj = case_when(
      segment %in% unlist(strsplit('b C d g k p t f s x S', ' ')) ~ 'obstruent',
      segment %in% unlist(strsplit('m n N l r R Y', ' ')) ~ 'sonorant',
      segment %in% c('', ' ') ~ 'blank',
      TRUE ~ 'vowel'
    ),
    follMannerMaj = case_when(
      following %in% unlist(strsplit('b C d g k p t f s x S', ' ')) ~ 'obstruent',
      following %in% unlist(strsplit('m n N l r R Y', ' ')) ~ 'sonorant',
      following %in% c('', ' ') ~ 'blank',
      TRUE ~ 'vowel'
    ),


    prevPlaceMaj = case_when(
      previous %in% unlist(strsplit('b m p f', ' ')) ~ 'labial',
      previous %in% unlist(strsplit('C d l n N r R s S t Y', ' ')) ~ 'coronal',
      previous %in% unlist(strsplit('g k x', ' ')) ~ 'dorsal',
      previous %in% c('', ' ') ~ 'blank',
      TRUE ~ 'vowel'
    ),
    placeMaj = case_when(
      segment %in% unlist(strsplit('b m p f', ' ')) ~ 'labial',
      segment %in% unlist(strsplit('C d l n N r R s S t Y', ' ')) ~ 'coronal',
      segment %in% unlist(strsplit('g k x', ' ')) ~ 'dorsal',
      segment %in% c('', ' ') ~ 'blank',
      TRUE ~ 'vowel'
    ),
    follPlaceMaj = case_when(
      following %in% unlist(strsplit('b m p f', ' ')) ~ 'labial',
      following %in% unlist(strsplit('C d l n N r R s S t Y', ' ')) ~ 'coronal',
      following %in% unlist(strsplit('g k x', ' ')) ~ 'dorsal',
      following %in% c('', ' ') ~ 'blank',
      TRUE ~ 'vowel'
    ),

    prevVoicing = case_when(
      previous %in% unlist(strsplit('a A aj AJ aw AW b d e E ej EJ ew EW g i I ja JA je JE jo JO ju JU l m n N o O oj OJ r R u U uj UJ wa WA waj WAJ we WE wi WI wo WO Y', ' ')) ~ 'voiced',
      previous %in% unlist(strsplit('C f k p s S t x', ' ')) ~ 'voiceless',
      previous %in% c('', ' ') ~ 'blank'
    ),
    voicing = case_when(
      segment %in% unlist(strsplit('a A aj AJ aw AW b d e E ej EJ ew EW g i I ja JA je JE jo JO ju JU l m n N o O oj OJ r R u U uj UJ wa WA waj WAJ we WE wi WI wo WO Y', ' ')) ~ 'voiced',
      segment %in% unlist(strsplit('C f k p s S t x', ' ')) ~ 'voiceless',
      segment %in% c('', ' ') ~ 'blank'
    ),
    follVoicing = case_when(
      following %in% unlist(strsplit('a A aj AJ aw AW b d e E ej EJ ew EW g i I ja JA je JE jo JO ju JU l m n N o O oj OJ r R u U uj UJ wa WA waj WAJ we WE wi WI wo WO Y', ' ')) ~ 'voiced',
      following %in% unlist(strsplit('C f k p s S t x', ' ')) ~ 'voiceless',
      following %in% c('', ' ') ~ 'blank'
    )
  )

#identify places in the word
#........................................................
df$mid <- df$onset + ((df$offset - df$onset)/2)
df$wordMid <- df$wordOnset + ((df$wordOffset - df$wordOnset)/2)

#Duplicates entries for dynamic inforation for each token
#========================================================
time_df <- as.data.frame(matrix(nrow = nrow(df), ncol = 57))
names(time_df) <- c('index', 'midTime', paste0('f1_', 1:11), paste0('f2_', 1:11), paste0('f3_', 1:11), paste0('pitch_', 1:11), paste0('intensity_', 1:11))

newn <- 11

dfv <- df %>% filter(type == 'vowel')

for(i in 1:nrow(dfv)){
  print(i)
  tmpSeq <- seq(dfv$onset[i], dfv$offset[i], length.out = 11)
  tmpline <- dfv[i,] %>%
    slice(rep(1:n(), each = newn)) %>%
    mutate(token = i,
           perc = 1:11,
           time = tmpSeq
    )
  
  if(i == 1){
    dat <- tmpline
  }else{
    dat <- rbind(dat, tmpline)
  }
}

dat <- dat %>%
  mutate(row_index = 1:n())


#Add empty columns to be populated from Praat
#========================================================
dat_append <- dat %>%
  select(speaker, time, row_index) %>%
  mutate(
    formant_1 = 0, 
    formant_2 = 0, 
    formant_3 = 0, 
    pitchValue = 0, 
    intensityValue = 0,
    mfcc_1 = 0,
    mfcc_2 = 0,
    mfcc_3 = 0,
    mfcc_4 = 0,
    mfcc_5 = 0,
    mfcc_6 = 0,
    mfcc_7 = 0,
    mfcc_8 = 0,
    mfcc_9 = 0,
    mfcc_10 = 0,
    mfcc_11 = 0,
    mfcc_12 = 0
  )

#saves datasets
#========================================================
write.csv(dat, 'nanasu_data.csv', row.names = F, quote = F)
write.csv(dat_append, 'nanasu_append.csv', row.names = F, quote = F)
