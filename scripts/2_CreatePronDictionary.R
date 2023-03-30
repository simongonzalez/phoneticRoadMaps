#Loads libraries
#========================================================
library(rPraat)#Textgrid processing
library(readtext)#read data from text
library(tidyverse)#data wrangling

#Function for filtering
#........................................................
`%nin%` <- Negate(`%in%`)

#Reads the tranbscription and duration file CSV file created on the previous stage.
#From the script: 1_GetTranscriptionFiles.R
#........................................................
df <- read.csv('datainfo.csv')

#Gets all unique words in all the transcriptions
#........................................................
allwords <- sort(unique(unlist(str_split(df$clean_text, ' '))))
#Total = 1557 words


#Creates an empty variable to store the phonemic representation of the words
#........................................................
entries = NULL

#Creates the phonemic representations
#........................................................
for(ii in 1:length(allwords)){
  tmpE = allwords[ii]
  
  if(is.na(as.numeric(tmpE))){
    tmpE = paste(unlist(strsplit(tmpE, '')), collapse = ' ')
  }
  
  entries = append(entries, tmpE)
  
}

#stores entries and phonemes as a dataframe
pronDict = data.frame(allwords, entries)

#saves the dictionary as a Tab-Separated file
write.table(pronDict, file = paste0('nanasu_asis_', Sys.time(), '.txt'), sep = '\t', quote = F, row.names = F, col.names = F)
