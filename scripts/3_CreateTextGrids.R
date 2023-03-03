#Author: Simon Gonzalez
#email: simon.gonzalez@anu.edu.au

#Loads libraries
#========================================================
library(tidyverse)
library(rPraat)

#Reads the tranbscription and duration file CSV file created on the previous stage.
#From the script: 1_GetTranscriptionFiles.R
#........................................................
df <- read.csv('datainfo.csv')

#Creates the textgrids and saves it to a folder named: 
#........................................................
for(i in sort(unique(df$file))){

  #get all information of the iterated file
  dfi <- df %>%
    filter(file == i)
  
  #fix times to make sure the start times are lower than the end ones
  for(k in 2:nrow(dfi)){
    if(dfi$start[k] < dfi$end[k-1]){
      dfi$end[k-1] <- dfi$start[k]
    }
  }

  #get duration of the iterated file
  dur <- dfi$duration[1]
  
  #get start and endtime of the file
  dfi <- dfi %>%
    filter(start < dur) %>%
    filter(end <= duration)
  
  #gets the file name
  filename <- paste0('./tgs/', dfi$textgrid[1])

  #create TG from time information
  tg <- tg.createNewTextGrid(0, dur)
  #add tier
  tg <- tg.insertNewIntervalTier(tg, 1, "transcription")
  
  #adds intervals to tiers
  for(k in 1:nrow(dfi)){
    tg <- tg.insertInterval(tg, "transcription", dfi$start[k], dfi$end[k], dfi$clean_text[k])
  }
  
  #saves the TextGrid
  tg.write(tg = tg, fileNameTextGrid = filename)
}
