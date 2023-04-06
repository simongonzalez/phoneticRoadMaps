#Author: Simon Gonzalez
#email: simon.gonzalez@anu.edu.au

#Loads libraries
#========================================================
library(tidyverse) #data wrangling
library(data.table) #data wrangling
library(readr) #reading files in multiple formats
library(warbleR) #audio files manipulation

#gets the file paths of the transcription files in xml format, in a folder named XML
fls <- list.files('./xml', full.names = TRUE)

#Reads information from each transcription file
#........................................................
for(i in fls){
  print(i)# checks progress
  
  #Reads XML file line by line from the input files
  df <- str_squish(read_lines(i))
  
  #locates lines that identify the speaker id, idenitified by the start string
  sid_locs <- grep('^<S id=', df)
  #locates the audio index
  audio_index <- sid_locs + 1
  #locates the form index
  form_index <- sid_locs + 2
  
  #Creates a dataframe with information on the file, id, audio, and form.
  dati <- data.frame(file = i, id = df[sid_locs], 
                     audio = df[audio_index], 
                     form = df[form_index])
  
  #Saves the iterated dataframe into a major dataset.
  if(i == fls[1]){
    dat <- dati
  }else{
    dat <- rbind(dat, dati)
  }
}

#This section identifies the time-stamp information:
#Start time, End time, and text (orthographic transcription) for each speaker intervention.
#........................................................
dat2 <- dat %>%
  mutate(start = as.numeric( str_squish( gsub('start=|"', '', 
                                              gsub('end=.*', '',  gsub('<AUDIO |/>', '', audio)) ) ) ) ) %>%
  mutate(end = as.numeric( str_squish( gsub('end=|"', '', 
                                              gsub('.*end=', '',  gsub('<AUDIO |/>', '', audio)) ) ) ) ) %>%
  mutate(raw_text = gsub('<FORM>|</FORM>', '', form)) %>%
  mutate(clean_text = str_squish( tolower( gsub(',|\\.|–|-|\\?|\\!|_|\\«|\\»|\\||\\/|…|:|"|\\[|\\]|\\(|\\)|\\{|\\}|;|‘|’|“|„|\\*|=', '', raw_text)) ) ) %>%
  mutate(audioFile = gsub('\\/xml\\/', '/audio/', file)) %>%
  mutate(audioFile = gsub('\\.xml', '.mp3', audioFile))


#Gets the duration information for each audio file located in the "audio" folder.
#........................................................
all_df <- warbleR::duration_sound_files(path = './audio') %>%
  rename(sound = sound.files) %>%
  mutate(audioFile = paste0('./audio/', sound)) %>%
  right_join(dat2, by = 'audioFile') %>%
  mutate(name = gsub('\\.mp3', '', sound)) %>%
  mutate(textgrid = paste0(name, '.TextGrid'))

#Saves the duration information for each file, as well as the transcription information.
#The output is a CSV file.
#........................................................
write.csv(all_df, 'datainfo.csv', row.names = F)
