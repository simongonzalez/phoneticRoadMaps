#Author: Simon Gonzalez
#email: simon.gonzalez@anu.edu.au

#Loads libraries
#========================================================
library(tidyverse)
library(plotly)
library(data.table)

#get coefficient of dynamic values function
#========================================================
get_coeffs <- function(tmpvec, namein){
  smooth_coeffs <- emuR::dct(tmpvec, m = 2, fit = TRUE)
  all_coeffs <- emuR::dct(tmpvec, m = 3, fit = F)
  
  out_c0 <- all_coeffs[1]
  out_c1<- all_coeffs[2]
  out_c2 <- all_coeffs[3]
  out_c3 <- all_coeffs[4]
  
  out_target_20 <- smooth_coeffs[3]
  out_target_50 <- smooth_coeffs[6]
  out_target_80 <- smooth_coeffs[9]
  
  tmpdf <- data.frame(smooth_coeffs, out_c0, out_c1, out_c2, out_c3,
                      out_target_20, out_target_50, out_target_80)
  
  names(tmpdf) <- paste0(namein, '_', names(tmpdf))
  
  return(tmpdf)
}

#reads the main file
#........................................................
dfin <- as.data.frame(fread('df_matched.csv'))

#assigns an overall token index
#........................................................
df <- dfin %>%
  select(-one_of('pitchValue')) %>%
  group_by(fullname, onset) %>%
  mutate(overall_token = cur_group_id())

for(tokeni in sort(unique(df$overall_token))){
  print(paste0(tokeni, '/', length(sort(unique(df$overall_token)))))
  dftoken <- df %>%
    filter(overall_token == tokeni)
  
  iterated_cols <- unlist(strsplit('formant_1 formant_2 formant_3 intensityValue mfcc_1 mfcc_2 mfcc_3 mfcc_4 mfcc_5 mfcc_6 mfcc_7 mfcc_8 mfcc_9 mfcc_10 mfcc_11 mfcc_12', ' '))
  
  for(coli in iterated_cols){
    dftoken <- dftoken %>%
      bind_cols(get_coeffs(dftoken[[coli]], coli))
  }
  
  if(tokeni == 1){
    dat <- dftoken
  }else{
    dat <- rbind(dat, dftoken)
  }
  
}

#creates a copied dataframe
dat_with_values <- dat

#saves dataframe
save(dat_with_values, file = 'dat_with_values.RData')

