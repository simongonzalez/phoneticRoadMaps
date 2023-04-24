#Author: Simon Gonzalez
#email: simon.gonzalez@anu.edu.au

#Loads libraries
#========================================================
library(tidyverse)#data wrangling
library(tidyquant)#visualisation
library(lmerTest)#models
library(ggrepel)#text
library(joeyr)#analysis

#loads file
lod('dat_normalised_20220817.RData')

#reassigns dataframe
dat <- df

#Creates plots
#========================================================
# Group the dat data frame by gender and segment
dat %>%
  group_by(gender, segment) %>%
  
  # Add three new columns that represent the mean values of F1, F2, and duration_ms within each group
  mutate(F1 = mean(F1, na.rm = TRUE),
         F2 = mean(F2, na.rm = TRUE),
         Duration = mean(duration_ms, na.rm = TRUE)) %>% 
  
  # Keep only unique rows of gender and segment, with all columns intact
  distinct(gender, segment, .keep_all = TRUE) %>%
  
  # Keep only the gender, segment, F1, F2, and Duration columns
  dplyr::select(gender, segment, F1, F2, Duration)  %>% 
  
  # Remove the grouping structure of the data frame
  ungroup() %>%
  
  # Create a new column that concatenates gender and segment
  mutate(plotlabel = paste(gender, segment)) %>%
  
  # Create a scatterplot of F2 vs. F1, with point color determined by segment
  ggplot(aes(F2, F1, color = segment, label = plotlabel)) +
  
  # Add scatterplot points, with size determined by Duration
  geom_point(aes(size = Duration), show.legend = FALSE) +
  
  # Add labels to the scatterplot points, with partial transparency
  geom_text_repel(show.legend = FALSE, alpha = 0.7) +
  
  # Reverse the direction of the x-axis
  scale_x_reverse() + #limits = c(2000, 1000)) +
  
  # Reverse the direction of the y-axis
  scale_y_reverse() + #limits = c(700, 300)) +
  
  # Set the color palette for the segment variable
  scale_color_tq() +
  
  # Apply a custom theme to the plot, with a base font size of 12
  theme_tq(base_size = 12)


ggsave('./papermaterial/allVowels.jpg', width = 15, height = 12, units = 'cm')

#========================================================
dat %>%
  mutate(Gender = gender) %>%
  ggplot(aes(segment, duration_ms, fill = Gender)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 250)) +
  scale_fill_tq() +
  labs(x = 'Vowel', y = 'Duration in ms') + 
  facet_grid(~segment, scales = 'free') +
  theme_tq(base_size = 12)

ggsave('./papermaterial/allVowelsDur.jpg', width = 15, height = 12, units = 'cm')

#========================================================
dat %>%
  group_by(gender, segment) %>%
  mutate(F1 = mean(F1_sc, na.rm = TRUE),
         F2 = mean(F2_sc, na.rm = TRUE),
         Duration = mean(duration_ms, na.rm = TRUE)) %>% 
  distinct(gender, segment, .keep_all = TRUE) %>%
  select(gender, segment, F1, F2, Duration)  %>% 
  ungroup() %>%
  mutate(plotlabel = paste(gender, segment)) %>%
  ggplot(aes(F2, F1, color = interaction(gender, segment), label = plotlabel)) +
  geom_point(aes(size = Duration), show.legend = FALSE) +
  geom_text_repel(show.legend = FALSE, alpha = 0.7) +
  scale_x_reverse() + #limits = c(2000, 1000)) +
  scale_y_reverse() + #limits = c(700, 300)) +
  scale_color_tq() +
  theme_tq(base_size = 12)

ggsave(paste0('./11_explore/all.png'), width = 15, height = 10, units = 'cm')

#========================================================
dat %>%
  group_by(region, segment) %>%
  mutate(F1 = mean(F1_sc, na.rm = TRUE),
         F2 = mean(F2_sc, na.rm = TRUE),
         Duration = mean(duration_ms, na.rm = TRUE)) %>% 
  distinct(region, segment, .keep_all = TRUE) %>%
  select(region, segment, F1, F2, Duration, stressed)  %>% 
  ungroup() %>%
  mutate(plotlabel = paste(region, segment)) %>%
  ggplot(aes(F2, F1, color = region, label = plotlabel)) +
  geom_point(aes(size = Duration)) +
  #geom_text_repel(show.legend = FALSE, alpha = 0.7) +
  scale_x_reverse(limits = c(2000, 1000)) +
  scale_y_reverse(limits = c(700, 300)) +
  scale_color_tq() +
  facet_wrap(~stressed) +
  theme_tq(base_size = 12)

ggsave(paste0('./11_explore/all_regions.png'), width = 15, height = 10, units = 'cm')
