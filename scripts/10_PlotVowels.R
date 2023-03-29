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
dat %>%
  group_by(gender, segment) %>%
  mutate(F1 = mean(F1, na.rm = TRUE),
         F2 = mean(F2, na.rm = TRUE),
         Duration = mean(duration_ms, na.rm = TRUE)) %>% 
  distinct(gender, segment, .keep_all = TRUE) %>%
  dplyr::select(gender, segment, F1, F2, Duration)  %>% 
  ungroup() %>%
  mutate(plotlabel = paste(gender, segment)) %>%
  ggplot(aes(F2, F1, color = segment, label = plotlabel)) +
  geom_point(aes(size = Duration), show.legend = FALSE) +
  geom_text_repel(show.legend = FALSE, alpha = 0.7) +
  scale_x_reverse() + #limits = c(2000, 1000)) +
  scale_y_reverse() + #limits = c(700, 300)) +
  scale_color_tq() +
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
