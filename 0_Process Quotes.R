####
# Process Quotes
####

# Transcripts from https://en.wikiquote.org/wiki/The_Golden_Girls_(season_7)
#Have speaker, but limited selection

library(tidyverse); library(tidytext)

#Import
gq_raw <- readxl::read_excel("data/GG Quotes Raw.xlsx", col_names = F)

#The girls
list_gg <- c("Blanche", "Dorothy", "Sophia", "Rose")

#Processing w/o tidytext
gq_pro <- gq_raw %>% 
  rename(raw = X__1) %>% 
  separate(raw, c("Speaker", "Script"),  sep=":", extra="merge") %>% 
  mutate(Quote = trimws(gsub("\\[[^\\]]*\\]", "", Script, perl=T)),
         Quote = gsub("]", "", Quote, fixed=T),
         Quote = gsub("^\\s+", " ", Quote),
         Quote = trimws(Quote)) %>% 
  rowwise() %>% 
  mutate(Direction = ifelse(grepl("[", Script, fixed = T), 
                            gsub("[\\[\\]]", "", regmatches(Script, gregexpr("\\[.*?\\]", Script))[[1]]), NA)) %>% 
  ungroup() %>% 
  select(-Script) %>%
  rowwise() %>% 
  mutate(Episode = ifelse(grepl("[", Speaker, fixed = T), 
                          gsub("[\\[\\]]", "", regmatches(Speaker, gregexpr("\\[.*?\\]", Speaker))[[1]]), NA),
         Episode = as.numeric(gsub("]", "", gsub("[", "", Episode, fixed=T), fixed=T))
  ) %>% 
  ungroup() %>% 
  mutate(Episode = zoo::na.locf(Episode),
         Season = as.numeric(substr(Episode, 1, 1))) %>% 
  mutate(Speaker = ifelse(grepl("[", Speaker, fixed = T), NA, Speaker)) %>% 
  #Drop two consecutive NA rows
  mutate(index = ifelse(is.na(Speaker), 1, 0)) %>% 
  mutate(Quote_ID = cumsum(index)+1) %>% 
  select(-index) %>% 
  filter(!is.na(Speaker)) %>% 
  #filter(!is.na(Quote)) %>% 
  #some cleaning
  mutate(Speaker = gsub("[^[:alnum:][:space:]]", "", Speaker)) %>% 
  mutate(Speaker = as.character(Speaker)) %>% 
  filter(!is.na(Speaker)) %>%
  mutate(Speaker2 = ifelse(Speaker %in% list_gg, Speaker, "Other"))

#Save

saveRDS(gq_pro, "data/Processed_Quotes.RDS")
