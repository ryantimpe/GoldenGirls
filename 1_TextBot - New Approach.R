####
# GG Text Processing
####

library(tidyverse); library(tidytext);

#Import
gg_raw <- readxl::read_excel("data/GG Quotes Raw.xlsx", col_names = F)
gg_trans_raw <- readRDS("data/Processed_Transcripts.RDS")

####
# Quotes - use as the base
#####

#Processing w/o tidytext
gg_pro1 <- gg_raw %>% 
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
  mutate(Speaker = gsub("[^[:alnum:][:space:]]", "", Speaker))

####
# Transcripts!
####

####
# EDits parts of speech
####

parts_of_speech_gg <- parts_of_speech %>% 
  group_by(word) %>% 
  filter(!any((pos %in% c("Preposition", "Conjunction", "Interjection", "Verb")))) %>% 
  ungroup() %>% 
  filter(!(word %in% c("a", "i", "an", "my", "be", "and", "the", "it's", "in", "at", "of"))) %>% 
  filter(!grepl("'s", word, fixed=T)) 

#Helper functions
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

n_words <- function(x, start=1 , end=3, tail = F, tail_length = 2) {
  if(!tail){
    ul <- unlist(strsplit(x, split = "\\s+"))[start:end]
  } else {
    ul <- tail(unlist(strsplit(x, split = "\\s+")), tail_length)
  }
  paste(ul,collapse=" ")
}

####
# Bot Text generator
####
list_gg <- c("Blanche", "Dorothy", "Sophia", "Rose")

gg_pro2 <- gg_pro1 %>% 
  mutate(Speaker = as.character(Speaker)) %>% 
  filter(!is.na(Speaker)) %>%
  mutate(Speaker2 = ifelse(Speaker %in% list_gg, Speaker, "Other"))

gb_words <- gg_pro2 %>% 
  unnest_tokens(word, Quote) 






gb_sent <- gg_pro2 %>% 
  unnest_tokens(sentence, Quote, token="sentences", drop=F) %>% 
  rowwise() %>% 
  mutate(sent2 = n_words(sentence, tail=T)) %>% 
  ungroup() %>% 
  unnest_tokens(last2, sent2, token="ngrams", n=2, drop=F)

gb_ngrams <- gg_pro2 %>%
  unnest_tokens(phrase, Quote, token="ngrams", n=5, drop=F) %>% 
  bind_rows(
    gg_pro2 %>% unnest_tokens(phrase, Quote, token="ngrams", n=6, drop=F)
  ) %>% 
  bind_rows(
    gg_pro2 %>% unnest_tokens(phrase, Quote, token="ngrams", n=7, drop=F)
  ) %>% 
  rowwise() %>% 
  mutate(first_word = unlist(strsplit(phrase, " "))[[1]],
         last_word = unlist(strsplit(phrase, " "))[[length(unlist(strsplit(phrase, " ")))]]) %>% 
  ungroup()

gb_2gram <- gg_pro2 %>% 
  unnest_tokens(bigram, Quote, token="ngrams", n=2, drop=F)

gb_3gram <- gg_pro2 %>% 
  unnest_tokens(trigram, Quote, token="ngrams", n=3, drop=F) %>% 
  rowwise() %>% 
  mutate(first2 = n_words(trigram, 1, 2),
         last2  = n_words(trigram, 2, 3)) %>% 
  ungroup()

gb_3gram_starts <- gb_3gram %>% 
  group_by(Quote_ID) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(Quote_start = n_words(Quote)) %>% 
  ungroup()


#Run from here

sel_g1 <- sample(list_gg, 1)
g1_0 <- gb_3gram_starts %>% 
  filter(Speaker2 == sel_g1) %>% 
  sample_n(1)

current2 <- as.character(g1_0$last2)
g1_quote <- n_words(g1_0$Quote_start, 1, 2)

continue <- TRUE
while(continue == TRUE){
  g1_1 <- gb_3gram %>% 
    #filter(Speaker2 == sel_g1) %>% 
    filter(first2 == as.character(current2)) %>% 
    mutate(last_word = n_words(last2, 2, 2)) 
  
  stoppage <- runif(1)
  
  if(nrow(g1_1) == 0) {
    g1_quote2 <- gb_sent %>% 
      filter(Speaker2 == sel_g1) %>% 
      sample_n(1)
    
    g1_quote <- g1_quote2$sentence
    
    print("cheat")
    continue <- FALSE
  } else if(current2 %in% gb_sent$last2 & stoppage > 0.8){
    g1_quote2 <- gb_sent %>% 
      filter(last2 == current2) %>% 
      sample_n(1)
    
    g1_quote <- c(head(g1_quote, length(g1_quote)-1), as.character(g1_quote2$sent2))
    
    continue <- FALSE
  } else {
    g1_1 <- g1_1 %>% 
      sample_n(1) 
    
    g1_quote <- c(g1_quote, n_words(g1_1$first2, 2, 2))
    
    current2 <- as.character(g1_1$last2)
  } 
  
}

paste0(sel_g1, ": ", firstup(paste(g1_quote, collapse = " ")))

gg_mentioned <- list_gg[sapply(tolower(list_gg), function(x){any(grepl(x, g1_quote))})]
if(length(gg_mentioned)==0){
  sel_g2 <- sample(list_gg[list_gg != sel_g1], 1)
} else{
  sel_g2 <- gg_mentioned
}
sel_g2

