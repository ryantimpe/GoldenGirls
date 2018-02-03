library(tidytext); library(tidyverse)

gg_verbs <- parts_of_speech %>% 
  filter(pos == "Verb (transitive)") %>% 
  rowwise() %>% 
  mutate(drop = grepl("\\b(\\w+ing)\\b", word) | grepl("\\b(\\w+ed)\\b", word)) %>% 
  ungroup() %>% 
  filter(!drop) %>% 
  inner_join(gb_words %>% select(word) %>% distinct())

gg_nouns <- parts_of_speech %>% 
  filter(pos == "Noun") %>% 
  inner_join(gb_words %>% select(word) %>% distinct())


list_gg <- c("Blanche", "Dorothy", "Sophia", "Rose")
sel_g1 <- sample(list_gg, 1)
sel_g2 <- sample(list_gg[list_gg != sel_g1], 1)

dir.multi <- runif(1) > .5

paste(sel_g1,
      ifelse(dir.multi, paste("and", sel_g2), ""),
      ifelse(dir.multi, sample(gg_verbs$word, 1), paste0(sample(gg_verbs$word, 1), "s")),
      "a", sample(gg_nouns$word, 1)
      )

gg_directions <- gg_pro2 %>% 
  filter(!is.na(Direction)) %>% 
  sample_n(1) %>% 
  pull(Direction)

gg_directions
