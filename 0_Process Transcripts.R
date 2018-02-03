####
# Process transcripts
####

# Transcripts from https://www.springfieldspringfield.co.uk/episode_scripts.php?tv-show=the-golden-girls-1985
# Dont' have speaker

library(tidyverse); library(tidytext)

load("data/GG_Transcripts")

gt_raw <- bind_rows(gg_transcripts) %>% 
  mutate(Detail = c("Title", "Script")) %>% 
  select(Detail, everything()) %>% 
  gather(Episode, value, 2:ncol(.)) %>% 
  spread(Detail, value)

gt_pro <- gt_raw %>% 
  filter(!grepl("(2)", Title, fixed=T)) %>% 
  mutate(Script = gsub("\n", "", Script, fixed=T),
         Script = gsub("\t", "", Script, fixed=T),
         Script = gsub('\"', "", Script, fixed=T),
         Script = gsub("^\\s+", " ", Script),
         Script = gsub(" - ", " ", Script, fixed=T)) %>% 
  mutate(
    Script = gsub("Thank you for being a friend", "", Script, fixed=T),
    Script = gsub("Thank you for bein' a friend", "", Script, fixed=T),
    Script = gsub("Traveled down the road and back again", "", Script, fixed=T),
    Script = gsub("Your heart is true", "", Script, fixed=T),
    Script = gsub("You're a pal and a confidante", "", Script, fixed=T),
    Script = gsub("You're a pal and a confidant", "", Script, fixed=T),
    Script = gsub("And if you threw a party", "", Script, fixed=T),
    Script = gsub("Invited everyone you knew", "", Script, fixed=T),
    Script = gsub("You would see the biggest gift would be from me", "", Script, fixed=T),
    Script = gsub("You would see", "", Script, fixed=T),
    Script = gsub("The biggest gift would be from me", "", Script, fixed=T),
    Script = gsub("And the card attached would say", "", Script, fixed=T),
    Script = gsub("(music)", "", Script, fixed=T)
  ) 

gt_pro2 <- gt_pro %>% 
  unnest_tokens(Line, Script, token = "sentences", to_lower=F) %>% 
  group_by(Episode) %>% 
  mutate(Line_number = row_number()) %>% 
  mutate(Line_type = case_when(
    grepl("?", Line, fixed=T) ~ "Question",
    grepl("?", lag(Line, 1), fixed=T) ~ "Response",
    TRUE ~ "General"
  )) %>% 
  ungroup() %>% 
  mutate(Season = as.numeric(substr(Episode, 1, 1)))
  

#Save

saveRDS(gt_pro2, "data/Processed_Transcripts.RDS")
  