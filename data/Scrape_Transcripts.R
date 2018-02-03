library(rvest)

list_seasons <- 1:7
list_episodes <- 1:26

gg_transcripts <- list()

for(seas in list_seasons){
  for(episode in list_episodes){
    
    #There's no #26 in a few seasons
    if(seas %in% c(1, 3, 4, 5, 7) & episode == 26){next}
    
    #Prepare episode character
    epi <- ifelse(episode < 10, paste0("0", episode), as.character(episode))
    
    #Url
    url_base <- "https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=the-golden-girls-1985&episode=s0"
    
    url <- paste0(url_base, seas, "e", epi)
    
    #Get HTML
    episode_data <- read_html(url)
    
    #Title
    episode_title <- episode_data %>% 
      html_nodes("h3") %>% 
      html_text()
    
    #Script
    episode_script <- episode_data %>% 
      html_nodes(".scrolling-script-container") %>% 
      html_text()
    
    #Save it
    gg_transcripts[[paste0(seas, ".", epi)]][["title"]] <- episode_title
    gg_transcripts[[paste0(seas, ".", epi)]][["script"]] <- episode_script
  }
}

###
#Manual fixes
####

# #Url
# url_base <- "https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=the-golden-girls-1985&episode=s0"
# 
# #1.12
# url <- paste0(url_base, 1, "e", 12)
# episode_data <- read_html(url)
# 
# #Script
# episode_script <- episode_data %>% 
#   html_nodes(".scrolling-script-container") %>% 
#   html_text()
# 
# gg_transcripts[[paste0(1, ".", 12)]][["script"]] <- episode_script

save(gg_transcripts, file="data/GG_Transcripts")
