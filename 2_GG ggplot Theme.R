####
# Develop a ggplot theme with GG colors
####

library(ggthemr)

tgg_colors <- c("#9affd0", #Aqua
                "#ffb5f5", #Pink
                "#5384ff", #Blue
                "#ff9e53", #Orange
                "#ffed89", #Yellow
                "#de89ff", #Purple
                "#ff6141", #Red/Orange
                
                "#ff25ab" #Bright pink
                
                )

tgg_background <- "#fcedcc" #Tan

# remove previous effects:
ggthemr_reset()
# Define colours for your figures with define_palette
tgg_theme <- define_palette(
  swatch = c("#eeeeee", tgg_colors), # colours for plotting points and bars
  gradient = c(lower = tgg_colors[2], upper = tgg_colors[1]), #upper and lower colours for continuous colours
  background = tgg_background#defining a grey-ish background 
)

ggthemr(tgg_theme, type="outer")

gg_trans_address4 +
  theme(
    plot.title = element_text(face = "italic")
  )

example_plot <- ggplot(diamonds, aes(price, fill = factor(cut))) +
  geom_histogram(binwidth = 850) +
  xlab('Price (USD)') +
  ylab('Count') +
  scale_x_continuous(label = function(x) paste0(x / 1000, 'k')) +
  #theme(legend.position = 'none') +
  scale_y_continuous(label = function(x) format(x, big.mark = ",", scientific = FALSE))

example_plot

ggplot(mtcars, aes(wt, mpg)) + geom_point(aes(colour=factor(cyl), size = qsec)) +
  theme(
    plot.background = element_rect(fill = tgg_colors[1])
  )
