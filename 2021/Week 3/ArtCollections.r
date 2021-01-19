
#Loading packages
library(tidyverse)
library(tidytuesdayR)
library(gganimate)
library(extrafont)
font_import()
fonts()


# Get the Data
artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")


#Bar Graph of medium of artwork
p <- artwork %>%
  mutate(medium = fct_lump(medium, 10)) %>% #filtering to top 10
  mutate(decade = floor(acquisitionYear/10)*10) %>%   #sorting into decades
  filter(!is.na(medium), medium!= "Other", !is.na(acquisitionYear)) %>% #removing Other and NAs
  group_by(decade) %>% 
  ggplot(aes(x=medium)) +
  geom_bar(fill="#9EC1A3") +    #changing bar colour
  theme_classic() + #removing grid
  theme(text = element_text(family = "Bodoni MT Condensed", color="white"), #changing font and colour
        legend.position="none",   #removing legend
        axis.text.x= element_text(vjust = 1, hjust=1, size=16, color="white"),  #changing font size and angle
        axis.text.y= element_text(vjust = 1, hjust=1, size=16, color="white"),  #changing axis text colour
        axis.title= element_text(vjust = 1, hjust=0.5, size=16, color="white"),  #changing x axis font size
        axis.line = element_line(colour = "white", size=1),  #increasing line thickness
        axis.ticks = element_line(colour = "white", size=1), #increasing tick size and length
        axis.ticks.length=unit(.25, "cm"),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#304040"), #changing background colour
        plot.title = element_text(size=30, family = "Bodoni MT"),  
        plot.caption = element_text(size=12, face = "italic")) +
  coord_flip() +  #flipping axes
  scale_y_continuous(expand = expansion(mult = c(0,0))) +   #removing gap between bar and x axis
  ylab("Pieces of Artwork") +    
  xlab("") + #remove axis labels
  labs(caption = "Data source: Tate Art Museum") + #changing legend label, adding caption
  ggtitle("Artwork acquired by the Tate")

p
  

#Animating plot to show count by acquisition year
p2<- p +  
  aes(group = medium) +
  geom_text(x = 1.1, y = 20000,  
            family = "Bodoni MT Condensed",  
            aes(label = as.character(decade)),  
            size = 20, col = "white") +  
  transition_states(decade, transition_length = 4, state_length = 2)

p2

animate(p2, fps=5, height=600, width=1200, res=100)

anim_save("Tate.gif")
