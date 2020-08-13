#Loading packages
library(tidyverse)
library(gganimate)
library(appa)
library(extrafont)  #I installed Herculanum from https://best-font.com/fonts/download-herculanum-font.html
font_import()
loadfonts(device="win")
loadfonts(device="postscript")
fonts()


#Get data
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load(2020, week = 33)

#Cleaned script as in readme


#Static line graph of IMBDB rating by chapter
p <- avatar %>%
  ggplot(aes(x=chapter_num, y=imdb_rating, color=book)) +
  geom_line() +
  theme_classic() + #removing grid
  theme(text = element_text(family = "Herculanum", size=15), #changing font
        legend.position="top", legend.text=element_text(size=14), #changing legend position and font
        axis.text.x= element_text(vjust = 1, hjust=1),  #changing axis text colour
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#fcfffa"), #changing background colour
        legend.background = element_rect(fill = "#fcfffa"),  #changing legend background colour
        plot.title = element_text(hjust = 0.5),  #centering title
        plot.caption = element_text(size=10, face = "italic")) +
  ylab("IMDB Ratings") +
  xlab("") +
  scale_x_continuous(name="Chapter Number") + #customising x axis breaks
  scale_color_manual(values=c("dodgerblue2", "orange", "red2")) +
  labs(color = "Book") +  #changing legend title
  labs(caption = "Data source: Avatar Wiki") + #changing legend label, adding caption
  ggtitle("Avatar: The Last Airbender Ratings")

p


#Using gganimate to slowly reveal data and keeping points
p2<- p + geom_point(aes(group = seq_along(chapter_num))) +
transition_reveal(chapter_num)
p2

#Animating, modifying size and resolution
animate(p2, fps=5, height=600, width=1200, res=300)

#Saving GIF
anim_save("avatarratings.gif")
