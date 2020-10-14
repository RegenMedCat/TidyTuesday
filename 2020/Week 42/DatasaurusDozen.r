#Loading packages
library(tidyverse)
library(tidytuesdayR)
library(gganimate)
library(extrafont)
font_import()
fonts()


#Getting data
tuesdata <- tidytuesdayR::tt_load(2020, week = 42)

datasaurus <- tuesdata$datasaurus


#Checking data
glimpse(datasaurus)


#Scatter plot of dino
p <- datasaurus %>%
  filter(dataset == "dino") %>%
  ggplot(aes(x=x, y=y, color=dataset)) +
  geom_point(size=4) +
  theme_classic() + #removing grid
  theme(text = element_text(family = "Elephant", size=16, color="white"), #changing font and colour
        legend.position="none",   #removing legend
        axis.text.x= element_text(vjust = 1, hjust=1, color="#e40006"),  #changing axis text colour
        axis.text.y= element_text(vjust = 1, hjust=1, color="#e40006"),  #changing axis text colour
        axis.line = element_line(colour = "#ffe900", size=1.2),  #increased line thickness
        axis.ticks = element_line(colour = "#ffe900", size=1.2),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "black"), #changing background colour
        plot.title = element_text(hjust = 0.5),  #centering title
        plot.caption = element_text(size=10, face = "italic")) +
  scale_color_manual(values="white") +
  ylab("") +    #removing axis labels
  xlab("") +
  labs(caption = "Data source: Datasaurus Dozen") + #changing legend label, adding caption
  ggtitle("RAWRR!")

p


#Using gganimate to reveal points
p2<- p + geom_point(aes(group = seq_along(x))) +
  transition_reveal(x)
p2

animate(p2, fps=20, height=550, width=550, res=100)

#saving gif
anim_save("dino.gif")

