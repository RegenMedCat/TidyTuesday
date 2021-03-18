#Loading packages
library(tidyverse)
library(lubridate)
library(tidytuesdayR)
library(ggimage)
library(grid)
library(png)
library(shadowtext)
library(extrafont)
font_import()
fonts()


#Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 12)

games <- tuesdata$games

glimpse(games)


#Simple line graph of Stardew Valley players over time
p <- games %>%
  filter(gamename == "Stardew Valley") %>%  #filtering to only Stardew Valley
  mutate(date = dmy(paste("01", month, year, sep = "-"))) %>%   #making date month, year
  ggplot(aes(x=date, y=avg)) +
  geom_line() +
  geom_point() +
  theme_classic() + #removing grid
  theme(text = element_text(family = "Book Antiqua"), #changing font
        axis.title= element_text(size=16),  #changing axis font size
        plot.title = element_text(size=30, family = "Bodoni MT", hjust=0.5),  
        plot.caption = element_text(size=12, face = "italic")) +
  xlab("Date") +    
  ylab("Average number of players at the same time") + 
  labs(caption = "Data source: Steam") + #changing legend label, adding caption
  ggtitle("Stardew Valley Players")

p

#Plotting chickens as points
#Borrowed code from https://buzzrbeeline.blog/2018/06/13/fun-and-easy-r-graphs-with-images/
#and https://themockup.blog/posts/2020-10-11-embedding-images-in-ggplot/

image <- readPNG("Chicken.png") #loading image
games$image <- "Chicken.png"  #adding image to dataframe

#Defining aspect ratio
asp_ratio <- 1


p2 <- games %>%
  filter(gamename == "Stardew Valley") %>%  #filtering to only Stardew Valley
  mutate(date = dmy(paste("01", month, year, sep = "-"))) %>%   #making date month, year
  ggplot(aes(x=date, y=avg)) +
  geom_line(lwd=1) +
  geom_point() +
  geom_image(aes(x=date, y=avg, image=image, asp = asp_ratio), size=0.04) +  #adjusting size of images
  theme_classic() + #removing grid
  theme(text = element_text(family = "Book Antiqua"), #changing font
        axis.title= element_text(size=18),  #changing axis font size
        axis.text.x = element_shadowtext(colour = "white", size=14),   #used shadowtext to make more legible on blue background
        axis.text.y = element_shadowtext(colour = "white", size=14, angle=90, vjust= -1, hjust=0.5),
        plot.title = element_shadowtext(colour = "white", size=24, hjust=0.5),  
        plot.caption = element_shadowtext(colour = "white", size=12, face = "italic"),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 1)) +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +  #adding o.5 cm margins to plot as stuff was being cut off
  xlab("") +   #removing axis labels 
  ylab("") + 
  labs(caption = "Data source: Steam") + #changing legend label, adding caption
  ggtitle("Average Number of Stardew Valley Players at One Time")

p2


#Adding cloud background image
img = "stardew.png"
ggbackground(p2, img)


ggsave("stardew Plot.png", dpi = 300, height = 8, width = 10 * asp_ratio)
