#Loading packages
library(tidyverse)
library(ggstream)
library(showtext)
library(rvest)
library(png)
library(cowplot)
library(magick)


#Adding Google fonts
font_add_google("Bungee", "Bungee")
font_add_google("Oswald", "Oswald")
showtext_auto()


#Loading data
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/episodes.csv')



#Plotting stream plot of views over episode number for actors' debut seasons
p <- episodes %>%
  select(season_number, uk_viewers, episode_number)  %>%
  mutate(season_number = replace(season_number, season_number == "1", "Eccleston"),
         season_number = replace(season_number, season_number == "2", "Tennant"),
         season_number = replace(season_number, season_number == "5", "Smith"),
         season_number = replace(season_number, season_number == "8", "Capaldi"),
         season_number = replace(season_number, season_number == "11", "Whittaker"))  %>%  #renaming debut seasons
  filter(!is.na(episode_number), !is.na(uk_viewers),
         season_number != "3", season_number != "4", season_number != "6", season_number != "7", season_number != "9",
         season_number != "10", season_number != "12", season_number != "13") %>% #filtering out unwanted seasons (messy but only thing I could get to work)
    ggplot(aes(episode_number, uk_viewers, fill = season_number)) +
    geom_stream() +
    geom_stream_label(aes(label = season_number, family = "Bungee"), size = 14, colour = "white") +
    #Adding labels, customising font
    theme_classic(base_size = 40) +  #Increasing base size
    theme(text = element_text(family="Oswald", face="bold", size = 60, colour = "white"),  #Using custom Google fonts
          axis.text.x = element_text(colour = "white"),
          axis.text.y = element_text(colour = "white"),
          plot.title = element_text(hjust = 0.5, size = 80), #Centering title
          axis.title = element_text(size = 68),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),  #Adjusting plot margins
          legend.position = "none",  #Removing legend
          panel.background = element_rect(fill = '#102372'),  #Changing background to tardis blue
          plot.background = element_rect(fill = '#102372'),
          axis.line = element_line(colour = "white"),
          axis.ticks = element_line(colour = "white")) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 13), breaks = seq(0, 13, 1)) +  #Customising axis breaks
    xlab("Episode Number") +  #Adding labels and caption
    ylab("UK Viewers") +
    labs(caption = "Data source: datardis") +   #Adding caption
    labs(title="New Who Doctor Debut Seasons") +  #Adding plot title
    scale_fill_manual(values = c("#230741", "#d6b035", "#a22b47", "#63b0e6", "#c5a8a0"))  #Using custom fill colours

p

#Saving plot
ggsave("DrWho.png", height = 8, width = 10, units = "in", dpi = 300)


#Importing TARDIS image
image <- image_read("tardis.png") %>%
  image_resize("400x400")
  

#Reading in saved plot
pub_plot <- image_read("DrWho.png")


#Adding tardis to plot
pub_plot %>% 
  image_composite(image, offset = "+2550+400")
