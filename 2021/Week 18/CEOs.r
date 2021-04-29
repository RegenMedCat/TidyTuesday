
#Loading packages
library(tidyverse)
library(extrafont)
library(magick)
library(ggimage)
font_import()
fonts()


#Getting data
departures <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')

glimpse(departures)
head(departures)


#Stacked bar plot of departures grouped by type
p <- departures %>%
  filter(departure_code!= "8", departure_code!= "9", fyear >= 1989) %>% #filtering out missing and errors, year 1990 onwards
  mutate(departure_code = case_when(departure_code== "1" ~ "Death",      #Changing labels on bars
                                    departure_code== "2" ~ "Illness",
                                    departure_code== "3" ~ "Dismissed, performance",
                                    departure_code== "4" ~ "Dismissed, legal",
                                    departure_code== "5" ~ "Retired",
                                    departure_code== "6" ~ "New job",
                                    departure_code== "7" ~ "Other",)) %>%
  ggplot(aes(x=fyear)) +
  geom_bar(aes(fill=departure_code)) +    #fill by reason for leaving
  theme_classic() + #removing grid
  theme(text = element_text(family = "HP Simplified", size=14), #changing font
        axis.title= element_text(size=18),  #changing axis font size
        plot.title = element_text(size=30, family = "HP Simplified", hjust=0.5),  
        plot.caption = element_text(size=14, face = "italic"),
        legend.position="top",
        plot.margin = unit(c(1.5,1.5,1.5,1.5), "cm")) +  #adjusting margins
  xlab("") +    
  ylab("Count") +
  scale_y_continuous(breaks =seq(0,450,50), expand = c(0, 0)) +   #removing gap between bar and x axis
  expand_limits(y = 400) +
  scale_x_continuous(breaks =seq(0,2020,5)) +  #labels every 5 years
  scale_fill_brewer(palette = "Blues", "Reasons") +
  labs(caption = "Data source: Gentry et al.") + #changing legend label, adding caption
  ggtitle("CEO Departures")

p


#Adding clipboard image as background
img = "https://raw.githubusercontent.com/RegenMedCat/TidyTuesday/master/2021/Clipboard.png"
ggbackground(p, img)


#Saving plot
ggsave("CEO.png", height = 8, width = 11, units = "in", dpi = 300)
