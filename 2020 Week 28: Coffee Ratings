#Loading packages
library(tidyverse)
library(plotly)
library(janitor)
library(skimr)
library(extrafont)
font_import()
loadfonts(device="win")


#Get data
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load(2020, week = 28)

#Cleaned script as in readme


#Violin plot, grouped by processing method
all_ratings %>%
  filter(total_cup_points>0 & !is.na(processing_method)) %>%   #removing 0s and NAs
  mutate(processing_method = fct_relevel(processing_method, "Pulped natural / honey", 
                                                             "Natural / Dry", 
                                                             "Semi-washed / Semi-pulped", 
                                                             "Washed / Wet",
                                                             "Other")) %>%    #reordering groups
  ggplot(aes(x=processing_method, y=total_cup_points, fill=processing_method)) +
  geom_violin() +
  geom_boxplot(width=0.1, color="white") + #overlaying boxplot and colouring white
  theme_classic() + #removing grid
  theme(text = element_text(family = "Modern No. 20", size=14, color="white"), #changing font
        legend.position="none", legend.text=element_text(size=12), #changing legend position and font
        axis.text.x= element_text(color="white", angle = 45, vjust = 1, hjust=1),  #changing axis text colour and angle
        axis.text.y= element_text(color="white"),  #making y axis text white
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#200d00"), #changing background colour
        legend.background = element_rect(fill = "#200d00"),  #changing legend background colour
        plot.title = element_text(hjust = 0.5),  #centering title
        plot.caption = element_text(size=10, face = "italic"),
        axis.line = element_line(colour = "white"),
        axis.ticks = element_line(colour = "white")) +
  ylab("Total Cup Points") +
  xlab("") +
  labs(caption = "Data source: Coffee Quality Database") + #changing legend label, adding caption
  ggtitle("Coffee Quality by Processing Method") +
scale_fill_manual(values=c("#E5D3B3", "#D2B48C", "#B99976", "#987554", "#664229"))

#Saving plot
ggsave("Violin Plot Processing.png", height = 6, width = 11, units = "in", dpi = 300)


#Scatter, grouped by bean colour
all_ratings %>%
  filter(total_cup_points>0 & !is.na(color)) %>%   #removing 0s and NAs
  ggplot(aes(x=total_cup_points, y=color, color=color)) +
  geom_point() +
  theme_classic() + #removing grid
  theme(text = element_text(family = "Modern No. 20", size=14, color="white"), #changing font
        legend.position="none", legend.text=element_text(size=12), #changing legend position and font
        axis.text.x= element_text(color="white", vjust = 1, hjust=1),  #changing axis text colour
        axis.text.y= element_text(color="white"),  #making y axis text white
        axis.ticks.y = element_blank(),   #removing ticks on y axis
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#200d00"), #changing background colour
        legend.background = element_rect(fill = "#200d00"),  #changing legend background colour
        plot.title = element_text(hjust = 0.5),  #centering title
        plot.caption = element_text(size=10, face = "italic"),
        axis.line = element_line(colour = "white"),
        axis.ticks = element_line(colour = "white")) +
  scale_x_continuous(breaks=seq(0,100,5)) + #changing interval on y axis
  ylab("") +
  xlab("Total Cup Points") +
  scale_color_manual(values=c("#2ec0d1", "#29d68e", "#42e718", "#a6b0b0")) +
  labs(caption = "Data source: Coffee Quality Database") + #changing legend label, adding caption
  ggtitle("Coffee Quality by Bean Colour")

#Saving plot
ggsave("Scatter Plot Colour.png", height = 6, width = 11, units = "in", dpi = 300)
