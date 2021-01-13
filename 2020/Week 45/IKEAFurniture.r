#Loading packages
library(tidyverse)
library(tidytuesdayR)
library(gganimate)
library(extrafont)
font_import()
fonts()


#Getting data
ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv')

#Checking data
glimpse(ikea)


#Bar Graph
p <- ikea %>%
  ggplot(aes(x=category)) +
  geom_bar(fill="#ffdb01") +    #making bars yellow
  theme_classic() + #removing grid
  theme(text = element_text(family = "Verdana Pro", color="white"), #changing font and colour
        legend.position="none",   #removing legend
        axis.text.x= element_text(vjust = 1, hjust=1, size=14, color="white", angle=45),  #changing font size and angle
        axis.text.y= element_text(vjust = 1, hjust=1, size=14, color="white"),  #changing axis text colour
        axis.title= element_text(vjust = 1, hjust=0.5, size=14, color="white"),  #changing x axis font size
        axis.line = element_line(colour = "white", size=1.5),  #increasing line thickness
        axis.ticks = element_line(colour = "white", size=1.5), #increasing tick size and length
        axis.ticks.length=unit(.25, "cm"),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#0057a4"), #changing background colour
        plot.title = element_text(hjust = 0.5, size=30, family = "Verdana Pro Black"),  #centering title
        plot.caption = element_text(size=14, face = "italic")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +   #removing gap between bar and x axis
  ylab("Count") +    
  xlab("") + #remove axis labels
  labs(caption = "Data source: Kaggle") + #changing legend label, adding caption
  ggtitle("IKEA BAR GRAPH")

p

#Saving plot
ggsave("IKEA Bar Plot.png", height = 8, width = 11, units = "in", dpi = 300)


#Boxplot of prices
p2 <- ikea %>%
  ggplot(aes(x=category, y=price)) +
  geom_boxplot(fill="#ffdb01", color= "white") +    #making bars yellow and points white
  geom_point(position = "jitter", alpha=0.2, color= "white") + #adding transparent points with jitter
  theme_classic() + #removing grid
  theme(text = element_text(family = "Verdana Pro", color="white"), #changing font and colour
        legend.position="none",   #removing legend
        axis.text.x= element_text(vjust = 1, hjust=1, size=14, color="white", angle=45),  #changing font size and angle
        axis.text.y= element_text(vjust = 1, hjust=1, size=14, color="white"),  #changing axis text colour
        axis.title= element_text(vjust = 1, hjust=0.5, size=14, color="white"),
        axis.line = element_line(colour = "#ffdb01", size=1.5),  #increasing line thickness
        axis.ticks = element_line(colour = "#ffdb01", size=1.5), #increasing tick size and length
        axis.ticks.length=unit(.25, "cm"),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#0057a4"), #changing background colour
        plot.title = element_text(hjust = 0.5, size=30, family = "Verdana Pro Black"),  #centering title
        plot.caption = element_text(size=14, face = "italic")) +
  ylab("Price (Riyals)") +    
  xlab("") +    #remove axis label
  labs(caption = "Data source: Kaggle") + #changing legend label, adding caption
  ggtitle("IKEA BOXPLOT")

p2


#Saving plot
ggsave("IKEA Box Plot.png", height = 8, width = 11, units = "in", dpi = 300)


#Scatterplot of bookcases with item names
p3 <- ikea %>%
  filter(category == "Bookcases & shelving units") %>%  #filtering for bookcases
  ggplot(aes(x=width, y=height, label = name)) +
  geom_point(color = "white", pch=21) +   #changing  point character and colour
  geom_text(family = "Verdana Pro", color="#ffdb01", size=4, check_overlap = TRUE) +  #adding text labels, changing font, colour and size
  theme_classic() + #removing grid
  theme(text = element_text(family = "Verdana Pro", color="white"), #changing font and colour
        legend.position="none",   #removing legend
        axis.text.x= element_text(vjust = 1, hjust=1, size=14, color="white"),  #changing font size and angle
        axis.text.y= element_text(vjust = 1, hjust=1, size=14, color="white"),  #changing axis text colour
        axis.title= element_text(vjust = 1, hjust=0.5, size=14, color="white"),
        axis.line = element_line(colour = "white", size=1.5),  #increasing line thickness
        axis.ticks = element_line(colour = "white", size=1.5), #increasing tick size and length
        axis.ticks.length=unit(.25, "cm"),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#0057a4"), #changing background colour
        plot.title = element_text(hjust = 0.5, size=30, family = "Verdana Pro Black"),  #centering title
        plot.caption = element_text(size=14, face = "italic")) +
  ylab("Height (cm)") +    
  xlab("Width (cm)") +
  labs(caption = "Data source: Kaggle") + #changing legend label, adding caption
  ggtitle("IKEA Bookcases and Shelving")

p3

#Saving plot
ggsave("Bookcase Scatter.png", height = 8, width = 11, units = "in", dpi = 300)
