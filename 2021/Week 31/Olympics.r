#Loading packages
library(tidyverse)
library(ggpubr)
library(extrafont)
font_import()
fonts()

#Loading data
olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

#Checking data
glimpse(olympics)

#Creating custom colour schemes
col1 <- c("tan4", "gold3", "gray80")
col2 <- c("#fbb32c", "#ef5c70", "#eb334c", "#040404", "#0484cb")


#Creating dataframe of medal counts in archery grouped by team
dat <- olympics %>%
        filter(sport == "Archery") %>%
        group_by(team) %>%
        count(medal) %>%
        mutate(freq = n / sum(n)*100) #calculating percentages

dat


#Plotting donut chart of South Korea's medals
p <- dat %>%
      filter(team == "South Korea") %>%
      ggdonutchart("freq", label = "medal", fill="medal") +
      theme_void() +
      theme(legend.position = "none",
            plot.title = element_text(family="Lato", hjust=0.5, vjust=-48, size=14), #positioning  title
            plot.margin = margin(-3, -3, -3, -3, "cm")) + #adjusting margins
      xlim(c(-2, 4)) +  #making donut thinner
      scale_fill_manual(values = col1) +
      ggtitle("South Korea")

p


#Plotting donut chart of Belgium's medals
p2 <- dat %>%
  filter(team == "Belgium") %>%
  ggdonutchart("freq", label = "medal", fill="medal") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(family="Lato", hjust=0.5, vjust=-48, size=14),
        plot.margin = margin(-3, -3, -3, -3, "cm")) +
  xlim(c(-2, 4)) +
  scale_fill_manual(values = col1) +
  ggtitle("Belgium")

p2


#Plotting donut chart of France's medals
p3 <- dat %>%
  filter(team == "France") %>%
  ggdonutchart("freq", label = "medal", fill="medal") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(family="Lato", hjust=0.5, vjust=-48, size=14),
        plot.margin = margin(-3, -3, -3, -3, "cm")) +
  xlim(c(-2, 4)) +
  scale_fill_manual(values = col1) +
  ggtitle("France")

p3


#Plotting donut chart of USA's medals
p4 <- dat %>%
  filter(team == "United States") %>%
  ggdonutchart("freq", label = "medal", fill="medal") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(family="Lato", hjust=0.5, vjust=-107, size=14),
        plot.margin = margin(-7, -3.5, 0, 0, "cm")) +
  xlim(c(-2, 4)) +
  scale_fill_manual(values = col1) +
  ggtitle("USA")

p4


#Plotting donut chart of China's medals
p5 <- dat %>%
  filter(team == "China") %>%
  ggdonutchart("freq", label = "medal", fill="medal") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(family="Lato", hjust=0.5, vjust=-107, size=14),
        plot.margin = margin(-7, 0, 0, -3.5, "cm")) +
  xlim(c(-2, 4)) + 
  scale_fill_manual(values = col1) +
  ggtitle("China")

p5


#Arranging graphs
a1 <- ggarrange(p, p2, p3, ncol = 3)
a2 <- ggarrange(p4, p5)

a1
a2

rings <- ggarrange(a1, a2, ncol = 1, nrow = 2)
annotate_figure(rings,
                bottom = text_grob("Data source: Kaggle",
                                   x=0.92, y=1, family="Lato", face = "italic", size = 12),
                top = text_grob("Olympic Medals in Archery 1920-2016",
                                   x=0.5, y=-16, family="Lato Semibold", size = 20))

#Saving plot
ggsave("Rings.png", height = 6, width = 10, units = "in", dpi = 300)



#Plotting line graph of cumulative medal count
p6 <- olympics %>%
  filter(team %in% c("South Korea", "Belgium", "France", "United States", "China")) %>%  #filtering to 5 teams
  group_by(team) %>%
  ggplot(aes(x=year, y=n, color=team)) +
  geom_step(aes(y=..y..),stat="ecdf", size=1.5) + #making cumulative
    theme_classic() + #removing grid
    theme(text = element_text(family = "Lato", size=14), #changing font and size
          plot.title = element_text(family="Lato Semibold", hjust=0.5, size=18), #centring title, changing font
          plot.caption = element_text(family = "Lato", size=12, face = "italic"), #custom caption text
          legend.position = "top",  #positioning legend below title 
          legend.title = element_blank()) + #removing legend title
    theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +  #adding 0.5 cm margins to plot as stuff was being cut off
  scale_y_continuous(expand = expansion(mult = c(0,0))) +  #removing gap between bar and x axis
  scale_colour_manual(values = col2) +
  ylab("Cumulative Frequency") +    
  xlab("Count") + 
  ggtitle("Olympic Medals in Archery Over Time") +
  labs(caption = "Data source: Kaggle")

p6


#Saving plot
ggsave("Medal Frequency.png", height = 6, width = 10, units = "in", dpi = 300)
