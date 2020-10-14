#Packages
library(tidyverse)
library(reshape2)
library(extrafont)
font_import()
loadfonts(device="win")


#Loading data
census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/census.csv')


#Creating colour scheme
Mycolours <- c("tomato4", "lightsalmon1", "goldenrod2", "lemonchiffon2")


#Looking at census data
glimpse(census)


#Line graph of slaves over time grouped by region
census %>%
  filter(region %in% c("Midwest", "Northeast", "Northwest", "South", "West")) %>%  #filtering out USA total
  group_by(year, region) %>%
  summarise(y=sum(black_slaves)) %>%  #grouping and summing no. of black slaves for each region to avoid multiple points for each division
  ggplot(aes(x=year, y=y/10^6, group=region, color=region)) + #expressing in millions
  geom_line(lwd=1) +  #increasing line width
  geom_point(size=3) +  #increasing point size
  theme(legend.position="right", text = element_text(family = "Georgia", size=14, color="white"),   #changing font
        legend.key=element_blank(),    #removing fill on legend key
        legend.background = element_rect(fill = "gray5"),
        legend.title = element_blank(),     #removing legend title
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),   #removing grid and fill
        plot.title = element_text(hjust = 0.5),  #centering graph title
        plot.background = element_rect(fill = "gray5"), #changing background colour
        panel.background = element_blank(),
        axis.line = element_line(colour = "white"),     #changing axis line and tick colours
        axis.ticks = element_line(colour = "white"),
        axis.text.x= element_text(color="white"),     #changing axis text to white
        axis.text.y= element_text(color="white")) +
  ylab("Number of Black Slaves (Millions)") +
  xlab("Census Year") +
  scale_x_continuous(breaks=seq(1790,1870,10)) +  #changing intervals on x axis
  scale_color_manual(values=Mycolours) +   #adding custom colour scheme
  ggtitle("Slavery in the US") -> fig1  #adding title
  fig1

#Saving plot
ggsave("Slavery in the US.png", fig1, height = 6, width = 11, units = "in", dpi = 300)


#Bar graph of free and enslaved popluation
#using reshape2 package to melt and alow plotting of two columns
census_melt <- melt(census[,c("year", "black_slaves", "black_free")],id.vars = 1)

census_melt %>%
  ggplot(aes(x=year, y=value/10^6)) +   #expressing in millions
  geom_bar(aes(fill = variable),stat = "identity", position = "stack") +  #filling by variable and stacking bars
  theme(legend.position="right", text = element_text(family = "Georgia", size=14, color="white"),   #changing font
        legend.background = element_rect(fill = "gray12"),
        legend.key = element_rect(color="gray12", size = 0.8),   #removing annoying white border around legend colours
        legend.title = element_blank(),  #removing legend title
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),   #removing grid and fill
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),  #centering graph title
        plot.background = element_rect(fill = "gray12"), #changing background colour
        axis.line = element_line(colour = "white"),     #changing axis line and tick colours
        axis.ticks = element_line(colour = "white"),
        axis.text.x= element_text(color="white"),     #changing axis text to white
        axis.text.y= element_text(color="white")) +
  ylab("Population (Millions)") +
  xlab("Census Year") +
  scale_fill_manual(values=c("tomato2","goldenrod2"), labels=c("Slaves","Free")) +  #adding custom colour scheme
  ggtitle("Black Population in the US 1790-1870") -> fig2  #adding title
  fig2

#Saving plot
ggsave("Slavery in the US Bar.png", fig2, height = 6, width = 11, units = "in", dpi = 300)
