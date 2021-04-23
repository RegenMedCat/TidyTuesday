#Loading packages
library(tidyverse)
library(treemapify)
library(ggpubr)
library(shadowtext)
library(extrafont)
font_import()
fonts()


#Getting the data
netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')

#Checking data
glimpse(netflix_titles)


#Making custom colour scheme
reds <- c("#9B060E", "#B40710", "#CC0812", "#E50914", "#F6121D", "#F72a35")


#Histogram of number of TV show releases from top 5 countries
p <- netflix_titles %>%
    filter(type == "TV Show", !is.na(country), release_year >= 1999) %>%  #filtering to only TV shows, removing NAs, year 2000 onwards
    mutate(country = fct_lump(country, 4)) %>% #filtering to top 4
    ggplot(aes(x=release_year)) +
    geom_histogram(aes(fill=country), binwidth = 1) +
    theme_classic() + #removing grid
    theme(text = element_text(family = "Gill Sans MT Condensed", color="white", size=18), #changing font and colour
          axis.text.x=element_text(colour="white"),
          axis.text.y=element_text(colour="white"),
          axis.ticks = element_blank(), #remove axis ticks
          plot.title = element_shadowtext(size=24, hjust=0.5, color="#E50914"), #making title outlined red, centred
          legend.text=element_text(color="white", size=10),
          legend.title = element_blank(),   #removing legend title 
          axis.line = element_line(colour = "white"),  #making axis lines white
          plot.background = element_rect(fill = "#564D4D"),  #changing background to grey
          panel.background = element_rect(fill = "transparent",colour = NA),  #removing background on plot and legend
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA),
          legend.position = c(0.2, 0.8)) +  #custom position below title
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +  #adding o.5 cm margins to plot as stuff was being cut off  
  scale_y_continuous(expand = expansion(mult = c(0,0))) +   #removing gap between bar and x axis
    scale_fill_manual(values = reds) +
    xlab("Year Released") +    
    ylab("Count") + 
    ggtitle("NETFLIX TV Shows: Top 5")

p


#Line graph of number of TV show releases per year
p2 <- netflix_titles %>%
    filter(type == "TV Show", release_year >= 1999) %>%  #filtering to only TV shows, removing NAs, year 2000 onwards
    ggplot(aes(x=release_year)) +
    geom_line(stat = "count", color="white", lwd=1) +
    geom_point(stat = "count", color="white", size=1.5) +
    theme_classic() + #removing grid
    theme(text = element_text(family = "Gill Sans MT Condensed", color="white", size=18), #changing font and colour
          axis.text.x=element_text(colour="white"),
          axis.text.y=element_text(colour="white"),
          axis.ticks = element_blank(), #remove axis ticks
          plot.title = element_shadowtext(size=24, hjust=0.5, color="#E50914"), #making title outlined red, centred
          axis.line = element_line(colour = "white"),  #making axis lines white
          plot.background = element_rect(fill = "#564D4D"),  #changing background to grey
          panel.background = element_rect(fill = "#564D4D", colour = NA)) +  #removing background on plot
    theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +  #adding o.5 cm margins to plot as stuff was being cut off
    xlab("Year Released") +    
    ylab("Count") + 
    ggtitle("NETFLIX TV Shows: Total")
  
p2


#Tree plot of TV show genres

#Creating df with counts for genre
dat <- netflix_titles %>%
        group_by(listed_in) %>%
        filter(type == "TV Show", release_year >= 1999) %>%  #filtering to only TV shows, removing NAs, year 2000 onwards
        tally()


p3 <- dat %>%
  ggplot(aes(area = n, label = listed_in)) +
  geom_treemap(fill="#E50914", lwd=2) +   #changing fill, increasing line thickness
  geom_treemap_text(family = "Gill Sans MT Condensed", colour = "white", place = "centre",
                    grow = TRUE) +  #changing font, colour and centring
  theme(plot.background = element_rect(fill = "#564D4D"),  #changing background colour
        plot.caption = element_text(family = "Gill Sans MT Condensed", size=16, face = "italic", color="white"), #custom caption text
        plot.margin = unit(c(1,1,1,1), "cm")) +
  labs(caption = "Data source: Kaggle")

p3


#Arranging graphs
#There's maybe a tidier way to do this but when I tried to do it in one step, I couldn't get the second row to change to one column
a1 <- ggarrange(p, p2, ncol = 2)  #histogram and line graph on top
a2 <- ggarrange(p3)  #second row treeplot

ggarrange(a1, a2, ncol = 1, nrow = 2)     


#Saving plot
ggsave("Netflix.png", height = 8, width = 11, units = "in", dpi = 300)
