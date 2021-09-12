#Loading packages
library(tidyverse)
library(showtext)
library(ggstream)
library(patchwork)
library(wesanderson)


#Choosing palette
pal <- wes_palette("BottleRocket2")


#Creating custom medal colour scheme
col1 <- c("gray70", "gold3", "tan4")
col2 <- c("gold3", "gray70", "tan4")

#Adding Google fonts
font_add_google("Racing Sans One", "Racing")
font_add_google("Oswald", "oswald")


#Loading data
circuits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/circuits.csv')
constructor_results <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructor_results.csv')
constructor_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructor_standings.csv')
constructors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructors.csv')
driver_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/driver_standings.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/drivers.csv')
lap_times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/lap_times.csv')
pit_stops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/pit_stops.csv')
qualifying <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/qualifying.csv')
races <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/races.csv')
results <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/results.csv')
seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/seasons.csv')
status <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/status.csv')


driver_results_df <- driver_standings %>% 
  left_join(races, by = "raceId") %>% 
  rename(driver_url = url) %>% 
  left_join(drivers, by = "driverId")


#Checking data
glimpse(driver_results_df)


#Finding top 5 teams for points totals
d1 <- driver_results_df %>%
  group_by(nationality) %>%
  tally(points, sort = T)

d1


#Plotting stream plot of points won by Nationality
p <- driver_results_df %>%
  filter(nationality == c("British", "German", "Finnish", "Brazilian", "French")) %>%  #Filtering to top 5
  group_by(nationality, year) %>%
  tally(points) %>%  #counting points
  ggplot(aes(year, n, fill = nationality)) +
  geom_stream() +
  geom_stream_label(aes(label = nationality, family = "Racing"), size = 12, colour = "white") +
  #Adding labels, customising font
  theme_classic(base_size = 20) +  #Increasing base size
  theme(text = element_text(family="oswald", size = 42, colour = "black"),  #Using custom Google fonts
        plot.title = element_text(family = "Racing", hjust = 0.5, size = 60), #Centering title
        axis.title = element_text(family = "Racing", size = 48),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),  #Adjusting plot margins
        legend.position = "none",  #Removing legend
        panel.background = element_rect(fill = '#f0f1f2'),  #Changing background to light grey
        plot.background = element_rect(fill = '#f0f1f2')) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1950, 2021, 10)) +  #Customising axis breaks
  scale_y_continuous(limits = c(-1500, 1500), breaks = seq(-1500, 1500, 500)) +
  xlab("Year") +  #Adding labels and caption
  ylab("Points") +
  labs(title="Top 5 Points Winners by Nationality") +   #Adding plot title
  scale_fill_manual(values = pal)  #Using Wes Anderson palette
  
p


#Lollipop chart of top 3 winning drivers
p2 <- driver_results_df %>%
  group_by(surname) %>%
  tally(wins, sort = T) %>%
  top_n(3) %>%
  arrange(n) %>%
  mutate(surname = factor(surname, levels = c("Schumacher", "Hamilton", "Vettel"))) %>%
  ggplot(aes(surname, n, colour = surname, label = n)) +
  geom_segment(aes(y = 0, x = surname,
                   yend = n, xend = surname),
                   color = "black") +
  geom_point(stat='identity', size=20)  +  #Adding points, customising colour and size
  geom_text(color="black", size = 16, family = "Racing") +  #Adding text to lollipops
  theme_classic(base_size = 20) +  #Increasing base size
  theme(text = element_text(family="oswald", size = 42, colour = "black"),  #Using custom Google fonts
        plot.title = element_text(family = "Racing", hjust = 0.5, vjust=-3, size = 60), #Adjusting title position
        plot.margin = margin(-1, 0.5, 0.5, 0.5, "cm"),  #Adjusting plot margins
        axis.title = element_text(family = "Racing", size = 48),
        legend.position = "none",  #Removing legend
        panel.background = element_rect(fill = '#f0f1f2'),  #Changing background to light grey
        plot.background = element_rect(fill = '#f0f1f2')) +
  scale_y_continuous(limits = c(0, 1250), breaks = seq(0, 1250, 250), expand = c(0, 0)) +
  xlab("") +  #Adding/removing labels
  ylab("Wins") +
  labs(title="Top 3 Winning Drivers") +  #Adding plot title
  scale_colour_manual(values = col1)  #Using custom palette

p2


#Line graph of top 3 winning drivers
p3 <- driver_results_df %>%
  filter(surname == c("Schumacher", "Hamilton", "Vettel"), year > 1990) %>%
  group_by(surname, year) %>%
  tally(wins, sort = T) %>%
  ggplot(aes(year, n, colour = surname)) +
  geom_line() +
  geom_point() +
  theme_classic(base_size = 20) +  #Increasing base size
  theme(text = element_text(family="oswald", size = 42, colour = "black"),  #Using custom Google fonts
        plot.title = element_text(family = "Racing", hjust = 0.5, vjust=-3, size = 60), #Adjusting title position
        axis.title = element_text(family = "Racing", size = 48),
        plot.margin = margin(-1, 0.5, 0.5, 0.5, "cm"),  #Adjusting plot margins
        legend.title=element_blank(),  #Removing legend title,
        legend.position = c(0.2, 0.90), #Customising legend position
        legend.key = element_rect(fill = "#f0f1f2", colour = "#f0f1f2"), #Changing legend fill
        legend.background = element_rect(fill = "#f0f1f2"),
        panel.background = element_rect(fill = '#f0f1f2'),
        plot.background = element_rect(fill = '#f0f1f2')) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 10)) +
  xlab("Year") +  #Adding labels
  ylab("Wins") +
  labs(title="Top 3 Winning Drivers") +  #Adding plot title
  labs(caption = "Data source: Ergast API") +   #Adding caption
  scale_colour_manual(values = col2)  #Using custom palette

p3



#Arranging plots using patchwork
p /
  (p2 | p3)


#Saving plots
ggsave("F1.png", height = 10, width = 10, units = "in", dpi = 300)
