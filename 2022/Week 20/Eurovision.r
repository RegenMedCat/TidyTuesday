#Loading packages
library(tidyverse)
library(showtext)


font_add_google("Covered By Your Grace", "Grace")


#Automatically uses showtext to render text
showtext_auto()


#Importing data
dat <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')

#Plotting scatter plot
p <- dat %>%
  filter(section  %in% c("grand-final", "final"),   #Filtering only to final results
         rank  %in% c("1", "2", "3"),   #Filtering to 1st-3rd
         year != "2020",   #Removing 2020 (no points that year)
         !is.na(total_points)) %>%   #removing NAs
  ggplot(aes(x = year, y = total_points,
             colour = factor(rank), shape = factor(rank))) +
  geom_point(size = 6, alpha = 0.8) +   #Increasing point size and making slightly transparent
  theme_classic((base_size = 30)) +   #Removing grid and grey panel, increasing base size
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),  #Customising margins
        text = element_text(family = "Grace", size = 110),  #Customising fonts
        axis.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  #Flipping x-axis text
        plot.caption = element_text(size = 60),
        panel.background = element_rect(fill = '#f5f30c'),  #Changing fill colour to light blue
        plot.background = element_rect(fill = '#f5f30c'),
        legend.position = c(0.5, 0.95),  #Customising legend position
        legend.spacing.x = unit(1.0, 'cm'), #Reducing legend spacing
        legend.direction = "horizontal",   #Making legend horizontal
        legend.title = element_blank(),   #Removing legend title
        plot.title = element_text(hjust = 0.5),  #Centering legend title
        axis.ticks = element_line(colour = "black"),
        legend.key = element_rect(fill = "#f5f30c", colour = "#f5f30c"), #Changing legend fill
        legend.background = element_rect(fill = "#f5f30c", colour = "#f5f30c")) +
  scale_colour_manual(values = c("#ef0e7e", "#0d43e8", "#05045d"),
                      labels=c('1st', '2nd', '3rd')) +  #Customising fill colours and shapes
  scale_shape_manual(values = c(15, 17, 19),
                     labels=c('1st', '2nd', '3rd')) +
  scale_x_continuous(limits = c(1955, 2022), breaks = seq(1955, 2022, 5)) +  #Customising axes
  scale_y_continuous(limits = c(0, 770), breaks = seq(0, 770, 100)) +
  xlab("Year") +  #Adding axis labels. title and caption
  ylab("Total points") +
  ggtitle("Eurovision results 1956-2022") +
  labs(caption = "Data source: Kaggle")

p

#Saving plot
ggsave("Eurovision Scatter.jpg", height = 8, width = 12, units = "in", dpi = 300)




#Plotting smooth density plot
p2 <- dat %>%
  filter(section  %in% c("grand-final", "final"),   #Filtering only to final results
         artist_country  %in% c("United Kingdom", "Sweden"),   #Filtering to UK and Sweden
         year != "2020") %>%   #Removing 2020
  ggplot(aes(x = rank,
             fill = artist_country)) +
  geom_bar(position = "stack", aes(width = 0.1)) +
  theme_classic((base_size = 30)) +   #Removing grid and grey panel, increasing base size
  theme(plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),  #Customising margins
        text = element_text(family = "Grace", size = 110),  #Customising fonts
        axis.text = element_text(colour = "black"),
        plot.caption = element_text(size = 60),
        panel.background = element_rect(fill = '#f5f30c'),  #Changing fill colour to light blue
        plot.background = element_rect(fill = '#f5f30c'),
        legend.position = c(0.5, 0.95),  #Customising legend position
        legend.spacing.x = unit(1.0, 'cm'), #Reducing legend spacing
        legend.direction = "horizontal",   #Making legend horizontal
        legend.title = element_blank(),   #Removing legend title
        plot.title = element_text(hjust = 0.5),  #Centering plot title
        axis.ticks = element_line(colour = "black"),
        legend.key = element_rect(fill = "#f5f30c", colour = "#f5f30c"), #Changing legend fill
        legend.background = element_rect(fill = "#f5f30c", colour = "#f5f30c")) +
  scale_fill_manual(values = c("#ef0e7e", "#0d43e8")) +
  scale_x_continuous(limits = c(0, 26), breaks = seq(0, 26, 1), expand = c(0, 0)) +  #Customising axes
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 1), expand = c(0, 0)) +
  xlab("Final Position") +  #Adding axis labels, title and caption
  ylab("Count") +
  ggtitle("Eurovision 1956-2022 UK vs. Sweden") +
  labs(caption = "Data source: Kaggle")

p2

#Saving plot
ggsave("Eurovision Histogram.jpg", height = 8, width = 12, units = "in", dpi = 300)




#Wrangling data
dat2 <-dat %>%
  select(artist_country, section, total_points, year) %>%   #Selecting columns
  group_by(artist_country) %>%   #Grouping
  filter(section  %in% c("grand-final", "final"),   #Filtering only to final results
         year != "2020",    #Removing 2020
         !is.na(total_points)) %>%  #Removing rows with NAs in total points
  mutate(Max = max(total_points), Min = min(total_points)) %>%  #Creating columns with max and minimum points
  mutate(Diff = Max - Min) %>% #Creating column with difference between max and min
  filter(row_number(artist_country) == 1) #Removing duplicate rows


#Changing to tibble (need to do this to get arrange and mutate to work for ordering by difference)
dat2 <- as_tibble(dat2)
str(dat2)


#Plotting lollipop plot
p3<- dat2 %>%
  arrange(Diff) %>% #Arranging by difference
  mutate(artist_country = factor(artist_country, levels = artist_country)) %>% #Updating factor levels
  ggplot(aes(x = artist_country, y = Max)) +
  geom_segment( aes(xend = artist_country, yend = Min)) +
  geom_point(aes(x = artist_country, y = Min), color="#ef0e7e", size = 3) +
  geom_point(aes(x = artist_country, y = Max), color="#0d43e8", size = 3) +
  theme_classic((base_size = 30)) +   #Removing grid and grey panel, increasing base size
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),  #Customising margins
        text = element_text(family = "Grace", size = 70),  #Customising fonts
        axis.text = element_text(colour = "black"),
        axis.text.y = element_text(size = 40),
        panel.background = element_rect(fill = '#f5f30c'),  #Changing fill colour to light blue
        plot.background = element_rect(fill = '#f5f30c'),
        plot.title = element_text(hjust = 0.5),  #Centering legend title
        axis.ticks = element_line(colour = "black"),
        legend.key = element_rect(fill = "#f5f30c", colour = "#f5f30c"), #Changing legend fill
        legend.background = element_rect(fill = "#f5f30c", colour = "#f5f30c")) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 800), breaks = seq(0, 800, 50)) +   #Customising axis
  xlab("") +  #Adding axis labels. title and caption
  ylab("Points") +
  ggtitle("Best and worst performances of each country in Eurovision 1956-2022") +
  labs(caption = "Data source: Kaggle")

p3

#Saving plot
ggsave("Eurovision Lollipop.jpg", height = 8, width = 12, units = "in", dpi = 300)
