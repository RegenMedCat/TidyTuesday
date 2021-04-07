
#Loading packages
library(tidyverse)
library(maps)
library(stringr)
library(gganimate)
library(transformr)
library(extrafont)
font_import()
fonts()


#Get the data
forest <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest.csv')
forest_area <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest_area.csv')
brazil_loss <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/brazil_loss.csv')
soybean_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/soybean_use.csv')
vegetable_oil <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/vegetable_oil.csv')


glimpse(forest)


#Plotting static map of 2015 forest data
#https://sarahpenir.github.io/r/making-maps/


#Making simple world map
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot


#Filtering to 2015 only and selecting columns
forestdat <- forest %>%
  filter(year == 2015) %>%
  select(region = entity, net_forest_conversion, code)
  
head(forestdat)


#Merging datasets
forestSubset <- inner_join(world, forestdat, by = "region")

head(forestSubset)


worldforest <- ggplot(data = forestSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = net_forest_conversion), colour="black") +  #added black border to countries
  borders("world") +  #adding borders to show countries with missing data
  theme_classic() +
  theme(text = element_text(family = "Verdana Pro Cond", size=16),
        axis.title = element_blank(),   #removing axes, ticks, titles etc.
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.caption = element_text(size=12, face = "italic"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +  #adding o.5 cm margins to plot as stuff was being cut off
  scale_fill_distiller("Net change (hectares)", palette ="PiYG", direction = 1) + #adding gradient fill, specifying direction
  ggtitle("Change every 5 years in forest area conversion") +
  labs(caption = "Data source: Our World in Data") #adding caption
  
worldforest


#Making animated map of all years for forest conversion
#Including all years
forestdat2 <- forest %>%
  select(region = entity, net_forest_conversion, code, year) 

head(forestdat2)

#Merging datasets
forestSubset2 <- inner_join(world, forestdat2, by = "region") %>%
mutate(year = factor(year)) #making year factor

head(forestSubset2)


worldforest2 <- ggplot(data = forestSubset2, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = net_forest_conversion), colour="black") +  #added black border to countries
  borders("world") +  #adding borders to show countries with missing data
  theme_classic() +
  theme(text = element_text(family = "Verdana Pro Cond", size=16),
        axis.title = element_blank(),   #removing axes, ticks, titles etc.
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.caption = element_text(size=12, face = "italic"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +  #adding o.5 cm margins to plot as stuff was being cut off
  scale_fill_distiller("Net change (hectares)", palette ="PiYG", direction = 1) + #adding gradient fill, specifying direction
  ggtitle("Change every 5 years in forest area conversion") +
  labs(caption = "Data source: Our World in Data") + #adding caption
  transition_time(as.integer(year)) +  #adding animation
  ease_aes("linear") +
  geom_text(data = forestSubset2, aes(-140, -40, label = year, family = "Verdana Pro Cond"), size = 8, check_overlap = TRUE)
  #adding label with year, check_overlap stops text overlapping

worldforest2


animate(worldforest2, fps=20, height=300, width=600)

#Saving GIF
anim_save("Forest Map.gif")
