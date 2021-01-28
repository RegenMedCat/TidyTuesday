#Loading packages
library(tidyverse)
library(tidytuesdayR)
library(extrafont)
library(treemapify)
library(magick)
font_import()
fonts()


#Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 5)

plastics <- tuesdata$plastics

#Checking data
glimpse(plastics)


#Filtering data
tree.data <- plastics %>%
  filter(country=="United Kingdom of Great Britain & Northern Ireland", parent_company!= c("Unbranded", "null"))  #removing unbranded and null

#Making treemap using treemapify
ggplot(tree.data, aes(area = grand_total, label = parent_company)) +
  geom_treemap(fill="#280c5e") +   #changing fill
  geom_treemap_text(family = "Segoe UI Semilight", colour = "white", place = "centre",
                    grow = TRUE) +  #changing fon, colour and centring
  theme(plot.background = element_rect(fill = "white"), #changing plot background colour
        plot.title = element_text(size=24, family = "Segoe UI Black", hjust=0.5),  #changing title font and centering
        plot.caption = element_text(size=12, face = "italic")) +
labs(caption = "Data source: Break Free from Plastic") + #adding caption
  ggtitle("Sources of Plastic Pollution in the UK")  #adding title

ggsave("Plot.png")


#Reading in plot and UK map
#Used tutorial https://themockup.blog/posts/2019-01-09-add-a-logo-to-your-plot/
plot <- image_read("https://raw.githubusercontent.com/RegenMedCat/TidyTuesday/master/2021/UK.png")
map <- image_read("https://live.staticflickr.com/7171/6504960113_dcdef82e94.jpg")

#Viewing images
print(plot)
print(map)

#Resizing map
small.map<-image_scale(map,'X120')

#Adding map to plot
plot %>% 
  image_composite(small.map, offset = "+320+10")   #offsetting to position beside title

#Saving
magick::image_write(plot, "Plot with Map.png")
