#Loading packages
library(tidyverse)
library(forcats)
library(extrafont)
font_import()
loadfonts(device="win")


#Get data
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2020-06-30')

comic_bechdel <- tuesdata$comic_bechdel
characters <- tuesdata$characters


#Looking at the data
glimpse(characters)
glimpse(comic_bechdel)


#Making colour schemes
mycolours <- c("deeppink2", "darkorchid2", "turquoise2")


#CHARACTERS
#Plotting clothing torn by character
characters %>%
  filter(clothing_torn>0) %>%   #filtering out zeros
  mutate(character = case_when(character== "Wolverine = Logan" ~ "Wolverine",
                               character == "Banshee = Sean Cassidy" ~ "Banshee",
                               character== "Storm = Ororo Munroe" ~ "Storm",
                               character== "Rogue = Name Unknown" ~ "Rogue",
                               character== "Psylocke = Elizabeth (Betsy) Braddock" ~ "Psylocke",
                               character== "Professor X" ~ "Professor X",
                               character== "Nightcrawler = Kurt Wagner" ~ "Nightcrawler",
                               character== "Mystique = Name Unknown" ~ "Mystique",
                               character== "Marvel Girl/Phoenix = Jean Grey" ~ "Marvel Girl/Phoenix",
                               character== "Magneto = Erik Magnus" ~"Magneto",
                               character== "Jubilee = Jubilation Lee" ~ "Jubilee",
                               character== "Havok = Alex Summers" ~ "Havok",
                               character== "Forge = Name Unknown" ~ "Forge",
                               character== "Dazzler = Alison Blaire" ~ "Dazzler",
                               character== "Cyclops = Scott Summers" ~ "Cyclops",
                               character== "Colossus = Peter (Piotr) Rasputin" ~ "Colossus",
                               character== "Binary/Ms Marvel = Carol Danvers" ~ "Binary/Ms Marvel",
                               character== "Ariel/Sprite/Shadowcat = Kitty Pryde" ~ "Ariel/Sprite/Shadowcat")) %>%  #shortening to alias only
  ggplot(aes(x=issue, y=character, color=as.factor(clothing_torn))) + #colouring by number of instances
  geom_point(size=2)+
  theme_classic() + #removing grid
  theme(text = element_text(family = "Rockwell", size=14, color="white"), #changing font
        legend.position="top", legend.text=element_text(size=12), legend.title=element_text(size=12), #changing legend position and font
        axis.text.x= element_text(color="white"), axis.text.y= element_text(color="white"),  #changing axis text colour
        panel.background = element_blank(),
        plot.background = element_rect(fill = "gray5"), #changing background colour
        legend.background = element_rect(fill = "gray5"),  #changing legend background colour
        legend.key = element_rect(fill="gray5"), #changing legend key fill
        plot.title = element_text(hjust = 0.5),  #centering title
        plot.caption = element_text(size=8, face = "italic"),
        axis.line = element_line(colour = "white"),
        axis.ticks = element_line(colour = "white")) +
  scale_x_continuous(breaks=seq(0,300,25)) + #changing interval on y axis
  labs(color = "Instances of Torn Clothing",
       caption = "Data source:  Claremont Run Project and Malcom Barret(https://twitter.com/malco_barrett)", size=4) +  #changing legend label, adding caption
  ylab("") +
  xlab("Issue Number") +
  ggtitle("Wolverine must have a tailor on retainer") +
  scale_color_manual(values=mycolours)

#Saving plot
ggsave("Torn Clothing.png", height = 6, width = 11, units = "in", dpi = 300)


#BECHDEL
#Plotting bechdel passes, grouping by series
comic_bechdel %>%
  filter(pass_bechdel %in% c("yes", "no"))  %>%   #filter out an NAs
  mutate(pass_bechdel = fct_relevel(pass_bechdel, "yes", "no")) %>%
ggplot(aes(x =series, y=..count.., fill=pass_bechdel)) +
  geom_bar(position = "stack") +
  geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 0.5), color="white", family = "Rockwell", size=3) + #adding counts and centring position
  theme_classic() + #removing grid
  theme(text = element_text(family = "Rockwell", size=14, color="white"), #changing font
        legend.position="top", legend.text=element_text(size=12), legend.title=element_text(size=12), #changing legend position and font
        axis.text.x= element_text(color="white", angle = 45, vjust = 1, hjust=1),  #changing axis text colour and angle
        axis.text.y= element_text(color="white"),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "gray5"), #changing background colour
        legend.background = element_rect(fill = "gray5"),  #changing legend background colour
        legend.key = element_rect(fill="gray5"), #changing legend key fill
        plot.title = element_text(hjust = 0.5),  #centering title
        plot.caption = element_text(size=10, face = "italic"),
        axis.line = element_line(colour = "white"),
        axis.ticks = element_line(colour = "white")) +
  ylab("Count") +
  xlab("") +
  labs(fill = "Passed",
       caption = "Data source:  Claremont Run Project and Malcom Barret (https://twitter.com/malco_barrett)") + #changing legend label, adding caption
  ggtitle("Comics and the Bechdel Test") +
  scale_fill_manual(values=c("skyblue3", "goldenrod2"))

#Saving plot
ggsave("Bechdel Test.png", height = 6, width = 11, units = "in", dpi = 300)
