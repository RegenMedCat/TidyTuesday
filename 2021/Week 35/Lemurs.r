#Loading packages
library(tidyverse)
library(showtext)

#Adding Google font
font_add_google("Patrick.Hand", "Patrick")


#Loading data
lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')


#Checking data
glimpse(lemurs)


#Sorting data to plot oldest lemurs
#Replacing the taxon with common names was a mission!
dat <-select(lemurs, age_at_death_y, taxon) %>%
  filter(!is.na(age_at_death_y)) %>%   #Removing NAs
  group_by(taxon) %>%
  summarise(max = max(age_at_death_y)) %>%
  mutate(taxon = replace(taxon, taxon == "CMED", "Fat-tailed dwarf lemur"),
         taxon = replace(taxon, taxon == "DMAD", "Aye-aye"),
         taxon = replace(taxon, taxon == "EALB", "White-fronted brown lemur"),
         taxon = replace(taxon, taxon == "ECOL", "Collared brown lemur"),
         taxon = replace(taxon, taxon == "ECOR", "Crowned lemur"),
         taxon = replace(taxon, taxon == "EFLA", "Blue-eyed black lemur"),
         taxon = replace(taxon, taxon == "EFUL", "Common brown lemur"),
         taxon = replace(taxon, taxon == "EMAC", "Black lemur"),
         taxon = replace(taxon, taxon == "EMON", "Mongoose lemur"),
         taxon = replace(taxon, taxon == "ERUB", "Red-bellied lemur"),
         taxon = replace(taxon, taxon == "ERUF", "Red-fronted brown lemur"),
         taxon = replace(taxon, taxon == "ESAN", "Sanford's brown lemur"),
         taxon = replace(taxon, taxon == "EUL", "Hybrid (true lemur)"),
         taxon = replace(taxon, taxon == "GMOH", "Mohol bushbaby"),
         taxon = replace(taxon, taxon == "HGG", "Eastern lesser bamboo lemur"),
         taxon = replace(taxon, taxon == "LCAT", "Ring-tailed lemur"),
         taxon = replace(taxon, taxon == "LTAR", "Slender loris"),
         taxon = replace(taxon, taxon == "MMUR", "Gray mouse lemur"),
         taxon = replace(taxon, taxon == "MZAZ", "Northern giant mouse lemur"),
         taxon = replace(taxon, taxon == "NCOU", "Slow loris"),
         taxon = replace(taxon, taxon == "NPYG", "Pygmy slow loris"),
         taxon = replace(taxon, taxon == "OGG", "Northern greater galago"),
         taxon = replace(taxon, taxon == "PCOQ", "Coquerel's sifaka"),
         taxon = replace(taxon, taxon == "PPOT", "Potto"),
         taxon = replace(taxon, taxon == "VAR", "Hybrid (ruffed lemur)"),
         taxon = replace(taxon, taxon == "VRUB", "Red ruffed lemur"),
         taxon = replace(taxon, taxon == "VVV", "Black-and-white ruffed lemur")) %>%
    mutate(taxon = fct_reorder(taxon, desc(max))) #Ordering by descending age

#Hybrid was there twice so I added extra info in brackets to differentiate them


#Lollipop plot of oldest lemurs
#https://www.r-graph-gallery.com/lollipop-plot.html followed this
p1 <- dat %>%
  ggplot(aes(x=taxon, y=max, label=max)) + 
  geom_segment(aes(y = 0, 
                   x = taxon,
                   xend = taxon,
                   yend = max), 
               color = "black") +
  geom_point(stat='identity', colour="#34511a", size=7)  +  #Adding points, customising colour and size
  geom_text(color="white", size=6) +  #Adding text to lollipops
  theme_classic() +  #Removing grid lines
  theme(text = element_text(family="Patrick", size= 46),  #Using custom Google font
    plot.title = element_text(hjust=0.5,  size=56),
    axis.title = element_text(size=42),
    panel.background = element_rect(fill = '#fcf9eb'),
    plot.background = element_rect(fill = '#fcf9eb')) +
  scale_y_continuous(limits=c(0, 40), breaks=seq(0, 40, 5), expand = c(0, 0)) + 
  #Customising limits, removing annoying axis gap
  xlab("") +   #Customising axis labels
  ylab("Age (Years)") +
  labs(title="The Oldest Lemurs") +   #Adding plot title
  labs(caption = "Data source: Kaggle") +   #Adding caption
  coord_flip()  #Flipping axes around

p1


#Saving plot
ggsave("Lemurs.png", height = 8, width = 10, units = "in", dpi = 300)
