#load packages
library(ggplot2)
library(ggthemes)

#whole plant traits
master_whole <- read.csv("isha/Harvard Master.csv")
colnames(master_whole)

ggplot(data = master_whole, mapping = aes(x = Type, y = Plant.height..ft.)) +
  geom_boxplot(aes(fill = Type)) +
  geom_point() +
  scale_fill_manual(values = c("lemonchiffon", "olivedrab3", "thistle"), name = "Species Type")+
  xlab("Species Type") +
  ylab("Plant Height (ft)") +
  ggtitle("Species Type vs Plant Height") +
  theme_tufte() +
  theme(legend.position = "none", panel.background = element_rect(fill = "grey"), 
        plot.title = element_text(hjust = 0.5))


#leaf traits
master_leaf <- read.csv("isha/Harvard MasterLeaf.csv")
colnames(master_leaf)

ggplot(master_leaf, aes(Type, Thickness..mm.)) +
  geom_boxplot(aes(fill = Type)) +
  geom_point() +
  scale_fill_manual(values = c("lemonchiffon", "olivedrab3", "thistle"), name = "Species Type")







