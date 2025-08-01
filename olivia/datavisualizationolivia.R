library(ggplot2)
library(ggthemes)

master1<-read.csv("olivia/Harvard Master2.csv")
colnames(master1)

master1$treatment <- stringr::str_extract(master1$Leaf.Number, "^..")

ggplot(data = master1, mapping = aes(x = treatment, y = Flavonoids )) +
  geom_boxplot(aes(fill = treatment))+
  geom_point(aes(color = treatment))+
  scale_fill_manual(values = c ("azure4","blueviolet","black","grey"),
                    (name="legendtitle"),
                    label=c("AA Absent","AA Present","LP Absent", "LP Present"))+
  scale_color_manual(values = c ("azure4","blueviolet","black","grey"),
  (name="legendtitle"),
  label=c("AA Absent","AA Present","LP Absent", "LP Present"))+
 
   scale_x_discrete(label=c("AA Absent","AA Present","LP Absent", "LP Present"),name = "treatments")+
  ylab("Flavonoids")+
  ggtitle("Flavonoid Concentration")+
  
  theme_tufte()+
  
  theme(legend.position="none",
        panel.background =element_rect(fill="orange"),
        plot.title = element_text(hjust = 0.5))




                



  

  
  
