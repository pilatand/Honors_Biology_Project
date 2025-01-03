library(mosaic)

Kokosing_NCyc <- read.csv("/Users/andrewpilat/Documents/Honors/Data/NCyc/Kokosing_River_NCyc.csv")

Kokosing_NCyc <- cbind(Kokosing_NCyc, Location = c("Up", "Mid", "Down","Up", "Mid", "Down","Up", "Mid", "Down",
                                                   "Up", "Mid", "Down","Up", "Mid", "Down","Up", "Mid", "Down"))

tiff("UreA_Kokosing.tiff", units = "in", width =5, height=5, res=300)
ggplot(Kokosing_NCyc)+geom_boxplot(aes(x = Location, y = ureA, color = Location), size = 0.7)+theme_classic()+geom_point(aes(x = Location, y = ureA))+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))+ theme(legend.position = "none")
dev.off()


##ggplot(Kokosing_NCyc)+geom_violin(aes(x = Location, y = ureA, color = Location))+theme_classic()+geom_point(aes(x = Location, y = ureA))


kruskal.test(Kokosing_NCyc$ureA ~ Kokosing_NCyc$Location)
help("kruskal.test")
##x^2 = 0.11475, df = 2, p-value = 0.9442
