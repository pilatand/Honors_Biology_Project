######ARCHIVE PROGRAM NOT ASSOCIATED WITH MANUSCRIPT##################



# Author: Andrew Pilat
# History:
# Created - 4/7/25
# Discontinued - 7/31/25
# Affiliation: Kenyon College, Department of Biology





library(dunn.test)

Kokosing_Boxplot <- read.csv("/Users/andrewpilat/Documents/Honors/Data/Kokosing_Boxplot.csv")
Kokosing_Boxplot <- Kokosing_Boxplot[1:18,]
Kokosing_Boxplot$Rep <- factor(Kokosing_Boxplot$Rep, levels = c("Upstream", "Midstream", "Downstream"))

tiff("Kokosing_Nitrate.tiff", units = "in", width =5, height=4, res=300)
ggplot(data=Kokosing_Boxplot)+geom_boxplot(aes(x=Rep, y=Nitrate))+theme_classic()+xlab("River Location")+
  ylab("Nitrate (mg/L)")+theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))+
  annotate("text", x = 1, y=3, label = "a")+annotate("text", x = 2, y=7, label = "b")+annotate("text", x = 3, y=1, label = "a")
dev.off()

kruskal.test(Kokosing_Boxplot$Nitrate, Kokosing_Boxplot$Rep) ##p = 0.004425
dunn.test(Kokosing_Boxplot$Nitrate, Kokosing_Boxplot$Rep, method = "bh") ##Midstream is significantly different, p < 0.01

tiff("Kokosing_Phosphate.tiff", units = "in", width =5, height=4, res=300)
ggplot(data=Kokosing_Boxplot)+geom_boxplot(aes(x=Rep, y=Phosphate))+theme_classic()+xlab("River Location")+
  ylab("Phosphate (mg/L)")+theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))+
  annotate("text", x = 1, y=1, label = "a")+annotate("text", x = 2, y=5, label = "b")+annotate("text", x = 3, y=1, label = "a")
dev.off()

kruskal.test(Kokosing_Boxplot$Phosphate, Kokosing_Boxplot$Rep) ##p = 0.001772
dunn.test(Kokosing_Boxplot$Phosphate, Kokosing_Boxplot$Rep, method = "bh") ## p < 0.05

tiff("Kokosing_Conductivity.tiff", units = "in", width =5, height=4, res=300)
ggplot(data=Kokosing_Boxplot)+geom_boxplot(aes(x=Rep, y=Cond))+theme_classic()+xlab("River Location")+
  ylab("Conductivity (μS/cm)")+theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))
dev.off()

ggplot(data=Kokosing_Boxplot)+geom_boxplot(aes(x=Rep, y=DO..mg.L))+theme_classic()+xlab("River Location")+
  ylab("Conductivity (μS/cm)")+theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))
dev.off()


kruskal.test(Kokosing_Boxplot$Cond, Kokosing_Boxplot$Rep) ##p = 0.05588
dunn.test(Kokosing_Boxplot$Cond, Kokosing_Boxplot$Rep, method = "bh") ##not significant
