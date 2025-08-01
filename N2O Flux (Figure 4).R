######Figure 4 Correlation##################



# Author: Andrew Pilat
# History:
# Created - 12/29/24
# Modified - 7/7/25
# Finalized - 7/31/25
# Affiliation: Kenyon College, Department of Biology


library(mosaic)

Flux_NCyc <- read.csv("/Users/andrewpilat/Documents/Honors/Data/NCyc/N2O_Flux_Data.csv")

Flux_NCyc <- cbind(Flux_NCyc, Real_Names = c("Ballfield", "Burtnett Wetland", "Blackjack", "Batnest", "Burtnett Farm", 
                                             "Ballfield", "BFEC Farm", "Burtnett Farm", "Blackjack", "McManis Farm", "Burtnett Farm",
                                             "Burtnett Wetland", "McManis Farm", "Batnest", "Ballfield"))
Flux_NCyc$logFlux <- log(Flux_NCyc$Flux_Rate)


tiff("Audrey_Flux.tiff", units = "in", width =8, height=5, res=300)
ggplot(Flux_NCyc)+geom_point(aes(x= norB, y = logFlux, color = Real_Names), size = 3)+theme_classic()+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))+xlim(95,500)+
  labs(
     x = "Relative norB Abundance",
     y = expression("Log N2O Flux (µg hr"^-1 ~ "m"^-2 ~")"),
    color = "Sites")+geom_smooth(aes(x= norB, y = logFlux), method = "lm", se = TRUE, color = "black")+
  scale_color_manual(values = c("red", "#f9a30d", "blue", "#6b82da","#6bdab0", "darkgreen", "purple"))
dev.off()

help(scale_color_manual)

cor.test(Flux_NCyc$logFlux~Flux_NCyc$norB, method = "spearman")
##Spearman test, S == 66.599, rho = 0.697, p-value = 0.01708


lm <- lm(Flux_NCyc$logFlux~Flux_NCyc$norB)
summary(lm)

plot(lm$residuals~lm$fitted.values)
qqplot(residuals)
qqnorm(Flux_NCyc$logFlux)

Flux_NCyc <- cbind(Flux_NCyc, Sites_Specific = c("Wetland", "Wetland", "Wetland", "Wetland", "Farm", 
                                             "Wetland", "Farm", "Farm", "Wetland", "Farm", "Farm",
                                             "Wetland", "Farm", "Wetland", "Wetland"))

Flux_NCyc$logFlux <- log(Flux_NCyc$Flux_Rate)

wilcox.test(Flux_NCyc$Flux_Rate~Flux_NCyc$Sites_Specific)
wilcox.test(Flux_NCyc$norB~Flux_NCyc$Sites_Specific)
boxplot(Flux_NCyc$logFlux~Flux_NCyc$Sites_Specific)

tiff("Audrey_Flux_Wetland_Farm_boxplots.tiff", units = "in", width =8, height=5, res=300)
ggplot(Flux_NCyc)+geom_boxplot(aes(x= Sites_Specific, y = logFlux))+ylim(-6,10)+
  labs(
    x = "Relative norB Abundance",
    y = expression("Log N2O Flux (µg hr"^-1 ~ "m"^-2 ~")"))+theme_classic()+ 
  theme(axis.text = element_text(size = 16),
 axis.title = element_text(size = 18))
dev.off()

help("wilcox.test")

tiff("Audrey_Flux_Wetland_Farm.tiff", units = "in", width =8, height=5, res=300)
ggplot(Flux_NCyc)+geom_point(aes(x= norB, y = logFlux, color = Sites_Specific), size = 3)+theme_classic()+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))+xlim(95,500)+
  labs(
    x = "Relative norB Abundance",
    y = expression("Log N2O Flux (µg hr"^-1 ~ "m"^-2 ~")"),
    color = "Sites")+geom_smooth(aes(x= norB, y = logFlux), method = "lm", se = TRUE, color = "black")+
  scale_color_manual(values = c("red", "blue"))
dev.off()






cor(Flux_NCyc$logFlux, Flux_NCyc$Nitrate, method = "spearman", use = "complete.obs")

tiff("Flux_Versus_Nitrate.tiff", units = "in", width =8, height=5, res=300)
ggplot(Flux_NCyc)+geom_point(aes(x= Nitrate, y = logFlux, color = Real_Names), size = 3)+theme_classic()+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))+xlim(0,40)+
  labs(
    x = "Nitrate (mg/L)",
    y = expression("Log N2O Flux (µg hr"^-1 ~ "m"^-2 ~")"),
    color = "Sites")+geom_smooth(aes(x= Nitrate, y = logFlux), method = "lm", se = TRUE, color = "black", na.rm = TRUE)+
  scale_color_manual(values = c("red", "#f9a30d", "blue", "#6b82da","#6bdab0", "darkgreen", "purple"))
dev.off()

lm <- lm(Flux_NCyc$logFlux ~ Flux_NCyc$Nitrate)
summary(lm)

plot(lm$residuals~lm$fitted.values)
help("geom_smooth")



###Checking other genes
cor.test(Flux_NCyc$logFlux, Flux_NCyc$nirS, method="spearman")
cor.test(Flux_NCyc$logFlux, Flux_NCyc$nirK, method="spearman")
cor.test(Flux_NCyc$logFlux, Flux_NCyc$norB, method="spearman")
cor.test(Flux_NCyc$logFlux, Flux_NCyc$nosZ, method="spearman")
