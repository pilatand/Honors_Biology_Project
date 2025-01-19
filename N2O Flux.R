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
