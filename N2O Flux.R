library(mosaic)

Flux_NCyc <- read.csv("/Users/andrewpilat/Documents/Honors/Data/NCyc/N2O_Flux_Data.csv")

Flux_NCyc <- cbind(Flux_NCyc, Real_Names = c("Ballfield", "Burtnett Wetland", "Blackjack", "Batnest", "Burtnett Farm", 
                                             "Ballfield", "BFEC Farm", "Burtnett Farm", "Blackjack", "McManis Farm", "Burtnett Farm",
                                             "Burtnett Wetland", "McManis Farm", "Batnest", "Ballfield"))

tiff("Audrey_Flux.tiff", units = "in", width =5, height=5, res=300)
ggplot(Flux_NCyc)+geom_point(aes(x= norB, y = Flux_Rate, color = Real_Names), size = 3)+theme_classic()+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))+
  labs(
     x = "norB",
     y = expression("N2O Flux (µg hr"^-1 ~ "m"^-2 ~")"),
    color = "Sites")
dev.off()

cor.test(Flux_NCyc$Flux_Rate~Flux_NCyc$norB, method = "spearman")
##Spearman test, S == 66.599, rho = 0.697, p-value = 0.01708

