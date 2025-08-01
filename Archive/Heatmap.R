library(ComplexHeatmap) ##From Bioconductor
library(circlize)
library(NSM3)
library(dunn.test)
library(colorspace)

##Read in the Master Spreadsheet
Master_NCYC <- read.csv("/users/andrewpilat/Documents/Honors/Data/NCyc/Master_NCYC.csv")

##Median for nifH
tapply(Master_NCYC$nifH, Master_NCYC$Median_Name, median)

##Median for amoB
tapply(Master_NCYC$amoB_B, Master_NCYC$Median_Name, median)

##Median for hao
tapply(Master_NCYC$hao, Master_NCYC$Median_Name, median)

##Median for nxrB
tapply(Master_NCYC$nxrB, Master_NCYC$Median_Name, median)

##Median for nirB
tapply(Master_NCYC$nirB, Master_NCYC$Median_Name, median)

##Median for nirS
tapply(Master_NCYC$nirS, Master_NCYC$Median_Name, median)

##Median for nirK
tapply(Master_NCYC$nirK, Master_NCYC$Median_Name, median)

##Median for norB
tapply(Master_NCYC$norB, Master_NCYC$Median_Name, median)

##Median for nosZ
tapply(Master_NCYC$nosZ, Master_NCYC$Median_Name, median)

##Median for hszA
tapply(Master_NCYC$hzsA, Master_NCYC$Median_Name, median)

##Median for ureA
tapply(Master_NCYC$ureA, Master_NCYC$Median_Name, median)

##Median for nfrA
tapply(Master_NCYC$nrfA, Master_NCYC$Median_Name, median)

##Read in the spreadsheet with the median values
Heatmap_NCyc <- read.csv("/users/andrewpilat/Documents/Honors/Data/NCyc/Heatmap_NCyc.csv")
Heatmap_NCyc <- Heatmap_NCyc[,-1]
rownames(Heatmap_NCyc) <- c("nifH", "amoB", "hao", "nxrB", "nirB", "nirS", "nirK", "norB", "nosZ", "hszA", "ureA")
colnames(Heatmap_NCyc) <- c("Bonney Water", "Fryxell Water", "Fryxell Mat", "Ballfield Soil", "Batnest Soil", "BFEC Soil", "Blackjack Soil", "Burtnett Soil", "Kokosing Soil", "BFEC Farm", "Burtnett Farm", "McManis Farm", "Kokosing Trail", "Kokosing Water", "Burtnett Water", "Foundation Water", "McManis Water", "Porter Water", "Compost Detritus")

Heatmap_matrix <- as.matrix(Heatmap_NCyc)

blue   <- hcl(h = 240, c = 80, l = 40)  # blue-ish
green  <- hcl(h = 130, c = 80, l = 60)  # green
yellow <- hcl(h = 55,  c = 90, l = 85)

col_fun= colorRamp2(c(0, 300, 600), c("#1565C0", "#8bc34a", "#ffea00"))

row_split <- c(rep(2,1), rep(3, 3), rep(1, 5), rep(4, 1), rep(5,1))
column_split <- c(rep(1, 3), rep(4, 6), rep(5, 3), rep(8,1), rep(3, 1), rep(2, 4), rep(7, 1))

tiff("Median_Heatmap_Remake.tiff", units = "in", width =16, height=9, res=300)
Heatmap(Heatmap_matrix, col = col_fun, row_order = rownames(Heatmap_matrix), column_order = colnames(Heatmap_matrix), 
        name = "Rel. Abundance", column_names_rot = 45, row_split= row_split, column_split = column_split, column_title = c("Antarctic Lakes", "Ohio Ponds", "Ohio River", "Ohio Wetlands", "Agricultural Soil", "Compost", 
                                                                                                                             "Trail Soil"),
      heatmap_legend_param = list(title = "Rel. Abundance", legend_height = unit(4, "cm"), legend_width = unit(4, "cm") ))
dev.off()
help("Legend")
###Statistics###
Master <- read.csv("/users/andrewpilat/Documents/Honors/Data/NCyc/Soil_versus_water.csv")

Soil <- Master[,12:22]
Soil <- Soil[1:47,]

help("wilcox.test")
wilcox.test(Master$nirB_Water, Master$nirB_Soil, alternative = "less", paired = FALSE) ## p < 0.0001
wilcox.test(Master$nirS_Water, Master$nirS_Soil, alternative = "less", paired = FALSE) ## p < 0.0001
wilcox.test(Master$nirK_Water, Master$nirK_Soil, alternative = "less", paired = FALSE) ## p < 0.0001
wilcox.test(Master$norB_Water, Master$norB_Soil, alternative = "less", paired = FALSE) ## p < 0.0001
wilcox.test(Master$nosZ_Water, Soil$nosZ_Soil, alternative = "two.sided", paired = FALSE) ## p < 0.0001

wilcox.test(Master$nirS_Soil, Master$nirK_Soil, alternative = "less", paired = FALSE)

boxplot(Master$nirS_Soil, Master$nirK_Soil)


wilcox.test(Master$hzsA_Water, Soil$hzsA_Soil, alternative = "greater", paired = FALSE)
boxplot(Master$)

kruskal.test(Master$nirB~Master$Median_Name) ## p < 0.0001
pSDCFlig(x =Master$nirB, g=Master$Groups) 
help("pSDCFlig")

kruskal.test(Master$nirS~Master$Median_Name) ## p < 0.0001

pSDCFlig(x =Master$nirS, g=Master$Groups)

water <- Master_NCYC[c(1:12, 28:45, 59,72:219),]
water$norB <- as.double(water$norB)
water$Median_Name <- as.factor(water$Median_Name)
kruskal.test(water$norB~water$Median_Name)
dunn.test(water$norB, water$Median_Name, method = "bh")

tapply(water$norB, water$Median_Name, median, na.rm = TRUE)

