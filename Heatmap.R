library(ComplexHeatmap) ##From Bioconductor
library(circlize)

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

##Read in the spreadsheet with the median values
Heatmap_NCyc <- read.csv("/users/andrewpilat/Documents/Honors/Data/NCyc/Heatmap_NCyc.csv")
Heatmap_NCyc <- Heatmap_NCyc[,-1]
rownames(Heatmap_NCyc) <- c("nifH", "amoB", "hao", "nxrB", "nirB", "nirS", "nirK", "norB", "nosZ", "hszA", "ureA")
colnames(Heatmap_NCyc) <- c("Bonney Water", "Fryxell Water", "Fryxell Mat", "Ballfield Soil", "Batnest Soil", "BFEC Soil", "Blackjack Soil", "Burtnett Soil", "Kokosing Soil", "BFEC Farm Soil", "Burtnett Farm Soil", "McManis Farm Soil", "Kokosing Trail Soil", "Kokosing Water", "Burtnett Water", "Foundation Water", "McManis Water", "Porter Water", "Compost Detritus")

Heatmap_matrix <- as.matrix(Heatmap_NCyc)

col_fun= colorRamp2(c(0, 250, 600), c("blue", "#76d587", "#ffff00"))

row_split <- c(rep(1,1), rep(2, 3), rep(3,1), rep(4, 4), rep(5, 1), rep(6,1))
column_split <- c(rep("Antarctica Lakes", 3), rep("Wetlands", 6), rep("Agricultural", 4), rep("River", 1), rep("Ponds", 4), rep("Compost", 1))

tiff("Median Heatmap.tiff", units = "in", width =15, height=5, res=300)
Heatmap(Heatmap_matrix, col = col_fun, row_order = rownames(Heatmap_matrix), column_order = colnames(Heatmap_matrix), 
        name = "Rel. Abundance", row_split= row_split, column_split = column_split)
dev.off()


