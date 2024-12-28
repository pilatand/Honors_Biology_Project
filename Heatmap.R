library(ComplexHeatmap) ##From Bioconductor

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
Heatmap_matrix <- as.matrix(Heatmap_NCyc)


Heatmap(Heatmap_matrix)



