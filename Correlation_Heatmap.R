library(mosaic)
library(ComplexHeatmap) ##From Bioconductor
library(circlize)

###Kokosing
Kokosing_NCyc <- read.csv("/Users/andrewpilat/Documents/Honors/Data/NCyc/Kokosing_River_NCyc.csv")

Cor_Kokosing <- Kokosing_NCyc[,c(6:23)]

correlation <- cor(Cor_Kokosing, method = "spearman")

write.csv(correlation, file = "/Users/andrewpilat/Documents/Honors/Data/NCyc/Kokosing_Correlation_Matrix.csv")


##p-values
#nifH
cor.test(Cor_Kokosing$nifH, Cor_Kokosing$Ammonia, method = "spearman")
cor.test(Cor_Kokosing$nifH, Cor_Kokosing$Nitrate, method = "spearman")
cor.test(Cor_Kokosing$nifH, Cor_Kokosing$Phosphate, method = "spearman")
cor.test(Cor_Kokosing$nifH, Cor_Kokosing$DO, method = "spearman")

#nirB
cor.test(Cor_Kokosing$nirB, Cor_Kokosing$Ammonia, method = "spearman")
cor.test(Cor_Kokosing$nirB, Cor_Kokosing$Nitrate, method = "spearman")
cor.test(Cor_Kokosing$nirB, Cor_Kokosing$Phosphate, method = "spearman")
cor.test(Cor_Kokosing$nirB, Cor_Kokosing$DO, method = "spearman")

#nirS
cor.test(Cor_Kokosing$nirS, Cor_Kokosing$Ammonia, method = "spearman")
cor.test(Cor_Kokosing$nirS, Cor_Kokosing$Nitrate, method = "spearman")
cor.test(Cor_Kokosing$nirS, Cor_Kokosing$Phosphate, method = "spearman")
cor.test(Cor_Kokosing$nirS, Cor_Kokosing$DO, method = "spearman")

#nirK
cor.test(Cor_Kokosing$nirK, Cor_Kokosing$Ammonia, method = "spearman")
cor.test(Cor_Kokosing$nirK, Cor_Kokosing$Nitrate, method = "spearman")
cor.test(Cor_Kokosing$nirK, Cor_Kokosing$Phosphate, method = "spearman")
cor.test(Cor_Kokosing$nirK, Cor_Kokosing$DO, method = "spearman")

#norB
cor.test(Cor_Kokosing$norB, Cor_Kokosing$Ammonia, method = "spearman")
cor.test(Cor_Kokosing$norB, Cor_Kokosing$Nitrate, method = "spearman")
cor.test(Cor_Kokosing$norB, Cor_Kokosing$Phosphate, method = "spearman")
cor.test(Cor_Kokosing$norB, Cor_Kokosing$DO, method = "spearman")

#nosZ
cor.test(Cor_Kokosing$nosZ, Cor_Kokosing$Ammonia, method = "spearman")
cor.test(Cor_Kokosing$nosZ, Cor_Kokosing$Nitrate, method = "spearman")
cor.test(Cor_Kokosing$nosZ, Cor_Kokosing$Phosphate, method = "spearman")
cor.test(Cor_Kokosing$nosZ, Cor_Kokosing$DO, method = "spearman")

#ureA
cor.test(Cor_Kokosing$ureA, Cor_Kokosing$Ammonia, method = "spearman")
cor.test(Cor_Kokosing$ureA, Cor_Kokosing$Nitrate, method = "spearman")
cor.test(Cor_Kokosing$ureA, Cor_Kokosing$Phosphate, method = "spearman")
cor.test(Cor_Kokosing$ureA, Cor_Kokosing$DO, method = "spearman")


##Data are sorted
Correlation_Matrix_Kokosing <- read.csv("/Users/andrewpilat/Documents/Honors/Data/NCyc/Kokosing_Correlation_Matrix_Edited.csv")
Correlation_Matrix_Kokosing <- Correlation_Matrix_Kokosing[1:4,]
rownames(Correlation_Matrix_Kokosing) <- Correlation_Matrix_Kokosing$X
Correlation_Matrix_Kokosing <- Correlation_Matrix_Kokosing[,-1]


Actual_Matrix_Kokosing <- as.matrix(Correlation_Matrix_Kokosing )


###Pond 2021####
Pond_2021_NCyc <- read.csv("/Users/andrewpilat/Documents/Honors/Data/NCyc/Pond_2021_Nitrogen.csv")

Cor_Pond_2021 <- Pond_2021_NCyc[,c(6:24)]

correlation <- cor(Cor_Pond_2021, method = "spearman")
write.csv(correlation, file = "/Users/andrewpilat/Documents/Honors/Data/NCyc/Pond2021_Correlation_Matrix.csv")

##p-values
#nifH
cor.test(Cor_Pond_2021$nifH, Cor_Pond_2021$NH4, method = "spearman")
cor.test(Cor_Pond_2021$nifH, Cor_Pond_2021$NO3, method = "spearman")
cor.test(Cor_Pond_2021$nifH, Cor_Pond_2021$PO4, method = "spearman")
cor.test(Cor_Pond_2021$nifH, Cor_Pond_2021$DO, method = "spearman")

#nirB
cor.test(Cor_Pond_2021$nirB, Cor_Pond_2021$NH4, method = "spearman")
cor.test(Cor_Pond_2021$nirB, Cor_Pond_2021$NO3, method = "spearman")
cor.test(Cor_Pond_2021$nirB, Cor_Pond_2021$PO4, method = "spearman")
cor.test(Cor_Pond_2021$nirB, Cor_Pond_2021$DO, method = "spearman")

#nirS
cor.test(Cor_Pond_2021$nirS, Cor_Pond_2021$NH4, method = "spearman")
cor.test(Cor_Pond_2021$nirS, Cor_Pond_2021$NO3, method = "spearman")
cor.test(Cor_Pond_2021$nirS, Cor_Pond_2021$PO4, method = "spearman")
cor.test(Cor_Pond_2021$nirS, Cor_Pond_2021$DO, method = "spearman")

#nirK
cor.test(Cor_Pond_2021$nirK, Cor_Pond_2021$NH4, method = "spearman")
cor.test(Cor_Pond_2021$nirK, Cor_Pond_2021$NO3, method = "spearman")
cor.test(Cor_Pond_2021$nirK, Cor_Pond_2021$PO4, method = "spearman")
cor.test(Cor_Pond_2021$nirK, Cor_Pond_2021$DO, method = "spearman")

#norB
cor.test(Cor_Pond_2021$norB, Cor_Pond_2021$NH4, method = "spearman")
cor.test(Cor_Pond_2021$norB, Cor_Pond_2021$NO3, method = "spearman")
cor.test(Cor_Pond_2021$norB, Cor_Pond_2021$PO4, method = "spearman")
cor.test(Cor_Pond_2021$norB, Cor_Pond_2021$DO, method = "spearman")

#nosZ
cor.test(Cor_Pond_2021$nosZ, Cor_Pond_2021$NH4, method = "spearman")
cor.test(Cor_Pond_2021$nosZ, Cor_Pond_2021$NO3, method = "spearman")
cor.test(Cor_Pond_2021$nosZ, Cor_Pond_2021$PO4, method = "spearman")
cor.test(Cor_Pond_2021$nosZ, Cor_Pond_2021$DO, method = "spearman")

#ureA
cor.test(Cor_Pond_2021$ureA, Cor_Pond_2021$NH4, method = "spearman")
cor.test(Cor_Pond_2021$ureA, Cor_Pond_2021$NO3, method = "spearman")
cor.test(Cor_Pond_2021$ureA, Cor_Pond_2021$PO4, method = "spearman")
cor.test(Cor_Pond_2021$ureA, Cor_Pond_2021$DO, method = "spearman")



##Data are sorted
Correlation_Matrix_Pond2021 <- read.csv("/Users/andrewpilat/Documents/Honors/Data/NCyc/Pond2021_Correlation_Matrix_Edited.csv")
rownames(Correlation_Matrix_Pond2021) <- Correlation_Matrix_Pond2021$X
Correlation_Matrix_Pond2021 <- Correlation_Matrix_Pond2021[,-1]

Actual_Matrix_Pond2021 <- Correlation_Matrix_Pond2021[1:4,]
Actual_Matrix_Pond2021 <- as.matrix(Actual_Matrix_Pond2021)


###Pond 2022###

Pond_2022_NCyc <- read.csv("/Users/andrewpilat/Documents/Honors/Data/NCyc/Pond_2022_NCyc.csv")

Cor_Pond_2022 <- Pond_2022_NCyc[,c(6:24)]


correlation <- cor(Cor_Pond_2022, method = "spearman", use = "complete.obs")
write.csv(correlation, file = "/Users/andrewpilat/Documents/Honors/Data/NCyc/Pond2022_Correlation_Matrix.csv")


##p-values
#nifH
cor.test(Cor_Pond_2022$nifH, Cor_Pond_2022$NH4, method = "spearman")
cor.test(Cor_Pond_2022$nifH, Cor_Pond_2022$NO3, method = "spearman")
cor.test(Cor_Pond_2022$nifH, Cor_Pond_2022$PO4, method = "spearman")
cor.test(Cor_Pond_2022$nifH, Cor_Pond_2022$DO, method = "spearman")

#nirB
cor.test(Cor_Pond_2022$nirB, Cor_Pond_2022$NH4, method = "spearman")
cor.test(Cor_Pond_2022$nirB, Cor_Pond_2022$NO3, method = "spearman")
cor.test(Cor_Pond_2022$nirB, Cor_Pond_2022$PO4, method = "spearman")
cor.test(Cor_Pond_2022$nirB, Cor_Pond_2022$DO, method = "spearman")

#nirS
cor.test(Cor_Pond_2022$nirS, Cor_Pond_2022$NH4, method = "spearman")
cor.test(Cor_Pond_2022$nirS, Cor_Pond_2022$NO3, method = "spearman")
cor.test(Cor_Pond_2022$nirS, Cor_Pond_2022$PO4, method = "spearman")
cor.test(Cor_Pond_2022$nirS, Cor_Pond_2022$DO, method = "spearman")

#nirK
cor.test(Cor_Pond_2022$nirK, Cor_Pond_2022$NH4, method = "spearman")
cor.test(Cor_Pond_2022$nirK, Cor_Pond_2022$NO3, method = "spearman")
cor.test(Cor_Pond_2022$nirK, Cor_Pond_2022$PO4, method = "spearman")
cor.test(Cor_Pond_2022$nirK, Cor_Pond_2022$DO, method = "spearman")

#norB
cor.test(Cor_Pond_2022$norB, Cor_Pond_2022$NH4, method = "spearman")
cor.test(Cor_Pond_2022$norB, Cor_Pond_2022$NO3, method = "spearman")
cor.test(Cor_Pond_2022$norB, Cor_Pond_2022$PO4, method = "spearman")
cor.test(Cor_Pond_2022$norB, Cor_Pond_2022$DO, method = "spearman")

#nosZ
cor.test(Cor_Pond_2022$nosZ, Cor_Pond_2022$NH4, method = "spearman")
cor.test(Cor_Pond_2022$nosZ, Cor_Pond_2022$NO3, method = "spearman")
cor.test(Cor_Pond_2022$nosZ, Cor_Pond_2022$PO4, method = "spearman")
cor.test(Cor_Pond_2022$nosZ, Cor_Pond_2022$DO, method = "spearman")

#ureA
cor.test(Cor_Pond_2022$ureA, Cor_Pond_2022$NH4, method = "spearman")
cor.test(Cor_Pond_2022$ureA, Cor_Pond_2022$NO3, method = "spearman")
cor.test(Cor_Pond_2022$ureA, Cor_Pond_2022$PO4, method = "spearman")
cor.test(Cor_Pond_2022$ureA, Cor_Pond_2022$DO, method = "spearman")

##Data are sorted
Correlation_Matrix_Pond2022 <- read.csv("/Users/andrewpilat/Documents/Honors/Data/NCyc/Pond2022_Correlation_Matrix_Edited.csv")
rownames(Correlation_Matrix_Pond2022) <- Correlation_Matrix_Pond2022$X
Correlation_Matrix_Pond2022 <- Correlation_Matrix_Pond2022[,-1]

Actual_Matrix_Pond2022 <- Correlation_Matrix_Pond2022[1:4,]


Actual_Matrix_Pond2022 <- as.matrix(Actual_Matrix_Pond2022)


##Column annotation
Genes_Metadata <- read.csv("/Users/andrewpilat/Documents/Honors/Data/NCyc/Genes_Kokosing_NCyc.csv")

rownames(Genes_Metadata) <- Genes_Metadata$X
Genes_Metadata <- Genes_Metadata[,-1]
Genes_Metadata <- Genes_Metadata[,c(1,5:9,11)]

Genes_Metadata <- as.matrix(Genes_Metadata)

boxplot_anno_Genes <- HeatmapAnnotation(
  Abundance = anno_boxplot(
    Genes_Metadata, width = unit(3, "cm"),
    rotation = 90, axis_param = list(gp = gpar(fontsize = 10))
  ),
  name = "Abundance",
  annotation_name_side = "left",
  annotation_name_gp = gpar(fontsize = 12)
)


##Row Annotations
Chemistry_Metadata <- read.csv("/Users/andrewpilat/Documents/Honors/Data/NCyc/Kokosing_Metadata.csv")
rownames(Chemistry_Metadata) <- Chemistry_Metadata$X
Chemistry_Metadata <- Chemistry_Metadata[,-1]
Chemistry_Metadata <- Chemistry_Metadata[2:5,]

Kokosing_Chemistry <- Chemistry_Metadata[,1:17]
Pond2021_Chemistry <- Chemistry_Metadata[,18:90]
Pond2022_Chemistry <- Chemistry_Metadata[,91:166]

Kokosing_Chemistry[] <- lapply(Kokosing_Chemistry, as.numeric)
Pond2021_Chemistry[] <- lapply(Pond2021_Chemistry, as.numeric)
Pond2022_Chemistry[] <- lapply(Pond2022_Chemistry, as.numeric)

Kokosing_Chemistry <- as.matrix(Kokosing_Chemistry)
Pond2021_Chemistry <- as.matrix(Pond2021_Chemistry)
Pond2022_Chemistry <- as.matrix(Pond2022_Chemistry)

Kokosing_annotation <- HeatmapAnnotation(which = "row", Koko_Meta = anno_boxplot(Kokosing_Chemistry, na.rm = TRUE, width = unit(3, "cm"),
  rotation = 45, axis = FALSE), annotation_name_gp = gpar(col = "white", fontsize = 0))

Pond2021_annotation <- HeatmapAnnotation(which = "row", Koko_Meta = anno_boxplot(Pond2021_Chemistry, na.rm = TRUE, width = unit(3, "cm"),
                                 rotation = 45, axis = FALSE), annotation_name_gp = gpar(col = "white", fontsize = 0))

Pond2022_annotation <- HeatmapAnnotation(which = "row", Concentration = anno_boxplot(Pond2022_Chemistry, na.rm = TRUE, width = unit(3, "cm"),
                         rotation = 45, axis_param = list(gp = gpar(fontsize = 10))), annotation_name_gp = gpar(fontsize = 12), annotation_name_side = "bottom")

col_fun= colorRamp2(c(-1, 0, 1), c("blue", "#76d587", "#ffff00"))

column_split <- c(rep(1,1), rep(2, 5), rep(3,1))

ht1 = Heatmap(Actual_Matrix_Kokosing, name = "Spearman", col = col_fun, row_title = "Kokosing River",
              row_order = rownames(Actual_Matrix_Kokosing), column_order = colnames(Actual_Matrix_Kokosing), row_names_side = "left",
              column_names_rot = 45, show_heatmap_legend = FALSE, column_split = column_split, top_annotation = boxplot_anno_Genes,row_names_gp = gpar(fontsize = 10),
              column_names_gp = gpar(fontsize = 12), right_annotation = Kokosing_annotation)
ht2 = Heatmap(Actual_Matrix_Pond2021, name = "Spearman Cor.", col = col_fun, row_title = "Ponds 2021",
              row_order = rownames(Actual_Matrix_Pond2021), column_order = colnames(Actual_Matrix_Pond2021), row_names_side = "left",
              column_names_rot = 45, column_split = column_split, row_names_gp = gpar(fontsize = 10),
              column_names_gp = gpar(fontsize = 10), right_annotation = Pond2021_annotation)
ht3 = Heatmap(Actual_Matrix_Pond2022, name = "Spearmans", col = col_fun, row_title = "Ponds 2022", 
              column_title = "N Cycle Genes",                                                                                                
              row_order = rownames(Actual_Matrix_Pond2022), column_order = colnames(Actual_Matrix_Pond2022), row_names_side = "left",
              column_names_rot = 45, show_heatmap_legend = FALSE, column_split = column_split,
              row_names_gp = gpar(fontsize = 10), 
              column_names_gp = gpar(fontsize = 10), right_annotation = Pond2022_annotation)  
  
ht_list = ht1 %v% ht2 %v% ht3

tiff("Pond_Kokosing_Spearman_Heatmaps.tiff", units = "in", width =6, height=7, res=300)
draw(ht_list)
dev.off()

help("anno_boxplot")


#####Cyanobacteria######
Pond_Cyanobacteria <- read.csv("/Users/andrewpilat/Documents/Honors/Data/Cyanobacteria_Heatmap.csv")
rownames(Pond_Cyanobacteria) <- Pond_Cyanobacteria$X
Pond_Cyanobacteria <- Pond_Cyanobacteria[,-1]

Actual_Pond_Cyanobacteria <- as.matrix(Pond_Cyanobacteria)
column_split = c(1,2,3,4)

tiff("Cyanobacteria_Correlation.tiff", units = "in", width =6, height=7, res=300)
Heatmap(Actual_Pond_Cyanobacteria, name = "Spearman", col = col_fun, row_title = "Cyanobacteria Genera", column_title = "Environmental Factors", row_split = rownames(Actual_Pond_Cyanobacteria),
        column_split = column_split, row_order = rownames(Actual_Pond_Cyanobacteria), column_order = colnames(Actual_Pond_Cyanobacteria), row_names_side = "left",
        column_names_rot = 45, row_names_gp = gpar(fontsize = 12, fontface = "italic"), column_title_side = "top",
        column_names_gp = gpar(fontsize = 12))
dev.off()
