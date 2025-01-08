library(mosaic)
library(ComplexHeatmap) ##From Bioconductor
library(circlize)

###Kokosing
Kokosing_NCyc <- read.csv("/Users/andrewpilat/Documents/Honors/Data/NCyc/Kokosing_River_NCyc.csv")

Cor_Kokosing <- Kokosing_NCyc[,c(6:23)]

correlation <- cor(Cor_Kokosing, method = "spearman")
write.csv(correlation, file = "/Users/andrewpilat/Documents/Honors/Data/NCyc/Kokosing_Correlation_Matrix.csv")

##Data are sorted
Correlation_Matrix_Kokosing <- read.csv("/Users/andrewpilat/Documents/Honors/Data/NCyc/Kokosing_Correlation_Matrix_Edited.csv")
rownames(Correlation_Matrix_Kokosing) <- Correlation_Matrix_Kokosing$X
Correlation_Matrix_Kokosing <- Correlation_Matrix_Kokosing[,-1]

Actual_Matrix_Kokosing <- as.matrix(Correlation_Matrix_Kokosing )


###Pond 2021####
Pond_2021_NCyc <- read.csv("/Users/andrewpilat/Documents/Honors/Data/NCyc/Pond_2021_Nitrogen.csv")

Cor_Pond_2021 <- Pond_2021_NCyc[,c(6:24)]

correlation <- cor(Cor_Pond_2021, method = "spearman")
write.csv(correlation, file = "/Users/andrewpilat/Documents/Honors/Data/NCyc/Pond2021_Correlation_Matrix.csv")

##Data are sorted
Correlation_Matrix_Pond2021 <- read.csv("/Users/andrewpilat/Documents/Honors/Data/NCyc/Pond2021_Correlation_Matrix_Edited.csv")
rownames(Correlation_Matrix_Pond2021) <- Correlation_Matrix_Pond2021$X
Correlation_Matrix_Pond2021 <- Correlation_Matrix_Pond2021[,-1]

Actual_Matrix_Pond2021 <- Correlation_Matrix_Pond2021[1:7,]
Actual_Matrix_Pond2021 <- as.matrix(Actual_Matrix_Pond2021)


###Pond 2022###

Pond_2022_NCyc <- read.csv("/Users/andrewpilat/Documents/Honors/Data/NCyc/Pond_2022_NCyc.csv")

Cor_Pond_2022 <- Pond_2022_NCyc[,c(6:24)]


correlation <- cor(Cor_Pond_2022, method = "spearman", use = "complete.obs")
write.csv(correlation, file = "/Users/andrewpilat/Documents/Honors/Data/NCyc/Pond2022_Correlation_Matrix.csv")

##Data are sorted
Correlation_Matrix_Pond2022 <- read.csv("/Users/andrewpilat/Documents/Honors/Data/NCyc/Pond2022_Correlation_Matrix_Edited.csv")
rownames(Correlation_Matrix_Pond2022) <- Correlation_Matrix_Pond2022$X
Correlation_Matrix_Pond2022 <- Correlation_Matrix_Pond2022[,-1]

Actual_Matrix_Pond2022 <- Correlation_Matrix_Pond2022[1:7,]


Actual_Matrix_Pond2022 <- as.matrix(Actual_Matrix_Pond2022)


##Column annotation
Genes_Metadata <- read.csv("/Users/andrewpilat/Documents/Honors/Data/NCyc/Genes_Kokosing_NCyc.csv")

rownames(Genes_Metadata) <- Genes_Metadata$X
Genes_Metadata <- Genes_Metadata[,-1]

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

Kokosing_Chemistry <- Chemistry_Metadata[,1:17]
Pond2021_Chemistry <- Chemistry_Metadata[,18:90]
Pond2022_Chemistry <- Chemistry_Metadata[,91:166]

Kokosing_Chemistry[] <- lapply(Kokosing_Chemistry, as.numeric)
Pond2021_Chemistry[] <- lapply(Pond2021_Chemistry, as.numeric)
Pond2022_Chemistry[] <- lapply(Pond2022_Chemistry, as.numeric)

Kokosing_Chemistry <- as.matrix(Kokosing_Chemistry[-1,])
Pond2021_Chemistry <- as.matrix(Pond2021_Chemistry[-1,])
Pond2022_Chemistry <- as.matrix(Pond2022_Chemistry[-1,])

Kokosing_annotation <- HeatmapAnnotation(which = "row", Koko_Meta = anno_boxplot(Kokosing_Chemistry, na.rm = TRUE, width = unit(3, "cm"),
  rotation = 45, axis = FALSE), annotation_name_gp = gpar(col = "white", fontsize = 0))

Pond2021_annotation <- HeatmapAnnotation(which = "row", Koko_Meta = anno_boxplot(Pond2021_Chemistry, na.rm = TRUE, width = unit(3, "cm"),
                                 rotation = 45, axis = FALSE), annotation_name_gp = gpar(col = "white", fontsize = 0))

Pond2022_annotation <- HeatmapAnnotation(which = "row", Concentration = anno_boxplot(Pond2022_Chemistry, na.rm = TRUE, width = unit(3, "cm"),
                         rotation = 45, axis_param = list(gp = gpar(fontsize = 10))), annotation_name_gp = gpar(fontsize = 12), annotation_name_side = "bottom")

col_fun= colorRamp2(c(-1, 0, 1), c("blue", "#76d587", "#ffff00"))

column_split <- c(rep(1,1), rep(2, 3), rep(3,1), rep(4, 4), rep(5, 1), rep(6,1))

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
