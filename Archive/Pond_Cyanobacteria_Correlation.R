######ARCHIVE PROGRAM NOT ASSOCIATED WITH MANUSCRIPT##################



# Author: Andrew Pilat
# History:
# Created - 2/3/25
# Discontinued - 7/31/25
# Affiliation: Kenyon College, Department of Biology



pond_data <- read.csv("/Users/andrewpilat/Documents/Honors/Data/Pond_Data_Aggregated.csv")

##DO##
cor.test(pond_data$Synechococcus, pond_data$DO, method = "spearman") ##r = 0.564, p < 0.001
cor.test(pond_data$Planktothrix, pond_data$DO, method = "spearman") ##r = 0.543, p < 0.001
cor.test(pond_data$Anabaena, pond_data$DO, method = "spearman") ##r = 0.430, p < 0.001
cor.test(pond_data$Nostoc, pond_data$DO, method = "spearman") ##r = 0.205, p < 0.015

##pH
cor.test(pond_data$Synechococcus, pond_data$pH, method = "spearman") ##r = 0.708, p < 0.001
cor.test(pond_data$Planktothrix, pond_data$pH, method = "spearman") ##r = 0.708, p < 0.001
cor.test(pond_data$Anabaena, pond_data$pH, method = "spearman") ##r = 0.495, p < 0.001
cor.test(pond_data$Nostoc, pond_data$pH, method = "spearman") ##r = 0.258, p = 0.002

##Conductivity
cor.test(pond_data$Synechococcus, pond_data$cond, method = "spearman") ##r = 0.501, p < 0.001
cor.test(pond_data$Planktothrix, pond_data$cond, method = "spearman") ## r = 0.562, p < 0.001
cor.test(pond_data$Anabaena, pond_data$cond, method = "spearman") ##r = 0.495, p < 0.001
cor.test(pond_data$Nostoc, pond_data$cond, method = "spearman") ##r = 0.258, p = 0.002

##nifH
cor.test(pond_data$Synechococcus, pond_data$nifH, method = "spearman") ##r = 0.543, p < 0.001
cor.test(pond_data$Planktothrix, pond_data$nifH, method = "spearman") ##r = 0.261, p = 0.0013
cor.test(pond_data$Anabaena, pond_data$nifH, method = "spearman") ##r = 0.368, p < 0.001
cor.test(pond_data$Nostoc, pond_data$nifH, method = "spearman") ## r = -0.130, p = 0.1125
