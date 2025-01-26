Denitrifers_Ncyc <- read.csv("/Users/andrewpilat/Documents/Honors/Kraken Spreadsheets/Denitrifiers_out.csv")

Denitrifier_Kokosing <- Denitrifers_Ncyc[1:18,]
Denitrifier_Data2021 <- Denitrifers_Ncyc[19:90,]
Denitrifier_Data2022 <- Denitrifers_Ncyc[91:167,]
Denitrifier_Wetlands <- Denitrifers_Ncyc[168:182,]

rownames(Denitrifier_Kokosing) <- Denitrifier_Kokosing$Specific_Info
Denitrifier_Kokosing  <- Denitrifier_Kokosing[, c(3:7, 9:15)]

rownames(Denitrifier_Data2021 ) <- Denitrifier_Data2021$Table.for.all.samples
Denitrifier_Data2021  <- Denitrifier_Data2021[, c(3:7, 9:15)]

rownames(Denitrifier_Data2022) <- Denitrifier_Data2022$Table.for.all.samples
Denitrifier_Data2022 <- Denitrifier_Data2022[, c(3:7, 9:15)]



rownames(Denitrifier_Wetlands) <- Denitrifier_Wetlands$Table.for.all.samples
Denitrifier_Wetlands  <- Denitrifier_Wetlands[, c(3:7, 9:15)]



Kokosing_correlation <- cor(Denitrifier_Kokosing, method = "spearman")
write.csv(Kokosing_correlation, "/Users/andrewpilat/Documents/Honors/Data/Kokosing_Denitrifer_Correlation.csv")

#Bacillus
cor.test(Denitrifier_Kokosing$Bacillus,Denitrifier_Kokosing$Nitrate, method = "spearman")
cor.test(Denitrifier_Kokosing$Bacillus,Denitrifier_Kokosing$DO, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Kokosing$Bacillus,Denitrifier_Kokosing$nirS,method = "spearman")
cor.test(Denitrifier_Kokosing$Bacillus,Denitrifier_Kokosing$nirK, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Kokosing$Bacillus,Denitrifier_Kokosing$norB, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Kokosing$Bacillus,Denitrifier_Kokosing$nosZ, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Kokosing$Bacillus,Denitrifier_Kokosing$nirB, use = "complete.obs", method = "spearman")

#Paracoccus
cor.test(Denitrifier_Kokosing$Paracoccus,Denitrifier_Kokosing$Nitrate, method = "spearman")
cor.test(Denitrifier_Kokosing$Paracoccus,Denitrifier_Kokosing$DO, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Kokosing$Paracoccus,Denitrifier_Kokosing$nirS,method = "spearman")
cor.test(Denitrifier_Kokosing$Paracoccus,Denitrifier_Kokosing$nirK,method = "spearman")
cor.test(Denitrifier_Kokosing$Paracoccus,Denitrifier_Kokosing$norB, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Kokosing$Paracoccus,Denitrifier_Kokosing$nirB, use = "complete.obs", method = "spearman")

#Pseudomonas
cor.test(Denitrifier_Kokosing$Pseudomonas,Denitrifier_Kokosing$Nitrate, method = "spearman")
cor.test(Denitrifier_Kokosing$Pseudomonas,Denitrifier_Kokosing$DO, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Kokosing$Pseudomonas,Denitrifier_Kokosing$nirS,method = "spearman")
cor.test(Denitrifier_Kokosing$Pseudomonas,Denitrifier_Kokosing$nirK,method = "spearman")
cor.test(Denitrifier_Kokosing$Pseudomonas,Denitrifier_Kokosing$norB, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Kokosing$Pseudomonas,Denitrifier_Kokosing$nosZ, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Kokosing$Pseudomonas,Denitrifier_Kokosing$nirB, use = "complete.obs", method = "spearman")

#Bradyrhizobium
cor.test(Denitrifier_Kokosing$Bradyrhizobium,Denitrifier_Kokosing$Nitrate, method = "spearman")
cor.test(Denitrifier_Kokosing$Bradyrhizobium, Denitrifier_Kokosing$DO, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Kokosing$Bradyrhizobium,Denitrifier_Kokosing$nirS,method = "spearman")
cor.test(Denitrifier_Kokosing$Bradyrhizobium,Denitrifier_Kokosing$nirK,method = "spearman")
cor.test(Denitrifier_Kokosing$Bradyrhizobium,Denitrifier_Kokosing$norB, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Kokosing$Bradyrhizobium,Denitrifier_Kokosing$nosZ, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Kokosing$Bradyrhizobium,Denitrifier_Kokosing$nosZ, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Kokosing$Bradyrhizobium,Denitrifier_Kokosing$nirB, use = "complete.obs", method = "spearman")

#Flavobacterium
cor.test(Denitrifier_Kokosing$Flavobacterium,Denitrifier_Kokosing$Nitrate, method = "spearman")
cor.test(Denitrifier_Kokosing$Flavobacterium, Denitrifier_Kokosing$DO, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Kokosing$Flavobacterium,Denitrifier_Kokosing$nirS,method = "spearman")
cor.test(Denitrifier_Kokosing$Flavobacterium,Denitrifier_Kokosing$nirK,method = "spearman")
cor.test(Denitrifier_Kokosing$Flavobacterium,Denitrifier_Kokosing$norB, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Kokosing$Flavobacterium,Denitrifier_Kokosing$nosZ, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Kokosing$Flavobacterium,Denitrifier_Kokosing$nirB, use = "complete.obs", method = "spearman")


Data2021_correlation <- cor(Denitrifier_Data2021, method = "spearman")
write.csv(Data2021_correlation, "/Users/andrewpilat/Documents/Honors/Data/Pond2021_Denitrifer_Correlation.csv")

#Bacillus
cor.test(Denitrifier_Data2021$Bacillus,Denitrifier_Data2021$Nitrate, method = "spearman")
cor.test(Denitrifier_Data2021$Bacillus,Denitrifier_Data2021$DO, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2021$Bacillus,Denitrifier_Data2021$nirS,method = "spearman")
cor.test(Denitrifier_Data2021$Bacillus,Denitrifier_Data2021$nirK, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2021$Bacillus,Denitrifier_Data2021$norB, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2021$Bacillus,Denitrifier_Data2021$nosZ, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2021$Bacillus,Denitrifier_Data2021$nirB, use = "complete.obs", method = "spearman")


#Paracoccus
cor.test(Denitrifier_Data2021$Paracoccus,Denitrifier_Data2021$Nitrate, method = "spearman")
cor.test(Denitrifier_Data2021$Paracoccus,Denitrifier_Data2021$DO, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2021$Paracoccus,Denitrifier_Data2021$nirS,method = "spearman")
cor.test(Denitrifier_Data2021$Paracoccus,Denitrifier_Data2021$nirK,method = "spearman")
cor.test(Denitrifier_Data2021$Paracoccus,Denitrifier_Data2021$norB, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2021$Paracoccus,Denitrifier_Data2021$nosZ, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2021$Paracoccus,Denitrifier_Data2021$nirB, use = "complete.obs", method = "spearman")

#Pseudomonas
cor.test(Denitrifier_Data2021$Pseudomonas,Denitrifier_Data2021$Nitrate, method = "spearman")
cor.test(Denitrifier_Data2021$Pseudomonas,Denitrifier_Data2021$DO, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2021$Pseudomonas,Denitrifier_Data2021$nirS,method = "spearman")
cor.test(Denitrifier_Data2021$Pseudomonas,Denitrifier_Data2021$nirK,method = "spearman")
cor.test(Denitrifier_Data2021$Pseudomonas,Denitrifier_Data2021$norB, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2021$Pseudomonas,Denitrifier_Data2021$nosZ, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2021$Pseudomonas,Denitrifier_Data2021$nirB, use = "complete.obs", method = "spearman")

#Bradyrhizobium
cor.test(Denitrifier_Data2021$Bradyrhizobium,Denitrifier_Data2021$Nitrate, method = "spearman")
cor.test(Denitrifier_Data2021$Bradyrhizobium, Denitrifier_Data2021$DO, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2021$Bradyrhizobium,Denitrifier_Data2021$nirS,method = "spearman")
cor.test(Denitrifier_Data2021$Bradyrhizobium,Denitrifier_Data2021$nirK,method = "spearman")
cor.test(Denitrifier_Data2021$Bradyrhizobium,Denitrifier_Data2021$norB, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2021$Bradyrhizobium,Denitrifier_Data2021$nosZ, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2021$Bradyrhizobium,Denitrifier_Data2021$nirB, use = "complete.obs", method = "spearman")

#Flavobacterium
cor.test(Denitrifier_Data2021$Flavobacterium,Denitrifier_Data2021$Nitrate, method = "spearman")
cor.test(Denitrifier_Data2021$Flavobacterium, Denitrifier_Data2021$DO, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2021$Flavobacterium,Denitrifier_Data2021$nirS,method = "spearman")
cor.test(Denitrifier_Data2021$Flavobacterium,Denitrifier_Data2021$nirK,method = "spearman")
cor.test(Denitrifier_Data2021$Flavobacterium,Denitrifier_Data2021$norB, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2021$Flavobacterium,Denitrifier_Data2021$nosZ, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2021$Flavobacterium,Denitrifier_Data2021$nirB, use = "complete.obs", method = "spearman")



Data2022_correlation <- cor(Denitrifier_Data2022, method = "spearman", use = "complete.obs")
write.csv(Data2022_correlation, "/Users/andrewpilat/Documents/Honors/Data/Pond2022_Denitrifer_Correlation.csv")

#Bacillus
cor.test(Denitrifier_Data2022$Bacillus,Denitrifier_Data2022$Nitrate, method = "spearman")
cor.test(Denitrifier_Data2022$Bacillus,Denitrifier_Data2022$DO, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2022$Bacillus,Denitrifier_Data2022$nirS,method = "spearman")
cor.test(Denitrifier_Data2022$Bacillus,Denitrifier_Data2022$nirK, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2022$Bacillus,Denitrifier_Data2022$norB, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2022$Bacillus,Denitrifier_Data2022$nosZ, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2022$Bacillus,Denitrifier_Data2022$nirB, use = "complete.obs", method = "spearman")

#Paracoccus
cor.test(Denitrifier_Data2022$Paracoccus,Denitrifier_Data2022$Nitrate, method = "spearman")
cor.test(Denitrifier_Data2022$Paracoccus,Denitrifier_Data2022$DO, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2022$Paracoccus,Denitrifier_Data2022$nirS,method = "spearman")
cor.test(Denitrifier_Data2022$Paracoccus,Denitrifier_Data2022$nirK,method = "spearman")
cor.test(Denitrifier_Data2022$Paracoccus,Denitrifier_Data2022$norB, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2022$Paracoccus,Denitrifier_Data2022$nosZ, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2022$Paracoccus,Denitrifier_Data2022$nirB, use = "complete.obs", method = "spearman")

#Pseudomonas
cor.test(Denitrifier_Data2022$Pseudomonas,Denitrifier_Data2022$Nitrate, method = "spearman")
cor.test(Denitrifier_Data2022$Pseudomonas,Denitrifier_Data2022$DO, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2022$Pseudomonas,Denitrifier_Data2022$nirS,method = "spearman")
cor.test(Denitrifier_Data2022$Pseudomonas,Denitrifier_Data2022$nirK,method = "spearman")
cor.test(Denitrifier_Data2022$Pseudomonas,Denitrifier_Data2022$norB, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2022$Pseudomonas,Denitrifier_Data2022$nosZ, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2022$Pseudomonas,Denitrifier_Data2022$nirB, use = "complete.obs", method = "spearman")

#Bradyrhizobium
cor.test(Denitrifier_Data2022$Bradyrhizobium,Denitrifier_Data2022$Nitrate, method = "spearman")
cor.test(Denitrifier_Data2022$Bradyrhizobium, Denitrifier_Data2022$DO, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2022$Bradyrhizobium,Denitrifier_Data2022$nirS,method = "spearman")
cor.test(Denitrifier_Data2022$Bradyrhizobium,Denitrifier_Data2022$nirK,method = "spearman")
cor.test(Denitrifier_Data2022$Bradyrhizobium,Denitrifier_Data2022$norB, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2022$Bradyrhizobium,Denitrifier_Data2022$nosZ, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2022$Bradyrhizobium,Denitrifier_Data2022$nirB, use = "complete.obs", method = "spearman")

#Flavobacterium
cor.test(Denitrifier_Data2022$Flavobacterium,Denitrifier_Data2022$Nitrate, method = "spearman")
cor.test(Denitrifier_Data2022$Flavobacterium, Denitrifier_Data2022$DO, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2022$Flavobacterium,Denitrifier_Data2022$nirS,method = "spearman")
cor.test(Denitrifier_Data2022$Flavobacterium,Denitrifier_Data2022$nirK,method = "spearman")
cor.test(Denitrifier_Data2022$Flavobacterium,Denitrifier_Data2022$norB, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2022$Flavobacterium,Denitrifier_Data2022$nosZ, use = "complete.obs", method = "spearman")
cor.test(Denitrifier_Data2022$Flavobacterium,Denitrifier_Data2022$nirB, use = "complete.obs", method = "spearman")


relative_abundance_Koksoing <- Denitrifier_Kokosing[, c(1:5)]
boxplot(relative_abundance_Koksoing)

relative_abundance_2021 <- Denitrifier_Data2021[, c(1:5)]
boxplot(relative_abundance_2021)

relative_abundance_2022 <- Denitrifier_Data2022[, c(1:4)]
boxplot(relative_abundance_2022)
