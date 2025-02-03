compost_trail <- read.csv("/Users/andrewpilat/Documents/Honors/Data/Compost_Trail.csv")

wilcox.test(compost_trail$Thermomicrobia ~ compost_trail$Sample.Location)
wilcox.test(compost_trail$Mycolicibacterium ~ compost_trail$Sample.Location, paired = FALSE)
wilcox.test(compost_trail$Mycobacterium ~ compost_trail$Sample.Location)
wilcox.test(compost_trail$Bradyrhizobium ~ compost_trail$Sample.Location)


compost_trail <- compost_trail[1:6,]

##Bradyrhizobium
cor.test(compost_trail$Bradyrhizobium, compost_trail$nirB, method = "spearman") ## p = 0.007666, r = 0.927
cor.test(compost_trail$Bradyrhizobium, compost_trail$nirS, method = "spearman") ## p= 0.04986, r = -0.8816
cor.test(compost_trail$Bradyrhizobium, compost_trail$nirK, method = "spearman") ## NS
cor.test(compost_trail$Bradyrhizobium, compost_trail$norB, method = "spearman") ## NS
cor.test(compost_trail$Bradyrhizobium, compost_trail$nosZ, method = "spearman") ## p = 0.04986, r = -0.811679

##Rhodoplanes
cor.test(compost_trail$Rhodoplanes, compost_trail$nirB, method = "spearman") ## p = 0.036, r = 0.84
cor.test(compost_trail$Rhodoplanes, compost_trail$nirS, method = "spearman")  ##NS
cor.test(compost_trail$Rhodoplanes, compost_trail$nirK, method = "spearman") ## NS
cor.test(compost_trail$Rhodoplanes, compost_trail$norB, method = "spearman") ## NS
cor.test(compost_trail$Rhodoplanes, compost_trail$nosZ, method = "spearman") ## NS

##Achromobacter
cor.test(compost_trail$Achromobacter, compost_trail$nirB, method = "spearman") ## NS
cor.test(compost_trail$Achromobacter, compost_trail$nirS, method = "spearman")  ## p = 0.04986, r = -8.11
cor.test(compost_trail$Achromobacter, compost_trail$nirK, method = "spearman") ## NS
cor.test(compost_trail$Achromobacter, compost_trail$norB, method = "spearman") ## NS
cor.test(compost_trail$Achromobacter, compost_trail$nosZ, method = "spearman") ## NS

##Sphaerobacter
cor.test(compost_trail$Sphaerobacter, compost_trail$nirB, method = "spearman") ## p = 0.0076, r = -0.928
cor.test(compost_trail$Sphaerobacter, compost_trail$nirS, method = "spearman") ## p= 0.04986, r = -0.8816
cor.test(compost_trail$Sphaerobacter, compost_trail$nirK, method = "spearman") ## NS
cor.test(compost_trail$Sphaerobacter, compost_trail$norB, method = "spearman") ## NS
cor.test(compost_trail$Sphaerobacter, compost_trail$nosZ, method = "spearman")


wetlands <- read.csv("/Users/andrewpilat/Documents/Honors/Data/Wetland_Genera_Genes.csv")

##Bradyrhizobium
cor.test(wetlands$Bradyrhizobium, wetlands$nirB, method = "spearman") ## NS
cor.test(wetlands$Bradyrhizobium, wetlands$nirS, method = "spearman") ## p= 0.00003499, r = -0.6184
cor.test(wetlands$Bradyrhizobium, wetlands$nirK, method = "spearman") ## p = 0.003766, r = -0.5209
cor.test(wetlands$Bradyrhizobium, wetlands$norB, method = "spearman") ## p = 0.00032, r = 0.621
cor.test(wetlands$Bradyrhizobium, wetlands$nosZ, method = "spearman") ## p = 0.00032, r  = -0.621

##Rhodoplanes
cor.test(wetlands$Rhodoplanes, wetlands$nirB, method = "spearman") ## p = 0.02106, r = 0.429
cor.test(wetlands$Rhodoplanes, wetlands$nirS, method = "spearman")  ##NS
cor.test(wetlands$Rhodoplanes, wetlands$nirK, method = "spearman") ## p-value = 0.007058, r = 0.48934
cor.test(wetlands$Rhodoplanes, wetlands$norB, method = "spearman") ## NS
cor.test(wetlands$Rhodoplanes, wetlands$nosZ, method = "spearman") ## NS

##Achromobacter
cor.test(wetlands$Achromobacter, wetlandsl$nirB, method = "spearman") ## NS
cor.test(wetlands$Achromobacter, wetlands$nirS, method = "spearman")  ## p < 0.001, r = 0.736
cor.test(wetlands$Achromobacter, wetlands$nirK, method = "spearman") ## p < 0.005, r = 0.507
cor.test(wetlands$Achromobacter, wetlands$norB, method = "spearman") ## p < 0.001, r = -0.5858
cor.test(wetlands$Achromobacter, wetlands$nosZ, method = "spearman") ## p < 0.001, r = 0.637


