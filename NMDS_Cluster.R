library("vegan")
library("mosaic")
library("ggrepel")
library("scales")
library("grid")
library("ggforce")
library("gridExtra")

###NCyc######
##Load in data
Heatmap_NCyc <- read.csv("/users/andrewpilat/Documents/Honors/Data/NCyc/Heatmap_NCyc.csv")

##Select specific columns
NMDS_Ncyc <- Heatmap_NCyc[,c(2:20)]

##Function to determine the appropriate stress
NMDS.scree <- function(x) { #where x is the name of the data frame variable
  plot(rep(1, 10), replicate(10, metaMDS(x, autotransform = F, k = 1)$stress), xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# of Dimensions", ylab = "Stress", main = "NMDS stress plot", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x, autotransform = F, k = i + 1)$stress))
  }
}

##Check the stress
NMDS.scree(NMDS_Ncyc)

Ncyc_NMDS_Run <- metaMDS(NMDS_Ncyc, k =2, distance = "bray", autotransform = FALSE)


##Dataframe for ggplot. This one pulls the NMDS data
Ncyc_NMDS_Run.sites <- as.data.frame(scores(Ncyc_NMDS_Run, display = "species")) #save NMDS results into dataframe
Ncyc_NMDS_Run.sites <- cbind(Ncyc_NMDS_Run.sites, Locations = rownames(Ncyc_NMDS_Run.sites))
Ncyc_NMDS_Run.sites <- cbind(Ncyc_NMDS_Run.sites, Clusters = c(rep("Antarctica", 3), rep("Soil", 10), rep("Antarctica", 1), rep("Pond", 4), rep("Soil", 1)))
#add grouping variable of cluster grouping to dataframe


##GGplot
tiff("NCyc_Clustering.tiff", units = "in", width =10, height=10, res=300)
ggplot(Ncyc_NMDS_Run.sites, aes(x=NMDS1, y=NMDS2, size = 1, color = Clusters))+coord_cartesian(xlim=c(-1.7,1.7), ylim=c(-1.7,1.7))+ #sets up the plot
  geom_point()+scale_x_continuous(labels = scales::number_format(accuracy = 0.1))+scale_y_continuous(labels = scales::number_format(accuracy = 0.1))+
  geom_mark_ellipse(aes(group= Clusters), fill = NA, alpha = 0.2, size = 0.8)+
  theme_classic()+ geom_vline(xintercept = 0, linetype = "dashed", color = "black")+ 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  theme(legend.position = "none")+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))
dev.off()


####Genera#####
Genera_Single_Sheet <- read.csv("/users/andrewpilat/Documents/Honors/Kraken Spreadsheets/Genera_for_NMDS.csv")
rownames(Genera_Single_Sheet) <- Genera_Single_Sheet[,1]
Genera_Single_Sheet <- Genera_Single_Sheet[,-1]

##tiff("Scree_plot.tiff", units = "in", width =10, height=10, res=300)
##NMDS.scree(Genera_Single_Sheet)
##dev.off()

Taxa_NMDS_Run <- metaMDS(Genera_Single_Sheet, k =2, distance = "bray", autotransform = FALSE)

##Dataframe for ggplot. This one pulls the NMDS data
Taxa_NMDS_Run.sites <- as.data.frame(scores(Taxa_NMDS_Run, display = "species")) #save NMDS results into dataframe
Taxa_NMDS_Run.sites <- cbind(Taxa_NMDS_Run.sites, Locations = rownames(Taxa_NMDS_Run.sites))
Taxa_NMDS_Run.sites <- cbind(Taxa_NMDS_Run.sites, Clusters = c(rep("Antarctica", 3), rep("Soil", 10), rep("Pond", 5), rep("Compost", 1)))

tiff("Taxa_Clustering.tiff", units = "in", width =10, height=10, res=300)
ggplot(Taxa_NMDS_Run.sites, aes(x=NMDS1, y=NMDS2, size = 1, color = Clusters))+coord_cartesian(xlim=c(-3.5,3.5), ylim=c(-1,1))+ #sets up the plot
  geom_point()+scale_x_continuous(labels = scales::number_format(accuracy = 0.1))+scale_y_continuous(labels = scales::number_format(accuracy = 0.1))+
  geom_mark_ellipse(aes(group= Clusters), fill = NA, alpha = 0.2, size = 0.8)+theme_classic()+ geom_vline(xintercept = 0, linetype = "dashed", color = "black")+ 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  theme(legend.position = "none")+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))
dev.off()

###Looking at all sites NCyc

All_sites_Ncyc <- read.csv("/Users/andrewpilat/Documents/Honors/Data/NCyc/Full_Gene_Profile.csv")

rownames(All_sites_Ncyc) <- All_sites_Ncyc$Admera_ID
All_sites_Ncyc <- All_sites_Ncyc[,-1]

tiff("All_Genes_Scree.tiff", units = "in", width =10, height=10, res=300)
NMDS.scree(All_sites_Ncyc)
dev.off()

Ncyc_All_Genes <- metaMDS(All_sites_Ncyc, k =2, distance = "bray", autotransform = FALSE)

Ncyc_All_Genes  <- as.data.frame(scores(Ncyc_All_Genes, display = "species")) #save NMDS results into dataframe

site_Names <- read.csv("/Users/andrewpilat/Documents/Honors/Data/NCyc/Names_Column.csv")


Ncyc_All_Genes <- cbind(Ncyc_All_Genes, Locations = site_Names$Median_Name)


tiff("NCyc_Clustering_All_Sites.tiff", units = "in", width =10, height=10, res=300)
ggplot(Ncyc_All_Genes, aes(x=NMDS1, y=NMDS2, size = 1, color = Locations))+coord_cartesian(xlim=c(-3.5,3.5), ylim=c(-1,1))+ #sets up the plot
  geom_point()+scale_x_continuous(labels = scales::number_format(accuracy = 0.1))+scale_y_continuous(labels = scales::number_format(accuracy = 0.1))+
  theme_classic()+ geom_vline(xintercept = 0, linetype = "dashed", color = "black")+ 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))
dev.off()

##Didn't work##


