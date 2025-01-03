library("vegan")
library("mosaic")
library("ggrepel")
library("scales")
library("grid")
library("ggforce")
library("gridExtra")

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
Ncyc_NMDS_Run.sites <- cbind(Ncyc_NMDS_Run.sites, Clusters = c(rep("Antarctica", 3), rep("Soil", 9), rep("Antarctica", 1), rep("Pond", 4), rep("Soil", 1)))
#add grouping variable of cluster grouping to dataframe


##GGplot
tiff("NCyc_Clustering.tiff", units = "in", width =10, height=10, res=300)
ggplot(Ncyc_NMDS_Run.sites, aes(x=NMDS1, y=NMDS2, size = 1, color = Clusters))+coord_cartesian(xlim=c(-1.5,1.56), ylim=c(-1.5,1.5))+ #sets up the plot
  geom_point()+scale_x_continuous(labels = scales::number_format(accuracy = 0.1))+scale_y_continuous(labels = scales::number_format(accuracy = 0.1))+
  geom_mark_ellipse(aes(group= Clusters), fill = NA, alpha = 0.2, size = 0.8)+
  theme_classic()+ geom_vline(xintercept = 0, linetype = "dashed", color = "black")+ 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  theme(legend.position = "none")+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))
dev.off()


