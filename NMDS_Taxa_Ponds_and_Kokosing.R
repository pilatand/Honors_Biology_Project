###NMDS.scree source: https://ourcodingclub.github.io/tutorials/ordination/
###Documentation for NMDS: https://www.rpubs.com/RGrieger/545184


library("vegan")
library("mosaic")

NMDS.scree <- function(x) { #where x is the name of the data frame variable
  plot(rep(1, 10), replicate(10, metaMDS(x, autotransform = F, k = 1)$stress), xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# of Dimensions", ylab = "Stress", main = "NMDS stress plot", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x, autotransform = F, k = i + 1)$stress))
  }
}


##2021 Pond Community Structure

##Load in the data
Data2021 <- read.csv("/Users/andrewpilat/Documents/Honors/Data/Data2021.csv")
Class_2021_data <- Data2021[,33:57]
row.names(Class_2021_data) <- Data2021$Family
Relevant_Metadata_2021 <- Data2021[,c(4:11)]
row.names(Relevant_Metadata_2021) <- Data2021$Family


##Fit the MDS
MDS_2021 <- metaMDS(Class_2021_data, k =4, distance = "bray", autotransform = FALSE)
MDS_2021 <- MDSrotate(MDS_2021, Relevant_Metadata_2021$temp)

##Fit the environmnetal variables
MDS_2021_Env_Fit <- envfit(MDS_2021, Relevant_Metadata_2021, permutations = 999)

help("envfit")

##Dataframe for ggplot. This one pulls the NMDS data
site.scrs <- as.data.frame(scores(MDS_2021, display = "sites")) #save NMDS results into dataframe
site.scrs <- cbind(site.scrs, genera = rownames(site.scrs))
#add grouping variable of cluster grouping to dataframe

##Pulling the environmental data
env.scores.2021 <- as.data.frame(scores(MDS_2021_Env_Fit, display = "vectors")) #extracts relevant scores from envifit
env.scores.2021 <- cbind(env.scores.2021, env.variables = rownames(env.scores.2021)) #and then gives them their names

env.scores.2021 <- cbind(env.scores.2021, pval = MDS_2021_Env_Fit$vectors$pvals) # add pvalues to dataframe
sig.env.scrs.2021 <- subset(env.scores.2021, pval<=0.05) #subset data to show variables significant at 0.05


env.scores.2021$highlight <- c("Highlight", "Normal", "Normal", "Highlight","Normal","Normal","Normal")
env.scores.2021$env.variables <- c("pH", "Cond", "Temp", "Tannin", "PO4", "NO3", "NH4")


Nitrogen <- env.scores.2021[7:8,]
DO <- env.scores.2021[4,]
Other <- env.scores.2021[c(1:3, 5:6),]

##GGplot
master2021 <- ggplot(site.scrs, aes(x=NMDS1, y=NMDS2))+ #sets up the plot
  geom_point(aes(x=NMDS1, y=NMDS2, size = 0.6), color = "black")+ #adds site points to plot, shape determined by Landuse, colour determined by Management
  coord_cartesian(xlim=c(-1.5,1.5), ylim=c(-1.5,1.5))+scale_x_continuous(labels = scales::number_format(accuracy = 0.1))+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))+geom_text_repel(data = site.scrs, fontface="bold",
                                                                                     aes(x = NMDS1, y = NMDS2, label = genera), color="black", nudge_y = -0.02)+
  geom_rect(aes(xmin=0.01, xmax = 0.18, ymin = -0.37, ymax = -0.15), fill = NA, color = "black")+
  geom_segment(aes(x=0.01, y= -0.15, xend = 0.4, yend = -0.68), color = "black", linetype = "dashed")+
  geom_segment(aes(x=0.18, y= -0.15, xend = 1.4, yend = -0.64), color = "black", linetype = "dashed")+
  theme_classic()+ geom_vline(xintercept = 0, linetype = "dashed", color = "black")+ 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  theme(legend.position = "none")+
  geom_segment(
    data = Other,
    aes(
      x = 0,
      y = 0,
      xend = NMDS1,
      yend = NMDS2, 
    ), color="black",
    arrow = arrow(length = unit(0.25, "cm")
    )) + geom_segment(
      data = Nitrogen, linewidth = 1.2,
      aes(
        x = 0,
        y = 0, 
        xend = NMDS1,
        yend = NMDS2, 
      ), color="red",
      arrow = arrow(length = unit(0.5, "cm")
      ))+ geom_segment(
        data = DO, linewidth = 1.2,
        aes(
          x = 0,
          y = 0, 
          xend = NMDS1,
          yend = NMDS2, 
        ), color="#36a207",
        arrow = arrow(length = unit(0.5, "cm")
        ))+
  geom_text_repel(
    data = env.scores.2021,
    aes(x = NMDS1, y = NMDS2, label = env.scores.2021$env.variables), nudge_x = -0.05, nudge_y = 0.005,
    color = "blue", fontface= "bold"
  ) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))

inset_plot <- ggplot(site.scrs, aes(x=NMDS1, y=NMDS2, size = 0.6))+ #sets up the plot
  geom_point(aes(x=NMDS1, y=NMDS2), color = "black")+ #adds site points to plot, shape determined by Landuse, colour determined by Management
  coord_cartesian(xlim=c(0.07, 0.17), ylim=c(-0.38, -0.17))+
  geom_text_repel(data = site.scrs, fontface="bold",
                  aes(x = NMDS1, y = NMDS2, label = genera), color="black", nudge_y = 0.005)+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  theme(legend.position = "none", axis.title = element_blank(), axis.text = element_blank(), panel.grid = element_blank(),
        axis.ticks = element_blank(), axis.line = element_blank(), panel.border = element_blank())

inset_grob <- ggplotGrob(inset_plot)
plot_with_inset <- master2021 +
  annotation_custom(
    grob = inset_grob,
    xmin = 0.3, xmax = 1.4,  # Adjust these values based on your main plot's limits
    ymin = -1.5, ymax = -0.6     # Adjust these values based on your main plot's limits
  )

tiff("2021 NMDS Community Inlet Labels", units = "in", width =10, height=9, res=300)
print(plot_with_inset)
dev.off()


site.scrs$Location <- Data2021$Pond

tiff("Ponds_2021_Sites.tiff", units = "in", width =10, height=10, res=300)
ggplot(site.scrs, aes(x=NMDS1, y=NMDS2, size = 0.6, color = Location))+coord_cartesian(xlim=c(-1.5,1.5), ylim=c(-1.5,1.5))+ #sets up the plot
  geom_point()+scale_x_continuous(labels = scales::number_format(accuracy = 0.1))+scale_y_continuous(labels = scales::number_format(accuracy = 0.1))+
  theme_classic()+ geom_vline(xintercept = 0, linetype = "dashed", color = "black")+scale_color_manual(values = c("#F8766D","#7CAE00", "#00BFC4", "#C77CFF"))+ 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))+theme(legend.position = "none")+
  geom_segment(
    data = Other,linewidth = 1.2,
    aes(
      x = 0,
      y = 0,
      xend = NMDS1,
      yend = NMDS2, 
    ), color="black",
    arrow = arrow(length = unit(0.5, "cm")
    )) + geom_segment(
      data = Nitrogen, linewidth = 1.2,
      aes(
        x = 0,
        y = 0, 
        xend = NMDS1,
        yend = NMDS2, 
      ), color="red",
      arrow = arrow(length = unit(0.5, "cm")
      ))+ geom_segment(
        data = DO, linewidth = 1.2,
        aes(
          x = 0,
          y = 0, 
          xend = NMDS1,
          yend = NMDS2, 
        ), color="#36a207",
        arrow = arrow(length = unit(0.5, "cm")
        ))
dev.off()

tiff("BoxplotTest.tiff", units = "in", width =10, height=10, res=300)
boxplot(Data2021$NO3 ~ Data2021$Pond)
dev.off()


##2022 Pond Community Structure##
Data2022 <- read.csv("/Users/andrewpilat/Documents/Honors/Data/Data2022.csv")
Data2022 <- Data2022[1:77,]
Class_2022_data <- Data2022[,32:56]
Relevant_Metadata_2022 <- Data2022[,c(4:11)]


##Check the stress
NMDS.scree(Class_2022_data)

##Fit the MDS
MDS_2022 <- metaMDS(Class_2022_data, k=4,distance = "bray", autotransform = FALSE)
MDS_2022 <- MDSrotate(MDS_2022, Relevant_Metadata_2022$temp)

##Fit the environmnetal variables
MDS_2022_Env_Fit <- envfit(MDS_2022, Relevant_Metadata_2022, permutations = 999, na.rm = TRUE)
head(MDS_2022_Env_Fit)


##Dataframe for ggplot. This one pulls the NMDS data
site.scrs.2022 <- as.data.frame(scores(MDS_2022, display = "sites")) #save NMDS results into dataframe
site.scrs.2022 <- cbind(site.scrs.2022, genera = rownames(site.scrs.2022))
#add grouping variable of cluster grouping to dataframe

##Pulling the environmental data
env.scores.2022 <- as.data.frame(scores(MDS_2022_Env_Fit, display = "vectors")) #extracts relevant scores from envifit
env.scores.2022 <- cbind(env.scores.2022, env.variables = rownames(env.scores.2022)) #and then gives them their names

env.scores.2022 <- cbind(env.scores.2022, pval = MDS_2022_Env_Fit$vectors$pvals) # add pvalues to dataframe
sig.env.scrs.2022 <- subset(env.scores.2022, pval<=0.05) #subset data to show variables significant at 0.05

env.scores.2022$env.variables <- c("pH", "Cond", "Temp", "Tannin", "PO4", "NO3", "NH4")
env.scores.2022$highlight <- c("Highlight",  "Normal","Normal","Highlight","Normal","Normal","Normal")

Nitrogen <- env.scores.2022[7:8,]
DO <- env.scores.2022[4,]
Other <- env.scores.2022[c(1:3, 5:6),]

##GGplot

master2022 <- ggplot(site.scrs.2022, aes(x=NMDS1, y=NMDS2))+scale_y_continuous(labels = scales::number_format(accuracy = 0.1))+ #sets up the plot
  geom_point(aes(x=NMDS1, y=NMDS2), color = "black")+ #adds site points to plot, shape determined by Landuse, colour determined by Management
  coord_cartesian(xlim=c(-1.5,1.5), ylim=c(-1.5,1.5))+scale_x_continuous(labels = scales::number_format(accuracy = 0.1))+
  geom_text_repel(data = site.scrs.2022, fontface="bold", nudge_y = 0.01,
                  aes(x = NMDS1, y = NMDS2, label = genera), color="black")+
  geom_rect(aes(xmin=-0.34, xmax = 0.12, ymin = -0.3, ymax = -0.04), fill = NA, color = "black")+
  geom_segment(aes(x=-0.32, y= -0.05, xend = 0.05, yend = -0.68), color = "black", linetype = "dashed")+
  geom_segment(aes(x=0.12, y= -0.05, xend = 1.5, yend = -0.64), color = "black", linetype = "dashed")+
  theme_classic()+ geom_vline(xintercept = 0, linetype = "dashed", color = "black")+ 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  theme(legend.position = "none")+
  geom_segment(
    data = Other,
    aes(
      x = 0,
      y = 0,
      xend = NMDS1,
      yend = NMDS2, 
    ), color="black",
    arrow = arrow(length = unit(0.25, "cm")
    )) + geom_segment(
        data = Nitrogen, linewidth = 1.2,
        aes(
          x = 0,
          y = 0, 
          xend = NMDS1,
          yend = NMDS2, 
        ), color="red",
        arrow = arrow(length = unit(0.5, "cm")
        ))+ geom_segment(
          data = DO, linewidth = 1.2,
          aes(
            x = 0,
            y = 0, 
            xend = NMDS1,
            yend = NMDS2, 
          ), color="#36a207",
          arrow = arrow(length = unit(0.5, "cm")
          ))+
  geom_text_repel(
    data = env.scores.2022,fontface="bold",
    aes(x = NMDS1, y = NMDS2, label = env.scores.2022$env.variables), nudge_y = 0.01,
    color = "blue"
  ) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))

inset_plot <- ggplot(site.scrs.2022, aes(x=NMDS1, y=NMDS2, size = 0.6))+ #sets up the plot
  geom_point(aes(x=NMDS1, y=NMDS2), color = "black")+ #adds site points to plot, shape determined by Landuse, colour determined by Management
  coord_cartesian(xlim=c(-0.3, 0.1), ylim=c(-0.27, -0.06))+
  geom_text_repel(data = site.scrs.2022, fontface="bold",
                  aes(x = NMDS1, y = NMDS2, label = genera), color="black")+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  theme(legend.position = "none", axis.title = element_blank(), axis.text = element_blank(), panel.grid = element_blank(),
        axis.ticks = element_blank(), axis.line = element_blank(), panel.border = element_blank())

inset_grob <- ggplotGrob(inset_plot)
plot_with_inset <- master2022 +
  annotation_custom(
    grob = inset_grob,
    xmin = 0, xmax = 1.5,  # Adjust these values based on your main plot's limits
    ymin = -1.5, ymax = -0.6     # Adjust these values based on your main plot's limits
  )

tiff("2022 NMDS Community Inlet Labels", units = "in", width =10, height=9, res=300)
print(plot_with_inset)
dev.off()

site.scrs.2022$Location <- Data2022$Pond


tiff("Ponds_2022_Sites.tiff", units = "in", width =10, height=10, res=300)
ggplot(site.scrs.2022, aes(x=NMDS1, y=NMDS2, size = 0.6, color = Location))+coord_cartesian(xlim=c(-1.5,1.5), ylim=c(-1.5,1.5))+ #sets up the plot
  geom_point()+scale_x_continuous(labels = scales::number_format(accuracy = 0.1))+scale_y_continuous(labels = scales::number_format(accuracy = 0.1))+
  theme_classic()+ geom_vline(xintercept = 0, linetype = "dashed", color = "black")+scale_color_manual(values = c("#F8766D","#7CAE00", "#00BFC4", "#C77CFF"))+ 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))+theme(legend.position = "none")+
  geom_segment(
    data = Other,linewidth = 1.2,
    aes(
      x = 0,
      y = 0,
      xend = NMDS1,
      yend = NMDS2, 
    ), color="black",
    arrow = arrow(length = unit(0.25, "cm")
    )) + geom_segment(
      data = Nitrogen, linewidth = 1.2,
      aes(
        x = 0,
        y = 0, 
        xend = NMDS1,
        yend = NMDS2, 
      ), color="red",
      arrow = arrow(length = unit(0.5, "cm")
      ))+ geom_segment(
        data = DO, linewidth = 1.2,
        aes(
          x = 0,
          y = 0, 
          xend = NMDS1,
          yend = NMDS2, 
        ), color="#36a207",
        arrow = arrow(length = unit(0.5, "cm")
        ))
dev.off()





#####Kokosing Genera######

Kokosing<- read.csv("/Users/andrewpilat/Documents/Honors/Data/Kokosing_Data.csv")

Kokosing <- Kokosing[1:18,]
rownames(Kokosing) <- Kokosing$Table.for.all.samples

Kokosing_Genera <- Kokosing[,10:35] 
Kokosing_Metadata <-Kokosing[,3:9]

NMDS.scree(Kokosing_Genera)


Kokosing_NMDS <- metaMDS(Kokosing_Genera, k =3, distance = "bray", autotransform = FALSE)
Kokosing_NMDS <- MDSrotate(Kokosing_NMDS, Kokosing_Metadata$Temp)
Kokosing_NMDS_Real <- as.data.frame(scores(Kokosing_NMDS, display = "species")) #save NMDS results into dataframe
Kokosing_NMDS_Real$Location <- Kokosing$Location


Kokosing_Env_Fit <- envfit(Kokosing_NMDS, Kokosing_Metadata, permutations = 999)


##Pulling the environmental data
Kokosing_env_scores <- as.data.frame(scores(Kokosing_Env_Fit, display = "vectors")) #extracts relevant scores from envifit
Kokosing_env_scores  <- cbind(Kokosing_env_scores , env.variables = rownames(Kokosing_env_scores)) #and then gives them their names

Kokosing_env_scores <- cbind(Kokosing_env_scores, pval = Kokosing_Env_Fit$vectors$pvals) # add pvalues to dataframe
sig.env.scrs.Kokosing <- subset(Kokosing_env_scores, pval<=0.05) #subset data to show variables significant at 0.05

Nitrogen <- Kokosing_env_scores[c(1,3),]
DO <- Kokosing_env_scores[5,]
Other <- Kokosing_env_scores[c(2,4,6:7),]

tiff("Kokosing_NMDS_Labels.tiff", units = "in", width =10, height=10, res=300)
ggplot(Kokosing_NMDS_Real, aes(x=NMDS1, y=NMDS2, size = 0.6, color = Location))+coord_cartesian(xlim=c(-1,1), ylim=c(-1,1))+ #sets up the plot
  geom_point()+scale_x_continuous(labels = scales::number_format(accuracy = 0.1))+scale_y_continuous(labels = scales::number_format(accuracy = 0.1))+
  theme_classic()+ geom_vline(xintercept = 0, linetype = "dashed", color = "black")+ 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))+theme(legend.position = "none")+
  geom_segment(
    data = Other, linewidth = 1.2,
    aes(
      x = 0,
      y = 0,
      xend = NMDS1,
      yend = NMDS2, 
    ), color="black",
    arrow = arrow(length = unit(0.25, "cm")
    ))+geom_segment(
      data = Nitrogen, linewidth = 1.2,
      aes(
        x = 0,
        y = 0,
        xend = NMDS1,
        yend = NMDS2, 
      ), color="red",
      arrow = arrow(length = unit(0.25, "cm")
      ))+geom_segment(
        data = DO, linewidth = 1.2,
        aes(
          x = 0,
          y = 0,
          xend = NMDS1,
          yend = NMDS2, 
        ), color = "#36a207",
        arrow = arrow(length = unit(0.25, "cm")
        ))+
  geom_text_repel(
      data = Kokosing_env_scores,
      aes(x = NMDS1, y = NMDS2, label = Kokosing_env_scores$env.variables), nudge_x = -0.05, nudge_y = 0.005,
      color = "blue", fontface= "bold",
    )+geom_text_repel(data = Kokosing_NMDS_Real, fontface="bold",
                      aes(x = NMDS1, y = NMDS2, label = rownames(Kokosing_NMDS_Real)), color="black", nudge_y = -0.02)
dev.off()

tiff("Kokosing_NMDS_NoLabels.tiff", units = "in", width =10, height=10, res=300)
ggplot(Kokosing_NMDS_Real, aes(x=NMDS1, y=NMDS2, size = 0.6, color = Location))+coord_cartesian(xlim=c(-1,1), ylim=c(-1,1))+ #sets up the plot
  geom_point()+scale_x_continuous(labels = scales::number_format(accuracy = 0.1))+scale_y_continuous(labels = scales::number_format(accuracy = 0.1))+
  theme_classic()+ geom_vline(xintercept = 0, linetype = "dashed", color = "black")+ 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))+theme(legend.position = "none")+
  geom_segment(
    data = Kokosing_env_scores, linewidth = 1.2,
    aes(
      x = 0,
      y = 0,
      xend = NMDS1,
      yend = NMDS2, 
    ), color="black",
    arrow = arrow(length = unit(0.25, "cm")
    ))+geom_segment(
      data = Nitrogen, linewidth = 1.2,
      aes(
        x = 0,
        y = 0,
        xend = NMDS1,
        yend = NMDS2, 
      ), color="red",
      arrow = arrow(length = unit(0.25, "cm")
      ))+geom_segment(
        data = DO, linewidth = 1.2,
        aes(
          x = 0,
          y = 0,
          xend = NMDS1,
          yend = NMDS2, 
        ), color = "#36a207",
        arrow = arrow(length = unit(0.25, "cm")
        ))
dev.off()








