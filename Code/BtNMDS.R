rm(list = ls()) # clear environment
cat("\014") # clear console
install.packages("pairwiseAdonis")

library(vegan)
library(tidyverse)
library(pairwiseAdonis)  

OguaBTnmDS <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\oguaNMDS_pt.csv")
View(OguaBTnmDS)
sum(is.na(OguaBTnmDS))
data1_ogua <- OguaBTnmDS[,3:20]
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
data1_ogua_n <- as.data.frame(lapply(data1_ogua, normalize))
data2_ogua <- OguaBTnmDS[,1:2]

NMDS_ogua_n <- metaMDS(data1_ogua, distance = "bray", k=2)

NMDS_ogua_n$stress
########################################################################################

igue_pt <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\IgueNMDS_pt.csv")
igue_pt$Site<- factor(igue_pt$Site, levels = c("0%", "<50%", ">50%"))
data1_igue_pt <- igue_pt[,3:18]
data2_igue_pt <- igue_pt[,1:2]
NMDS_igue_pt <- metaMDS(data1_igue_pt, distance = "bray", k=2)
NMDS_igue_pt$stress
stressplot(NMDS_igue_pt)

scores(NMDS_igue_pt) #### very important 

plot(NMDS_igue_pt$points)
plot(as.data.frame(NMDS_igue_pt$points), geom = "text", legend = "none")

# adonis2 is recommended 
fit_igue_pt <- adonis2 (data1_igue_pt~Site, data = data2_igue_pt, permutations = 999, method = "bray")
fit_igue_pt

distance_data_igue_pt <- vegdist(data1_igue_pt)
anova(betadisper(distance_data_igue_pt, data2_igue_pt$Site))

igue_pt_nmds <- as.data.frame(NMDS_igue_pt$points)

# Create a ggplot2 plot
ggplot(igue_pt, aes(x = igue_pt_nmds$MDS1, y = igue_pt_nmds$MDS2, color = Site)) +
  geom_point(size = 3) +  # Customize the point size
  labs(x = "NMDS1", y = "NMDS2") +  # Set axis labels
  scale_fill_manual(values = c("0%" = "orange", "<50%" = "blue", ">50%" = "darkgreen")) +  # Set fill colors
  scale_color_manual(values = c("0%" = "orange", 
                                "<50%" = "blue",
                                ">50%" = "darkgreen")) +  # Specify colors
  theme_minimal()+
  stat_ellipse(geom = "polygon", aes(group = Site, fill = Site),
               level = 0.95, 
               linewidth = 0.1,
               alpha = 0.05, show.legend = NA)+
  theme(
    text = element_text(family = "Times New Roman", size = 20)  # Set font to Times New Roman and font size to 14
  )+
  theme_classic()
##########################################################################################


ogua_pt <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\oguaNMDS_pt.csv")
ogua_pt$Site<- factor(ogua_pt$Site, levels = c("0%", "<50%", ">50%"))

data1_ogua_pt <- ogua_pt[,3:21]
data2_ogua_pt <- ogua_pt[,1:2]
NMDS_ogua_pt <- metaMDS(data1_ogua_pt, distance = "bray", k=2)
NMDS_ogua_pt$stress



# adonis2 is recommended 
fit_ogua_pt  <- adonis2 (data1_ogua_pt~Site, data = data2_ogua_pt, permutations = 999, method = "bray")
fit_ogua_pt 

distance_data_ogua_pt <- vegdist(data1_ogua_pt)
anova(betadisper(distance_data_ogua_pt, data2_ogua_pt$Site))

ogua_pt_nmds <- as.data.frame(NMDS_ogua_pt$points)


# Create a ggplot2 plot
ggplot(ogua_pt, aes(x = ogua_pt_nmds$MDS1, y = ogua_pt_nmds$MDS2, color = Site)) +
  geom_point(size = 3) +  # Customize the point size
  labs(x = "NMDS1", y = "NMDS2") +  # Set axis labels
  scale_fill_manual(values = c("0%" = "orange", "<50%" = "blue", ">50%" = "darkgreen")) +  # Set fill colors
  scale_color_manual(values = c("0%" = "orange", 
                                "<50%" = "blue",
                                ">50%" = "darkgreen")) +  # Specify colors
  theme_minimal()+
  stat_ellipse(geom = "polygon", aes(group = Site, fill = Site),
               level = 0.95, 
               size = 0.1,
               alpha = 0.05, show.legend = NA)+
  theme(
    text = element_text(family = "Times New Roman", size = 20)  # Set font to Times New Roman and font size to 14
  )+
  theme_classic()