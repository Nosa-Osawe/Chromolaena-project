library(vegan)
library(tidyverse)
library(pairwiseAdonis)  

Ogua_nmds <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\Ogua_fam_nmds.csv")
view(Ogua_nmds)
attach(Ogua_nmds)
Ogua_nmds$Chromolaena_level <- factor(Ogua_nmds$Chromolaena_level)

data1_Ogua_nmds <- Ogua_nmds[,3:18]
data2_Ogua_nmds <- Ogua_nmds[,1:2]
T_data1_Ogua_nmds<- scale(data1_Ogua_nmds)
NMDS_Ogua <- metaMDS(data1_Ogua_nmds, distance = "gower", k=2, na.rm = TRUE)
NMDS_Ogua$stress
stressplot(NMDS_Ogua)

scores(NMDS_Ogua) #### very important 


# adonis2 is recommended 
fit_Ogua <- adonis2 (data1_Ogua_nmds~Chromolaena_level, 
                     data = data2_Ogua_nmds, permutations = 9999, 
                     method = "bray")
fit_Ogua    ### No sig. difference, this time

distance_data_ogua <- vegdist(data1_Ogua_nmds)
anova(betadisper(distance_data_ogua, data2_Ogua_nmds$Chromolaena_level))

data1_Ogua_nmds <- as.data.frame(NMDS_Ogua$points)
NMDS_Ogua_nmds_sc <- as.data.frame(scores(NMDS_Ogua)$species)  
combined_ <- as.data.frame(cbind(Ogua_nmds, data1_Ogua_nmds))

ogua_nmds_plot  <- ggplot() +
  geom_point(data = combined_, aes(x = MDS1, y = MDS2, 
                                   color = Chromolaena_level, 
                                   fill = Chromolaena_level,
                                   shape = Method), 
             size = 3) + 
  stat_ellipse(data = combined_,  geom = "polygon", 
               aes(x = MDS1, y = MDS2, 
                   group = Chromolaena_level, fill = Chromolaena_level),
               level = 0.95, 
               linewidth = 0.1,
               alpha = 0.1,
               show.legend = NA)+ 
  scale_fill_manual(values = c("0%" = "orange", "<50%" = "blue", ">50%" = "darkgreen")) +
  scale_color_manual(values = c("orange", "blue", "darkgreen"), 
                     breaks = c("0%", "<50%", ">50%"))+
  geom_text(data = NMDS_Ogua_nmds_sc, 
            aes(x = NMDS1, y = NMDS2, label = rownames(NMDS_Ogua_nmds_sc)))+ 
  theme(
    text = element_text(family = "Times New Roman", size = 20)
  ) + labs(x = "NMDS1", y = "NMDS2")+
  theme_minimal()
ogua_nmds_plot

################################################################################################

Igue_nmds <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\igueosagie_fam_nmds.csv")
view(Igue_nmds)
attach(Igue_nmds)
length(Igue_nmds)
Igue_nmds$Chromolaena_level <- factor(Igue_nmds$Chromolaena_level)



data1_igue_nmds <- Igue_nmds[,3:20]
data2_igue_nmds <- Igue_nmds[,1:2]

NMDS_igue <- metaMDS(data1_igue_nmds, distance = "gower", k=2, na.rm = TRUE)
NMDS_igue$stress
stressplot(NMDS_igue)

scores(NMDS_Ogua) #### very important 


# adonis2 is recommended 
fit_igue <- adonis2 (data1_igue_nmds~Chromolaena_level, 
                     data = data2_igue_nmds, permutations = 9999, 
                     method = "gower")
fit_igue    ### No sig. difference, this time

distance_data_igue <- vegdist(data1_igue_nmds)
anova(betadisper(distance_data_igue, data2_igue_nmds$Chromolaena_level))

data1_Ogua_nmds <- as.data.frame(NMDS_Ogua$points)
NMDS_Ogua_nmds_sc <- as.data.frame(scores(NMDS_Ogua)$species)  
combined_ <- as.data.frame(cbind(Ogua_nmds, data1_Ogua_nmds))

ogua_nmds_plot  <- ggplot() +
  geom_point(data = combined_, aes(x = MDS1, y = MDS2, 
                                   color = Chromolaena_level, 
                                   fill = Chromolaena_level,
                                   shape = Method), 
             size = 3) + 
  stat_ellipse(data = combined_,  geom = "polygon", 
               aes(x = MDS1, y = MDS2, 
                   group = Chromolaena_level, fill = Chromolaena_level),
               level = 0.95, 
               linewidth = 0.1,
               alpha = 0.1,
               show.legend = NA)+ 
  scale_fill_manual(values = c("0%" = "orange", "<50%" = "blue", ">50%" = "darkgreen")) +
  scale_color_manual(values = c("orange", "blue", "darkgreen"), 
                     breaks = c("0%", "<50%", ">50%"))+
  geom_text(data = NMDS_Ogua_nmds_sc, 
            aes(x = NMDS1, y = NMDS2, label = rownames(NMDS_Ogua_nmds_sc)))+ 
  theme(
    text = element_text(family = "Times New Roman", size = 20)
  ) + labs(x = "NMDS1", y = "NMDS2")+
  theme_minimal()
ogua_nmds_plot



