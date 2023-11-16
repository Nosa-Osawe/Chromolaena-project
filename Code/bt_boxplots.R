library(tidyverse)

BT_div <- read.csv("C:\\Users\\HP\\Desktop\\bt_div.csv", stringsAsFactors = TRUE)
View(BT_div)
attach(BT_div)

BT_div$Site <- factor(BT_div$Site, levels = c("0%", "<50%", ">50%"))



BT_shannon <- BT_div %>%
  select(Location, Shannon_H, Site) %>%
  mutate(Site = as.factor(Site)) %>%  # Convert Site to a factor
  ggplot() +
  geom_boxplot(aes(x = Location, y = Shannon_H, fill = Site),
               notch = FALSE,
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL,) +
  geom_point(aes(x = Location, y = Shannon_H, colour = Site),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFD700", "#ADD8E0", "#90EE90")) +  # Set fill colors
  scale_color_manual(values = c("0%" = "orange", 
                                "<50%" = "blue",
                                ">50%" = "darkgreen")) + 
  labs(x= "Location",
       y= "Shannon_H diversity index", legend = FALSE)+
  theme(
    text = element_text(family = "Times New Roman", size = 24))+# Color of the mean points
  theme_bw()
print(BT_shannon)


BT_simpson <- BT_div %>%
  select(Location, Simpson_1.D, Site) %>%
  mutate(Site = as.factor(Site)) %>%  # Convert Site to a factor
  ggplot() +
  geom_boxplot(aes(x = Location, y = Simpson_1.D, fill = Site),
               notch = FALSE,
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL,) +
  geom_point(aes(x = Location, y = Simpson_1.D, colour = Site),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFD700", "#ADD8E0", "#90EE90")) +  # Set fill colors
  scale_color_manual(values = c("0%" = "orange", 
                                "<50%" = "blue",
                                ">50%" = "darkgreen")) + 
  labs(x= "Location",
       y= "Simpson's diversity index", legend = FALSE)+
  theme(
    text = element_text(family = "Times New Roman", size = 24))+# Color of the mean points
  theme_bw()
print(BT_simpson)


BT_Margalef <- BT_div %>%
  select(Location, Margalef, Site) %>%
  mutate(Site = as.factor(Site)) %>%  # Convert Site to a factor
  ggplot() +
  geom_boxplot(aes(x = Location, y = Margalef, fill = Site),
               notch = FALSE,
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL,) +
  geom_point(aes(x = Location, y = Margalef, colour = Site),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFD700", "#ADD8E0", "#90EE90")) +  # Set fill colors
  scale_color_manual(values = c("0%" = "orange", 
                                "<50%" = "blue",
                                ">50%" = "darkgreen")) + 
  labs(x= "Location",
       y= "Margalef index", legend = FALSE)+
  theme(
    text = element_text(family = "Times New Roman", size = 24))+# Color of the mean points
  theme_bw()
print(BT_Margalef)


BT_Taxa <- BT_div %>%
  select(Location, Taxa_S, Site) %>%
  mutate(Site = as.factor(Site)) %>%  # Convert Site to a factor
  ggplot() +
  geom_boxplot(aes(x = Location, y = Taxa_S, fill = Site),
               notch = FALSE,
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL,) +
  geom_point(aes(x = Location, y = Taxa_S, colour = Site),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFD700", "#ADD8E0", "#90EE90")) +  # Set fill colors
  scale_color_manual(values = c("0%" = "orange", 
                                "<50%" = "blue",
                                ">50%" = "darkgreen")) + 
  labs(x= "Location",
       y= "Taxa richness", legend = FALSE)+
  theme(
    text = element_text(family = "Times New Roman", size = 24))+# Color of the mean points
  theme_bw()
print(BT_Taxa)


BT_evenness <- BT_div %>%
  select(Location, Evenness_e.H.S, Site) %>%
  mutate(Site = as.factor(Site)) %>%  # Convert Site to a factor
  ggplot() +
  geom_boxplot(aes(x = Location, y = Evenness_e.H.S, fill = Site),
               notch = FALSE,
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL,) +
  geom_point(aes(x = Location, y = Evenness_e.H.S, colour = Site),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFD700", "#ADD8E0", "#90EE90")) +  # Set fill colors
  scale_color_manual(values = c("0%" = "orange", 
                                "<50%" = "blue",
                                ">50%" = "darkgreen")) + 
  labs(x= "Location",
       y= "Evenness", legend = FALSE)+
  theme(
    text = element_text(family = "Times New Roman", size = 24))+# Color of the mean points
  theme_bw()
print(BT_evenness)


BT_dominance <- BT_div %>%
  select(Location, Dominance_D, Site) %>%
  mutate(Site = as.factor(Site)) %>%  # Convert Site to a factor
  ggplot() +
  geom_boxplot(aes(x = Location, y = Dominance_D, fill = Site),
               notch = FALSE,
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL,) +
  geom_point(aes(x = Location, y = Dominance_D, colour = Site),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFD700", "#ADD8E0", "#90EE90")) +  # Set fill colors
  scale_color_manual(values = c("0%" = "orange", 
                                "<50%" = "blue",
                                ">50%" = "darkgreen")) + 
  labs(x= "Location",
       y= "Dominance", legend = FALSE)+
  theme(
    text = element_text(family = "Times New Roman", size = 24))+# Color of the mean points
  theme_bw()
print(BT_dominance)



BT_abundance <- BT_div %>%
  select(Location, Individuals, Site) %>%
  mutate(Site = as.factor(Site)) %>%  # Convert Site to a factor
  ggplot() +
  geom_boxplot(aes(x = Location, y = Individuals, fill = Site),
               notch = FALSE,
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL,) +
  geom_point(aes(x = Location, y = Individuals, colour = Site),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFD700", "#ADD8E0", "#90EE90")) +  # Set fill colors
  scale_color_manual(values = c("0%" = "orange", 
                                "<50%" = "blue",
                                ">50%" = "darkgreen")) + 
  labs(x= "Location",
       y= "Species abundance", legend = FALSE)+
  theme(
    text = element_text(family = "Times New Roman", size = 24))+# Color of the mean points
  theme_bw()
print(BT_abundance)
