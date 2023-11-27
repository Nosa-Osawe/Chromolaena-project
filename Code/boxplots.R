library(tidyverse)

PT_diversity <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\Pt_diversity.csv", 
                         stringsAsFactors = TRUE)
View(PT_diversity)
attach(PT_diversity)

PT_diversity$Site <- factor(PT_diversity$Site, levels = c("0%", "<50%", ">50%"))


PT_shannon <- PT_diversity %>%
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
  scale_fill_manual(values = c("#FFE699", "#D6EBF2", "#B4F7B4")) +  # Set fill colors
  scale_color_manual(values = c("0%" = "orange", 
                                "<50%" = "blue",
                                ">50%" = "darkgreen")) + 
  labs(x= "Location",
       y= "Shannon_H diversity index", legend = FALSE)+
  theme(
    text = element_text(family = "Times New Roman", size = 24))+# Color of the mean points
  theme_bw()
print(PT_shannon)



PT_simpson <- PT_diversity %>%
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
  scale_fill_manual(values = c("#FFE699", "#D6EBF2", "#B4F7B4")) +  # Set fill colors
  scale_color_manual(values = c("0%" = "orange", 
                                "<50%" = "blue",
                                ">50%" = "darkgreen")) + 
  labs(x= "Location",
       y= "Simpson's diversity index", legend = FALSE)+
  theme(
    text = element_text(family = "Times New Roman", size = 24))+# Color of the mean points
  theme_bw()
print(PT_simpson)


PT_Margalef <- PT_diversity %>%
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
  scale_fill_manual(values = c("#FFE699", "#D6EBF2", "#B4F7B4")) +  # Set fill colors
  scale_color_manual(values = c("0%" = "orange", 
                                "<50%" = "blue",
                                ">50%" = "darkgreen")) + 
  labs(x= "Location",
       y= "Margalef index", legend = FALSE)+
  theme(
    text = element_text(family = "Times New Roman", size = 24))+# Color of the mean points
  theme_bw()
print(PT_Margalef)


PT_Taxa <- PT_diversity %>%
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
  scale_fill_manual(values = c("#FFE699", "#D6EBF2", "#B4F7B4")) +  # Set fill colors
  scale_color_manual(values = c("0%" = "orange", 
                                "<50%" = "blue",
                                ">50%" = "darkgreen")) + 
  labs(x= "Location",
       y= "Taxa richness", legend = FALSE)+
  theme(
    text = element_text(family = "Times New Roman", size = 24))+# Color of the mean points
  theme_bw()
print(PT_Taxa)


PT_evenness <- PT_diversity %>%
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
  scale_fill_manual(values = c("#FFE699", "#D6EBF2", "#B4F7B4")) +  # Set fill colors
  scale_color_manual(values = c("0%" = "orange", 
                                "<50%" = "blue",
                                ">50%" = "darkgreen")) + 
  labs(x= "Location",
       y= "Evenness", legend = FALSE)+
  theme(
    text = element_text(family = "Times New Roman", size = 24))+# Color of the mean points
  theme_bw()
print(PT_evenness)


PT_dominance <- PT_diversity %>%
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
  scale_fill_manual(values = c("#FFE699", "#D6EBF2", "#B4F7B4")) +  # Set fill colors
  scale_color_manual(values = c("0%" = "orange", 
                                "<50%" = "blue",
                                ">50%" = "darkgreen")) + 
  labs(x= "Location",
       y= "Dominance", legend = FALSE)+
  theme(
    text = element_text(family = "Times New Roman", size = 24))+# Color of the mean points
  theme_bw()
print(PT_dominance)



PT_abundance <- PT_diversity %>%
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
  scale_fill_manual(values = c("#FFE699", "#D6EBF2", "#B4F7B4")) +  # Set fill colors
  scale_color_manual(values = c("0%" = "orange", 
                                "<50%" = "blue",
                                ">50%" = "darkgreen")) + 
  labs(x= "Location",
       y= "Species abundance", legend = FALSE)+
  theme(
    text = element_text(family = "Times New Roman", size = 24))+# Color of the mean points
  theme_bw()
print(PT_abundance)

#########################################################################################################3
