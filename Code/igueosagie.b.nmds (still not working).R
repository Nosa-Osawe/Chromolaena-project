library(pairwiseAdonis)
library(vegan)

infestation_colors <- c(
  "Zero" = "#4DAF4A",    
  "Mild" = "#FFD92F",    
  "High" = "#E41A1C"     
)


iguegosagie.b <- full_Data %>% 
  filter(Village == "Iguegosagie",
         Collection_method == "Beating tray") %>%
  dplyr::select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) > 0)) %>%
  mutate(Season_Infestation = paste(Seasons, Infestation_Gradient, sep = "_")) %>% 
  filter(if_any(where(is.numeric), ~ .x > 0)) %>% 
  as.data.frame()

Iguegosagie.b_nmds <- iguegosagie.b %>% 
  ungroup() %>% 
  dplyr::select(where(is.numeric))

Iguegosagie.b_nmds.c <- iguegosagie.b %>% 
  ungroup() %>% 
  dplyr::select(where(Negate(is.numeric)))


Iguegosagie.b_nmds_hell <- decostand(Iguegosagie.b_nmds, method = "hellinger")
Iguegosagie.b_bray_nmds <- metaMDS(Iguegosagie.b_nmds_hell,
                                   distance = "bray", k=2, na.rm = TRUE)

Iguegosagie.b_bray_nmds$stress
stressplot(Iguegosagie.b_bray_nmds)

scores(Iguegosagie.b_bray_nmds) 


Iguegosagie.b_distance<- vegdist(Iguegosagie.b_nmds_hell, method = "bray")
anova(betadisper(Iguegosagie.b_distance, 
                 Iguegosagie.b_nmds.c$Season_Infestation )) # PERMDISP

adonis2 (Iguegosagie.b_nmds~Season_Infestation, 
         data = Iguegosagie.b_nmds.c, permutations = 9999, 
         method = "bray")

Iguegosagie.b.pairwise <- pairwise.adonis(Iguegosagie.b_distance,
                                          Iguegosagie.b_nmds.c$Season_Infestation)
Iguegosagie.b.pairwise


Iguegosagie.b_bray_nmds_points <- as.data.frame(Iguegosagie.b_bray_nmds$points)
Iguegosagie.b_bray_nmds_species <- as.data.frame(scores(Iguegosagie.b_bray_nmds)$species)  
Iguegosagie.b_comb <- as.data.frame(cbind(Iguegosagie.b_bray_nmds_points, 
                                          Iguegosagie.b_nmds.c))

ggplot() +
  geom_point(data = Iguegosagie.b_comb, aes(x = MDS1, y = MDS2, 
                                            color = Infestation_Gradient, 
                                            fill = Infestation_Gradient,
                                            alpha = 0.95, shape = Seasons), 
             size = 3) + 
  scale_colour_manual(values = infestation_colors)+
  scale_fill_manual(values = infestation_colors)+
  theme(
    text = element_text(family = "Times New Roman", size = 20)
  ) + labs(x = "NMDS1", y = "NMDS2")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")+
  theme_minimal()+
  stat_ellipse(data = Iguegosagie.b_comb, 
               aes(x = MDS1, y = MDS2, 
                   group = Season_Infestation, 
                   color = Infestation_Gradient), 
               geom = "path", 
               level = 0.90, 
               linewidth = 0.7,   
               show.legend = NA)+
  stat_ellipse(data = Iguegosagie.b_comb,  geom = "polygon", 
               aes(x = MDS1, y = MDS2, 
                   group = Season_Infestation, fill = Infestation_Gradient),
               level = 0.90, 
               linewidth = 0.1,
               alpha = 0.1,
               show.legend = NA)+ 
  guides(
    color = guide_legend(title = "Infestation gradient"),   
    fill = "none",
    size = "none",
    alpha = "none"
  )


