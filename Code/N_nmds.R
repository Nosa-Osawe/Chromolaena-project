library(pairwiseAdonis)
library(vegan)

infestation_colors <- c(
  "Zero" = "#4DAF4A",    
  "Mild" = "#FFD92F",    
  "High" = "#E41A1C"     
)


iguegosagie.p <- full_Data %>% 
  filter(Village== "Iguegosagie",
         Collection_method== "Pitfall") %>%
  dplyr::select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) > 0)) %>%
  mutate(Season_Infestation = paste(Seasons,
                                    Infestation_Gradient, 
                                    sep = "_"))

Iguegosagie.p_nmds <- iguegosagie.p %>% 
  ungroup() %>% 
  dplyr::select(where(is.numeric))

Iguegosagie.p_nmds.c <- iguegosagie.p %>% 
  ungroup() %>% 
  dplyr::select(where(Negate(is.numeric)))


Iguegosagie.p_nmds_hell <- decostand(Iguegosagie.p_nmds, method = "hellinger")
Iguegosagie.p_bray_nmds <- metaMDS(Iguegosagie.p_nmds_hell,
                                 distance = "bray", k=2, na.rm = TRUE)

Iguegosagie.p_bray_nmds$stress
stressplot(Iguegosagie.p_bray_nmds)

scores(Iguegosagie.p_bray_nmds) 


Iguegosagie.p_distance<- vegdist(Iguegosagie.p_nmds_hell, method = "bray")
anova(betadisper(Iguegosagie.p_distance, 
                 Iguegosagie.p_nmds.c$Season_Infestation )) # PERMDISP

adonis2 (Iguegosagie.p_nmds~Season_Infestation, 
         data = Iguegosagie.p_nmds.c, permutations = 9999, 
         method = "bray")

Iguegosagie.p.pairwise <- pairwise.adonis(Iguegosagie.p_distance,
                             Iguegosagie.p_nmds.c$Season_Infestation)
Iguegosagie.p.pairwise


Iguegosagie.p_bray_nmds_points <- as.data.frame(Iguegosagie.p_bray_nmds$points)
Iguegosagie.p_bray_nmds_species <- as.data.frame(scores(Iguegosagie.p_bray_nmds)$species)  
Iguegosagie.p_comb <- as.data.frame(cbind(Iguegosagie.p_bray_nmds_points, 
                                Iguegosagie.p_nmds.c))

ggplot() +
  geom_point(data = Iguegosagie.p_comb, aes(x = MDS1, y = MDS2, 
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
  stat_ellipse(data = Iguegosagie.p_comb, 
               aes(x = MDS1, y = MDS2, 
                   group = Season_Infestation, 
                   color = Infestation_Gradient), 
               geom = "path", 
               level = 0.90, 
               linewidth = 0.7,   
               show.legend = NA)+
  stat_ellipse(data = Iguegosagie.p_comb,  geom = "polygon", 
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



# OGUA


Ogua.b <- full_Data %>% 
  filter(Village == "Ogua",
         Collection_method == "Beating tray") %>%
  dplyr::select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) > 0)) %>%
  mutate(Season_Infestation = paste(Seasons, Infestation_Gradient, sep = "_")) %>% 
  filter(if_any(where(is.numeric), ~ .x > 0)) %>% 
  as.data.frame()

Ogua.b_nmds <- Ogua.b %>% 
  ungroup() %>% 
  dplyr::select(where(is.numeric))

Ogua.b_nmds.c <- Ogua.b %>% 
  ungroup() %>% 
  dplyr::select(where(Negate(is.numeric)))


Ogua.b_nmds_hell <- decostand(Ogua.b_nmds, method = "hellinger")
Ogua.b_bray_nmds <- metaMDS(Ogua.b_nmds_hell,
                            distance = "bray", k=2, na.rm = TRUE)

Ogua.b_bray_nmds$stress
stressplot(Ogua.b_bray_nmds)

scores(Ogua.b_bray_nmds) 


Ogua.b_distance<- vegdist(Ogua.b_nmds_hell, method = "bray")
anova(betadisper(Ogua.b_distance, 
                 Ogua.b_nmds.c$Season_Infestation )) # PERMDISP

adonis2 (Ogua.b_nmds~Season_Infestation, 
         data = Ogua.b_nmds.c, permutations = 9999, 
         method = "bray")

Ogua.b.pairwise <- pairwise.adonis(Ogua.b_distance,
                                   Ogua.b_nmds.c$Season_Infestation)
Ogua.b.pairwise


Ogua.b_bray_nmds_points <- as.data.frame(Ogua.b_bray_nmds$points)
Ogua.b_bray_nmds_species <- as.data.frame(scores(Ogua.b_bray_nmds)$species)  
Ogua.b_comb <- as.data.frame(cbind(Ogua.b_bray_nmds_points, 
                                   Ogua.b_nmds.c))

ggplot() +
  geom_point(data = Ogua.b_comb, aes(x = MDS1, y = MDS2, 
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
  stat_ellipse(data = Ogua.b_comb, 
               aes(x = MDS1, y = MDS2, 
                   group = Season_Infestation, 
                   color = Infestation_Gradient), 
               geom = "path", 
               level = 0.90, 
               linewidth = 0.7,   
               show.legend = NA)+
  stat_ellipse(data = Ogua.b_comb,  geom = "polygon", 
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





Ogua.p <- full_Data %>% 
  filter(Village == "Ogua",
         Collection_method == "Pitfall") %>%
  dplyr::select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) > 0)) %>%
  mutate(Season_Infestation = paste(Seasons, Infestation_Gradient, sep = "_")) %>% 
  # filter(if_any(where(is.numeric), ~ .x > 0)) %>% 
  as.data.frame()

Ogua.p_nmds <- Ogua.p %>% 
  ungroup() %>% 
  dplyr::select(where(is.numeric))

Ogua.p_nmds.c <- Ogua.p %>% 
  ungroup() %>% 
  dplyr::select(where(Negate(is.numeric)))


Ogua.p_nmds_hell <- decostand(Ogua.p_nmds, method = "hellinger")
Ogua.p_bray_nmds <- metaMDS(Ogua.p_nmds_hell,
                            distance = "bray", k=2, na.rm = TRUE)

Ogua.p_bray_nmds$stress
stressplot(Ogua.p_bray_nmds)

scores(Ogua.p_bray_nmds) 


Ogua.p_distance<- vegdist(Ogua.p_nmds_hell, method = "bray")
anova(betadisper(Ogua.p_distance, 
                 Ogua.p_nmds.c$Season_Infestation )) # PERMDISP

adonis2 (Ogua.p_nmds~Season_Infestation, 
         data = Ogua.p_nmds.c, permutations = 9999, 
         method = "bray")

Ogua.p.pairwise <- pairwise.adonis(Ogua.p_distance,
                                   Ogua.p_nmds.c$Season_Infestation)
Ogua.p.pairwise


Ogua.p_bray_nmds_points <- as.data.frame(Ogua.p_bray_nmds$points)
Ogua.p_bray_nmds_species <- as.data.frame(scores(Ogua.p_bray_nmds)$species)  
Ogua.p_comb <- as.data.frame(cbind(Ogua.p_bray_nmds_points, 
                                   Ogua.p_nmds.c))

ggplot() +
  geom_point(data = Ogua.p_comb, aes(x = MDS1, y = MDS2, 
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
  stat_ellipse(data = Ogua.p_comb, 
               aes(x = MDS1, y = MDS2, 
                   group = Season_Infestation, 
                   color = Infestation_Gradient), 
               geom = "path", 
               level = 0.90, 
               linewidth = 0.7,   
               show.legend = NA)+
  stat_ellipse(data = Ogua.p_comb,  geom = "polygon", 
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

############   

# Iguovbiobo


Iguovbiobo.p <- full_Data %>% 
  filter(Village == "Iguovbiobo",
         Collection_method == "Pitfall") %>%
  dplyr::select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) > 0)) %>%
  mutate(Season_Infestation = paste(Seasons, Infestation_Gradient, sep = "_")) %>% 
  # filter(if_any(where(is.numeric), ~ .x > 0)) %>% 
  as.data.frame()

Iguovbiobo.p_nmds <- Iguovbiobo.p %>% 
  ungroup() %>% 
  dplyr::select(where(is.numeric))

Iguovbiobo.p_nmds.c <- Iguovbiobo.p %>% 
  ungroup() %>% 
  dplyr::select(where(Negate(is.numeric)))


Iguovbiobo.p_nmds_hell <- decostand(Iguovbiobo.p_nmds, method = "hellinger")
Iguovbiobo.p_bray_nmds <- metaMDS(Iguovbiobo.p_nmds_hell,
                                  distance = "bray", k=2, na.rm = TRUE)

Iguovbiobo.p_bray_nmds$stress
stressplot(Iguovbiobo.p_bray_nmds)

scores(Iguovbiobo.p_bray_nmds) 


Iguovbiobo.p_distance<- vegdist(Iguovbiobo.p_nmds_hell, method = "bray")
anova(betadisper(Iguovbiobo.p_distance, 
                 Iguovbiobo.p_nmds.c$Season_Infestation )) # PERMDISP

adonis2 (Iguovbiobo.p_nmds~Season_Infestation, 
         data = Iguovbiobo.p_nmds.c, permutations = 9999, 
         method = "bray")

Iguovbiobo.p.pairwise <- pairwise.adonis(Iguovbiobo.p_distance,
                                         Iguovbiobo.p_nmds.c$Season_Infestation)
Iguovbiobo.p.pairwise


Iguovbiobo.p_bray_nmds_points <- as.data.frame(Iguovbiobo.p_bray_nmds$points)
Iguovbiobo.p_bray_nmds_species <- as.data.frame(scores(Iguovbiobo.p_bray_nmds)$species)  
Iguovbiobo.p_comb <- as.data.frame(cbind(Iguovbiobo.p_bray_nmds_points, 
                                         Iguovbiobo.p_nmds.c))

ggplot() +
  geom_point(data = Iguovbiobo.p_comb, aes(x = MDS1, y = MDS2, 
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
  stat_ellipse(data = Iguovbiobo.p_comb, 
               aes(x = MDS1, y = MDS2, 
                   group = Season_Infestation, 
                   color = Infestation_Gradient), 
               geom = "path", 
               level = 0.90, 
               linewidth = 0.7,   
               show.legend = NA)+
  stat_ellipse(data = Iguovbiobo.p_comb,  geom = "polygon", 
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




Iguovbiobo.b <- full_Data %>% 
  filter(Village == "Iguovbiobo",
         Collection_method == "Beating tray") %>%
  dplyr::select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) > 0)) %>%
  mutate(Season_Infestation = paste(Seasons, Infestation_Gradient, sep = "_")) %>% 
  # filter(if_any(where(is.numeric), ~ .x > 0)) %>% 
  as.data.frame()

Iguovbiobo.b_nmds <- Iguovbiobo.b %>% 
  ungroup() %>% 
  dplyr::select(where(is.numeric))

Iguovbiobo.b_nmds.c <- Iguovbiobo.b %>% 
  ungroup() %>% 
  dplyr::select(where(Negate(is.numeric)))


Iguovbiobo.b_nmds_hell <- decostand(Iguovbiobo.b_nmds, method = "hellinger")
Iguovbiobo.b_bray_nmds <- metaMDS(Iguovbiobo.b_nmds_hell,
                                  distance = "bray", k=2, na.rm = TRUE)

Iguovbiobo.b_bray_nmds$stress
stressplot(Iguovbiobo.b_bray_nmds)

scores(Iguovbiobo.b_bray_nmds) 


Iguovbiobo.b_distance<- vegdist(Iguovbiobo.b_nmds_hell, method = "bray")
anova(betadisper(Iguovbiobo.b_distance, 
                 Iguovbiobo.b_nmds.c$Season_Infestation )) # PERMDISP

adonis2 (Iguovbiobo.b_nmds~Season_Infestation, 
         data = Iguovbiobo.b_nmds.c, permutations = 9999, 
         method = "bray")

Iguovbiobo.b.pairwise <- pairwise.adonis(Iguovbiobo.b_distance,
                                         Iguovbiobo.b_nmds.c$Season_Infestation)
Iguovbiobo.b.pairwise


Iguovbiobo.b_bray_nmds_points <- as.data.frame(Iguovbiobo.b_bray_nmds$points)
Iguovbiobo.b_bray_nmds_species <- as.data.frame(scores(Iguovbiobo.b_bray_nmds)$species)  
Iguovbiobo.b_comb <- as.data.frame(cbind(Iguovbiobo.b_bray_nmds_points, 
                                         Iguovbiobo.b_nmds.c))

ggplot() +
  geom_point(data = Iguovbiobo.b_comb, aes(x = MDS1, y = MDS2, 
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
  stat_ellipse(data = Iguovbiobo.b_comb, 
               aes(x = MDS1, y = MDS2, 
                   group = Season_Infestation, 
                   color = Infestation_Gradient), 
               geom = "path", 
               level = 0.90, 
               linewidth = 0.7,   
               show.legend = NA)+
  stat_ellipse(data = Iguovbiobo.b_comb,  geom = "polygon", 
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


############################################################################3

#   Ahor-Urokosa


Ahor_Urokosa.p <- full_Data %>% 
  filter(Village == "Ahor-Urokosa",
         Collection_method == "Pitfall") %>%
  dplyr::select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) > 0)) %>%
  mutate(Season_Infestation = paste(Seasons, Infestation_Gradient, sep = "_")) %>% 
  # filter(if_any(where(is.numeric), ~ .x > 0)) %>% 
  as.data.frame()

Ahor_Urokosa.p_nmds <- Ahor_Urokosa.p %>% 
  ungroup() %>% 
  dplyr::select(where(is.numeric))

Ahor_Urokosa.p_nmds.c <- Ahor_Urokosa.p %>% 
  ungroup() %>% 
  dplyr::select(where(Negate(is.numeric)))


Ahor_Urokosa.p_nmds_hell <- decostand(Ahor_Urokosa.p_nmds, method = "hellinger")
Ahor_Urokosa.p_bray_nmds <- metaMDS(Ahor_Urokosa.p_nmds_hell,
                                    distance = "bray", k=2, na.rm = TRUE)

Ahor_Urokosa.p_bray_nmds$stress
stressplot(Ahor_Urokosa.p_bray_nmds)

scores(Ahor_Urokosa.p_bray_nmds) 


Ahor_Urokosa.p_distance<- vegdist(Ahor_Urokosa.p_nmds_hell, method = "bray")
anova(betadisper(Ahor_Urokosa.p_distance, 
                 Ahor_Urokosa.p_nmds.c$Season_Infestation )) # PERMDISP

adonis2 (Ahor_Urokosa.p_nmds~Season_Infestation, 
         data = Ahor_Urokosa.p_nmds.c, permutations = 9999, 
         method = "bray")

Ahor_Urokosa.p.pairwise <- pairwise.adonis(Ahor_Urokosa.p_distance,
                                           Ahor_Urokosa.p_nmds.c$Season_Infestation)
Ahor_Urokosa.p.pairwise


Ahor_Urokosa.p_bray_nmds_points <- as.data.frame(Ahor_Urokosa.p_bray_nmds$points)
Ahor_Urokosa.p_bray_nmds_species <- as.data.frame(scores(Ahor_Urokosa.p_bray_nmds)$species)  
Ahor_Urokosa.p_comb <- as.data.frame(cbind(Ahor_Urokosa.p_bray_nmds_points, 
                                           Ahor_Urokosa.p_nmds.c))

ggplot() +
  geom_point(data = Ahor_Urokosa.p_comb, aes(x = MDS1, y = MDS2, 
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
  stat_ellipse(data = Ahor_Urokosa.p_comb, 
               aes(x = MDS1, y = MDS2, 
                   group = Season_Infestation, 
                   color = Infestation_Gradient), 
               geom = "path", 
               level = 0.90, 
               linewidth = 0.7,   
               show.legend = NA)+
  stat_ellipse(data = Ahor_Urokosa.p_comb,  geom = "polygon", 
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




Ahor_Urokosa.b <- full_Data %>% 
  filter(Village == "Ahor-Urokosa",
         Collection_method == "Beating tray") %>%
  dplyr::select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) > 0)) %>%
  mutate(Season_Infestation = paste(Seasons, Infestation_Gradient, sep = "_")) %>% 
  # filter(if_any(where(is.numeric), ~ .x > 0)) %>% 
  as.data.frame()

Ahor_Urokosa.b_nmds <- Ahor_Urokosa.b %>% 
  ungroup() %>% 
  dplyr::select(where(is.numeric))

Ahor_Urokosa.b_nmds.c <- Ahor_Urokosa.b %>% 
  ungroup() %>% 
  dplyr::select(where(Negate(is.numeric)))


Ahor_Urokosa.b_nmds_hell <- decostand(Ahor_Urokosa.b_nmds, method = "hellinger")
Ahor_Urokosa.b_bray_nmds <- metaMDS(Ahor_Urokosa.b_nmds_hell,
                                    distance = "bray", k=2, na.rm = TRUE)

Ahor_Urokosa.b_bray_nmds$stress
stressplot(Ahor_Urokosa.b_bray_nmds)

scores(Ahor_Urokosa.b_bray_nmds) 


Ahor_Urokosa.b_distance<- vegdist(Ahor_Urokosa.b_nmds_hell, method = "bray")
anova(betadisper(Ahor_Urokosa.b_distance, 
                 Ahor_Urokosa.b_nmds.c$Season_Infestation )) # PERMDISP

adonis2 (Ahor_Urokosa.b_nmds~Season_Infestation, 
         data = Ahor_Urokosa.b_nmds.c, permutations = 9999, 
         method = "bray")

Ahor_Urokosa.b.pairwise <- pairwise.adonis(Ahor_Urokosa.b_distance,
                                           Ahor_Urokosa.b_nmds.c$Season_Infestation)
Ahor_Urokosa.b.pairwise


Ahor_Urokosa.b_bray_nmds_points <- as.data.frame(Ahor_Urokosa.b_bray_nmds$points)
Ahor_Urokosa.b_bray_nmds_species <- as.data.frame(scores(Ahor_Urokosa.b_bray_nmds)$species)  
Ahor_Urokosa.b_comb <- as.data.frame(cbind(Ahor_Urokosa.b_bray_nmds_points, 
                                           Ahor_Urokosa.b_nmds.c))

ggplot() +
  geom_point(data = Ahor_Urokosa.b_comb, aes(x = MDS1, y = MDS2, 
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
  stat_ellipse(data = Ahor_Urokosa.b_comb, 
               aes(x = MDS1, y = MDS2, 
                   group = Season_Infestation, 
                   color = Infestation_Gradient), 
               geom = "path", 
               level = 0.90, 
               linewidth = 0.7,   
               show.legend = NA)+
  stat_ellipse(data = Ahor_Urokosa.b_comb,  geom = "polygon", 
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



