# prolly have to run the N_EDA scripts first-- most of it

library(vegan)

Iguegosagie_wet.d <- full_Data %>% 
  filter(Village== "Iguegosagie", Seasons == "Wet") %>%
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>% 
  group_by(Infestation_Gradient) %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  select(-1)

# Built-in vegan diversity indices
shannon <- diversity(Iguegosagie_wet.d, index = "shannon")
simpson <- diversity(Iguegosagie_wet.d, index = "simpson")
richness <- specnumber(Iguegosagie_wet.d)
total_abundance <- rowSums(Iguegosagie_wet.d)
margalef <- (richness - 1) / log(total_abundance)

# Combine into data frame
(indices.Iguegosagie_wet.d <- data.frame(
  Infestation_gradient = c("High", "Mild", "Zero"),
  Shannon = shannon,
  Simpson = simpson,
  Margalef = margalef,
  Richness = richness,
  Abundance = total_abundance
) %>% 
    mutate(Season = "Wet",
           Village = "Iguegosagie"))


Ogua_wet.d <- full_Data %>% 
  filter(Village== "Ogua", Seasons == "Wet") %>%
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>% 
  group_by(Infestation_Gradient) %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  select(-1)

# Built-in vegan diversity indices
shannon <- diversity(Ogua_wet.d, index = "shannon")
simpson <- diversity(Ogua_wet.d, index = "simpson")
richness <- specnumber(Ogua_wet.d)
total_abundance <- rowSums(Ogua_wet.d)
margalef <- (richness - 1) / log(total_abundance)

# Combine into data frame
(indices.Ogua_wet.d <- data.frame(
  Infestation_gradient = c("High", "Mild", "Zero"),
  Shannon = shannon,
  Simpson = simpson,
  Margalef = margalef,
  Richness = richness,
  Abundance = total_abundance
) %>% 
    mutate(Season = "Wet",
           Village = "Ogua"))


Ahor_Urokosa_wet.d <- full_Data %>% 
  filter(Village== "Ahor-Urokosa", Seasons == "Wet") %>%
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>% 
  group_by(Infestation_Gradient) %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  select(-1)

# Built-in vegan diversity indices
shannon <- diversity(Ahor_Urokosa_wet.d, index = "shannon")
simpson <- diversity(Ahor_Urokosa_wet.d, index = "simpson")
richness <- specnumber(Ahor_Urokosa_wet.d)
total_abundance <- rowSums(Ahor_Urokosa_wet.d)
margalef <- (richness - 1) / log(total_abundance)

# Combine into data frame
(indices.Ahor_Urokosa_wet.d <- data.frame(
  Infestation_gradient = c("High", "Mild", "Zero"),
  Shannon = shannon,
  Simpson = simpson,
  Margalef = margalef,
  Richness = richness,
  Abundance = total_abundance
) %>% 
    mutate(Season = "Wet",
           Village = "Ahor-Urokosa"))


Iguovbiobo_wet.d <- full_Data %>% 
  filter(Village== "Iguovbiobo", Seasons == "Wet") %>%
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>% 
  group_by(Infestation_Gradient) %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  select(-1)

# Built-in vegan diversity indices
shannon <- diversity(Iguovbiobo_wet.d, index = "shannon")
simpson <- diversity(Iguovbiobo_wet.d, index = "simpson")
richness <- specnumber(Iguovbiobo_wet.d)
total_abundance <- rowSums(Iguovbiobo_wet.d)
margalef <- (richness - 1) / log(total_abundance)

# Combine into data frame
(indices.Iguovbiobo_wet.d <- data.frame(
  Infestation_gradient = c("High", "Mild", "Zero"),
  Shannon = shannon,
  Simpson = simpson,
  Margalef = margalef,
  Richness = richness,
  Abundance = total_abundance
) %>% 
    mutate(Season = "Wet",
           Village = "Iguovbiobo"))



Iguegosagie_Dry.d <- full_Data %>% 
  filter(Village== "Iguegosagie", Seasons == "Dry") %>%
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>% 
  group_by(Infestation_Gradient) %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  select(-1)

# Built-in vegan diversity indices
shannon <- diversity(Iguegosagie_Dry.d, index = "shannon")
simpson <- diversity(Iguegosagie_Dry.d, index = "simpson")
richness <- specnumber(Iguegosagie_Dry.d)
total_abundance <- rowSums(Iguegosagie_Dry.d)
margalef <- (richness - 1) / log(total_abundance)

# Combine into data frame
(indices.Iguegosagie_Dry.d <- data.frame(
  Infestation_gradient = c("High", "Mild", "Zero"),
  Shannon = shannon,
  Simpson = simpson,
  Margalef = margalef,
  Richness = richness,
  Abundance = total_abundance
) %>% 
    mutate(Season = "Dry",
           Village = "Iguegosagie"))


Ogua_Dry.d <- full_Data %>% 
  filter(Village== "Ogua", Seasons == "Dry") %>%
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>% 
  group_by(Infestation_Gradient) %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  select(-1)

# Built-in vegan diversity indices
shannon <- diversity(Ogua_Dry.d, index = "shannon")
simpson <- diversity(Ogua_Dry.d, index = "simpson")
richness <- specnumber(Ogua_Dry.d)
total_abundance <- rowSums(Ogua_Dry.d)
margalef <- (richness - 1) / log(total_abundance)

# Combine into data frame
(indices.Ogua_Dry.d <- data.frame(
  Infestation_gradient = c("High", "Mild", "Zero"),
  Shannon = shannon,
  Simpson = simpson,
  Margalef = margalef,
  Richness = richness,
  Abundance = total_abundance
) %>% 
    mutate(Season = "Dry",
           Village = "Ogua"))


Ahor_Urokosa_Dry.d <- full_Data %>% 
  filter(Village== "Ahor-Urokosa", Seasons == "Dry") %>%
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>% 
  group_by(Infestation_Gradient) %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  select(-1)

# Built-in vegan diversity indices
shannon <- diversity(Ahor_Urokosa_Dry.d, index = "shannon")
simpson <- diversity(Ahor_Urokosa_Dry.d, index = "simpson")
richness <- specnumber(Ahor_Urokosa_Dry.d)
total_abundance <- rowSums(Ahor_Urokosa_Dry.d)
margalef <- (richness - 1) / log(total_abundance)

# Combine into data frame
(indices.Ahor_Urokosa_Dry.d <- data.frame(
  Infestation_gradient = c("High", "Mild", "Zero"),
  Shannon = shannon,
  Simpson = simpson,
  Margalef = margalef,
  Richness = richness,
  Abundance = total_abundance
) %>% 
    mutate(Season = "Dry",
           Village = "Ahor-Urokosa"))


Iguovbiobo_Dry.d <- full_Data %>% 
  filter(Village== "Iguovbiobo", Seasons == "Dry") %>%
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>% 
  group_by(Infestation_Gradient) %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  select(-1)

# Built-in vegan diversity indices
shannon <- diversity(Iguovbiobo_Dry.d, index = "shannon")
simpson <- diversity(Iguovbiobo_Dry.d, index = "simpson")
richness <- specnumber(Iguovbiobo_Dry.d)
total_abundance <- rowSums(Iguovbiobo_Dry.d)
margalef <- (richness - 1) / log(total_abundance)

# Combine into data frame
(indices.Iguovbiobo_Dry.d <- data.frame(
  Infestation_gradient = c("High", "Mild", "Zero"),
  Shannon = shannon,
  Simpson = simpson,
  Margalef = margalef,
  Richness = richness,
  Abundance = total_abundance
) %>% 
    mutate(Season = "Dry",
           Village = "Iguovbiobo"))



indices.combined <- rbind(indices.Ahor_Urokosa_Dry.d,
                          indices.Ahor_Urokosa_wet.d,
                          indices.Iguegosagie_Dry.d,
                          indices.Iguegosagie_wet.d,
                          indices.Iguovbiobo_Dry.d,
                          indices.Iguovbiobo_wet.d,
                          indices.Ogua_Dry.d,
                          indices.Ogua_wet.d) %>% 
  as.data.frame()



################################################################################
##############


# RAREFRACTIONAND & SPECIES ACCUMULATION CURVE

infestation_colors <- c(
  "Zero" = "#4DAF4A",   # green
  "Mild" = "#FFD92F",   # yellow
  "High" = "#E41A1C"    # red
)

# Iguegosagie

# High

Iguegosagie_wet.PT.H.s <- full_Data %>%   
  filter(Village == "Iguegosagie", 
         Seasons == "Wet", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "High") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguegosagie_wet.PT.H.sac <- specaccum(Iguegosagie_wet.PT.H.s, method = "random")

Iguegosagie_wet.PT.H.sac.df <- data.frame(
  Sites = Iguegosagie_wet.PT.H.sac$sites,
  Richness = Iguegosagie_wet.PT.H.sac$richness,
  SD = Iguegosagie_wet.PT.H.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "High")



# Mild
Iguegosagie_wet.PT.M.s <- full_Data %>%   
  filter(Village == "Iguegosagie", 
         Seasons == "Wet", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "Mild") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguegosagie_wet.PT.M.sac <- specaccum(Iguegosagie_wet.PT.M.s, method = "random")

Iguegosagie_wet.PT.M.sac.df <- data.frame(
  Sites = Iguegosagie_wet.PT.M.sac$sites,
  Richness = Iguegosagie_wet.PT.M.sac$richness,
  SD = Iguegosagie_wet.PT.M.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Mild")


# Zero

Iguegosagie_wet.PT.Z.s <- full_Data %>%   
  filter(Village == "Iguegosagie", 
         Seasons == "Wet", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "Zero") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguegosagie_wet.PT.Z.sac <- specaccum(Iguegosagie_wet.PT.Z.s, method = "random")

Iguegosagie_wet.PT.Z.sac.df <- data.frame(
  Sites = Iguegosagie_wet.PT.Z.sac$sites,
  Richness = Iguegosagie_wet.PT.Z.sac$richness,
  SD = Iguegosagie_wet.PT.Z.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Zero")



Iguegosagie_wet.PT.combined <-rbind(Iguegosagie_wet.PT.H.sac.df,
                                    Iguegosagie_wet.PT.M.sac.df,
                                    Iguegosagie_wet.PT.Z.sac.df)


# Plot in ggplot2
Iguegosagie_wet.PT.combined.plot<- ggplot(Iguegosagie_wet.PT.combined, aes(x = Sites, y = Richness,
          fill = Infestation_Gradient )) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),  alpha = 0.4) +
  geom_line( aes(colour =Infestation_Gradient), linewidth = 1.2) +
  geom_point(size = 1,color = "#333333") +
  labs(
    title = "Species Accumulation Curve",
    x = "Number of Samples",
    y = "Family Richness"
  ) +
  scale_fill_manual(values = infestation_colors) +
  scale_colour_manual(values = infestation_colors) +
  theme_classic()


# DRY

Iguegosagie_Dry.PT.H.s <- full_Data %>%   
  filter(Village == "Iguegosagie", 
         Seasons == "Dry", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "High") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguegosagie_Dry.PT.H.sac <- specaccum(Iguegosagie_Dry.PT.H.s, method = "random")

Iguegosagie_Dry.PT.H.sac.df <- data.frame(
  Sites = Iguegosagie_Dry.PT.H.sac$sites,
  Richness = Iguegosagie_Dry.PT.H.sac$richness,
  SD = Iguegosagie_Dry.PT.H.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "High")



# Mild
Iguegosagie_Dry.PT.M.s <- full_Data %>%   
  filter(Village == "Iguegosagie", 
         Seasons == "Dry", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "Mild") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguegosagie_Dry.PT.M.sac <- specaccum(Iguegosagie_Dry.PT.M.s, method = "random")

Iguegosagie_Dry.PT.M.sac.df <- data.frame(
  Sites = Iguegosagie_Dry.PT.M.sac$sites,
  Richness = Iguegosagie_Dry.PT.M.sac$richness,
  SD = Iguegosagie_Dry.PT.M.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Mild")


# Zero

Iguegosagie_Dry.PT.Z.s <- full_Data %>%   
  filter(Village == "Iguegosagie", 
         Seasons == "Dry", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "Zero") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguegosagie_Dry.PT.Z.sac <- specaccum(Iguegosagie_Dry.PT.Z.s, method = "random")

Iguegosagie_Dry.PT.Z.sac.df <- data.frame(
  Sites = Iguegosagie_Dry.PT.Z.sac$sites,
  Richness = Iguegosagie_Dry.PT.Z.sac$richness,
  SD = Iguegosagie_Dry.PT.Z.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Zero")



Iguegosagie_Dry.PT.combined <-rbind(Iguegosagie_Dry.PT.H.sac.df,
                                    Iguegosagie_Dry.PT.M.sac.df,
                                    Iguegosagie_Dry.PT.Z.sac.df)


# Plot in ggplot2
Iguegosagie_Dry.PT.combined.plot<- ggplot(Iguegosagie_Dry.PT.combined, aes(x = Sites, y = Richness,
                                                                           fill = Infestation_Gradient )) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),  alpha = 0.4) +
  geom_line( aes(colour =Infestation_Gradient), linewidth = 1.2) +
  geom_point(size = 1,color = "#333333") +
  scale_fill_manual(values = infestation_colors) +
  scale_colour_manual(values = infestation_colors) +
  labs(
    title = "Species Accumulation Curve",
    x = "Number of Samples",
    y = "Family Richness"
  ) +
  theme_classic()



Iguegosagie_wet.BT.H.s <- full_Data %>%   
  filter(Village == "Iguegosagie", 
         Seasons == "Wet", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "High") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguegosagie_wet.BT.H.sac <- specaccum(Iguegosagie_wet.BT.H.s, method = "random")

Iguegosagie_wet.BT.H.sac.df <- data.frame(
  Sites = Iguegosagie_wet.BT.H.sac$sites,
  Richness = Iguegosagie_wet.BT.H.sac$richness,
  SD = Iguegosagie_wet.BT.H.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "High")



# Mild
Iguegosagie_wet.BT.M.s <- full_Data %>%   
  filter(Village == "Iguegosagie", 
         Seasons == "Wet", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "Mild") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguegosagie_wet.BT.M.sac <- specaccum(Iguegosagie_wet.BT.M.s, method = "random")

Iguegosagie_wet.BT.M.sac.df <- data.frame(
  Sites = Iguegosagie_wet.BT.M.sac$sites,
  Richness = Iguegosagie_wet.BT.M.sac$richness,
  SD = Iguegosagie_wet.BT.M.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Mild")


# Zero

Iguegosagie_wet.BT.Z.s <- full_Data %>%   
  filter(Village == "Iguegosagie", 
         Seasons == "Wet", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "Zero") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguegosagie_wet.BT.Z.sac <- specaccum(Iguegosagie_wet.BT.Z.s, method = "random")

Iguegosagie_wet.BT.Z.sac.df <- data.frame(
  Sites = Iguegosagie_wet.BT.Z.sac$sites,
  Richness = Iguegosagie_wet.BT.Z.sac$richness,
  SD = Iguegosagie_wet.BT.Z.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Zero")



Iguegosagie_wet.BT.combined <-rbind(Iguegosagie_wet.BT.H.sac.df,
                                    Iguegosagie_wet.BT.M.sac.df,
                                    Iguegosagie_wet.BT.Z.sac.df)


# Plot in ggplot2
Iguegosagie_wet.BT.combined.plot<- ggplot(Iguegosagie_wet.BT.combined, aes(x = Sites, y = Richness,
                                                                           fill = Infestation_Gradient )) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),  alpha = 0.4) +
  geom_line( aes(colour =Infestation_Gradient), linewidth = 1.2) +
  geom_point(size = 1,color = "#333333") +
  scale_fill_manual(values = infestation_colors) +
  scale_colour_manual(values = infestation_colors) +
  labs(
    title = "Species Accumulation Curve",
    x = "Number of Samples",
    y = "Family Richness"
  ) +
  theme_classic()


# DRY

Iguegosagie_Dry.BT.H.s <- full_Data %>%   
  filter(Village == "Iguegosagie", 
         Seasons == "Dry", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "High") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguegosagie_Dry.BT.H.sac <- specaccum(Iguegosagie_Dry.BT.H.s, method = "random")

Iguegosagie_Dry.BT.H.sac.df <- data.frame(
  Sites = Iguegosagie_Dry.BT.H.sac$sites,
  Richness = Iguegosagie_Dry.BT.H.sac$richness,
  SD = Iguegosagie_Dry.BT.H.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "High")



# Mild
Iguegosagie_Dry.BT.M.s <- full_Data %>%   
  filter(Village == "Iguegosagie", 
         Seasons == "Dry", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "Mild") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguegosagie_Dry.BT.M.sac <- specaccum(Iguegosagie_Dry.BT.M.s, method = "random")

Iguegosagie_Dry.BT.M.sac.df <- data.frame(
  Sites = Iguegosagie_Dry.BT.M.sac$sites,
  Richness = Iguegosagie_Dry.BT.M.sac$richness,
  SD = Iguegosagie_Dry.BT.M.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Mild")


# Zero

Iguegosagie_Dry.BT.Z.s <- full_Data %>%   
  filter(Village == "Iguegosagie", 
         Seasons == "Dry", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "Zero") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguegosagie_Dry.BT.Z.sac <- specaccum(Iguegosagie_Dry.BT.Z.s, method = "random")

Iguegosagie_Dry.BT.Z.sac.df <- data.frame(
  Sites = Iguegosagie_Dry.BT.Z.sac$sites,
  Richness = Iguegosagie_Dry.BT.Z.sac$richness,
  SD = Iguegosagie_Dry.BT.Z.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Zero")



Iguegosagie_Dry.BT.combined <-rbind(Iguegosagie_Dry.BT.H.sac.df,
                                    Iguegosagie_Dry.BT.M.sac.df,
                                    Iguegosagie_Dry.BT.Z.sac.df)


# Plot in ggplot2
Iguegosagie_Dry.BT.combined.plot<- ggplot(Iguegosagie_Dry.BT.combined, aes(x = Sites, y = Richness,
                                                                           fill = Infestation_Gradient )) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),  alpha = 0.4) +
  geom_line( aes(colour =Infestation_Gradient), linewidth = 1.2) +
  geom_point(size = 1,color = "#333333") +
  scale_fill_manual(values = infestation_colors) +
  scale_colour_manual(values = infestation_colors) +
  labs(
    title = "Species Accumulation Curve",
    x = "Number of Samples",
    y = "Family Richness"
  ) +
  theme_classic()




# Ogua

# High

Ogua_wet.PT.H.s <- full_Data %>%   
  filter(Village == "Ogua", 
         Seasons == "Wet", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "High") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ogua_wet.PT.H.sac <- specaccum(Ogua_wet.PT.H.s, method = "random")

Ogua_wet.PT.H.sac.df <- data.frame(
  Sites = Ogua_wet.PT.H.sac$sites,
  Richness = Ogua_wet.PT.H.sac$richness,
  SD = Ogua_wet.PT.H.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "High")



# Mild
Ogua_wet.PT.M.s <- full_Data %>%   
  filter(Village == "Ogua", 
         Seasons == "Wet", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "Mild") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ogua_wet.PT.M.sac <- specaccum(Ogua_wet.PT.M.s, method = "random")

Ogua_wet.PT.M.sac.df <- data.frame(
  Sites = Ogua_wet.PT.M.sac$sites,
  Richness = Ogua_wet.PT.M.sac$richness,
  SD = Ogua_wet.PT.M.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Mild")


# Zero

Ogua_wet.PT.Z.s <- full_Data %>%   
  filter(Village == "Ogua", 
         Seasons == "Wet", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "Zero") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ogua_wet.PT.Z.sac <- specaccum(Ogua_wet.PT.Z.s, method = "random")

Ogua_wet.PT.Z.sac.df <- data.frame(
  Sites = Ogua_wet.PT.Z.sac$sites,
  Richness = Ogua_wet.PT.Z.sac$richness,
  SD = Ogua_wet.PT.Z.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Zero")



Ogua_wet.PT.combined <-rbind(Ogua_wet.PT.H.sac.df,
                             Ogua_wet.PT.M.sac.df,
                             Ogua_wet.PT.Z.sac.df)


# Plot in ggplot2
Ogua_wet.PT.combined.plot<- ggplot(Ogua_wet.PT.combined, aes(x = Sites, y = Richness,
                                                             fill = Infestation_Gradient )) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),  alpha = 0.4) +
  geom_line( aes(colour =Infestation_Gradient), linewidth = 1.2) +
  geom_point(size = 1,color = "#333333") +
  labs(
    title = "Species Accumulation Curve",
    x = "Number of Samples",
    y = "Family Richness"
  ) +
  scale_fill_manual(values = infestation_colors) +
  scale_colour_manual(values = infestation_colors) +
  theme_classic()


# DRY

Ogua_Dry.PT.H.s <- full_Data %>%   
  filter(Village == "Ogua", 
         Seasons == "Dry", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "High") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ogua_Dry.PT.H.sac <- specaccum(Ogua_Dry.PT.H.s, method = "random")

Ogua_Dry.PT.H.sac.df <- data.frame(
  Sites = Ogua_Dry.PT.H.sac$sites,
  Richness = Ogua_Dry.PT.H.sac$richness,
  SD = Ogua_Dry.PT.H.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "High")



# Mild
Ogua_Dry.PT.M.s <- full_Data %>%   
  filter(Village == "Ogua", 
         Seasons == "Dry", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "Mild") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ogua_Dry.PT.M.sac <- specaccum(Ogua_Dry.PT.M.s, method = "random")

Ogua_Dry.PT.M.sac.df <- data.frame(
  Sites = Ogua_Dry.PT.M.sac$sites,
  Richness = Ogua_Dry.PT.M.sac$richness,
  SD = Ogua_Dry.PT.M.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Mild")


# Zero

Ogua_Dry.PT.Z.s <- full_Data %>%   
  filter(Village == "Ogua", 
         Seasons == "Dry", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "Zero") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ogua_Dry.PT.Z.sac <- specaccum(Ogua_Dry.PT.Z.s, method = "random")

Ogua_Dry.PT.Z.sac.df <- data.frame(
  Sites = Ogua_Dry.PT.Z.sac$sites,
  Richness = Ogua_Dry.PT.Z.sac$richness,
  SD = Ogua_Dry.PT.Z.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Zero")



Ogua_Dry.PT.combined <-rbind(Ogua_Dry.PT.H.sac.df,
                             Ogua_Dry.PT.M.sac.df,
                             Ogua_Dry.PT.Z.sac.df)


# Plot in ggplot2
Ogua_Dry.PT.combined.plot<- ggplot(Ogua_Dry.PT.combined, aes(x = Sites, y = Richness,
                                                             fill = Infestation_Gradient )) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),  alpha = 0.4) +
  geom_line( aes(colour =Infestation_Gradient), linewidth = 1.2) +
  geom_point(size = 1,color = "#333333") +
  scale_fill_manual(values = infestation_colors) +
  scale_colour_manual(values = infestation_colors) +
  labs(
    title = "Species Accumulation Curve",
    x = "Number of Samples",
    y = "Family Richness"
  ) +
  theme_classic()



Ogua_wet.BT.H.s <- full_Data %>%   
  filter(Village == "Ogua", 
         Seasons == "Wet", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "High") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ogua_wet.BT.H.sac <- specaccum(Ogua_wet.BT.H.s, method = "random")

Ogua_wet.BT.H.sac.df <- data.frame(
  Sites = Ogua_wet.BT.H.sac$sites,
  Richness = Ogua_wet.BT.H.sac$richness,
  SD = Ogua_wet.BT.H.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "High")



# Mild
Ogua_wet.BT.M.s <- full_Data %>%   
  filter(Village == "Ogua", 
         Seasons == "Wet", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "Mild") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ogua_wet.BT.M.sac <- specaccum(Ogua_wet.BT.M.s, method = "random")

Ogua_wet.BT.M.sac.df <- data.frame(
  Sites = Ogua_wet.BT.M.sac$sites,
  Richness = Ogua_wet.BT.M.sac$richness,
  SD = Ogua_wet.BT.M.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Mild")


# Zero

Ogua_wet.BT.Z.s <- full_Data %>%   
  filter(Village == "Ogua", 
         Seasons == "Wet", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "Zero") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ogua_wet.BT.Z.sac <- specaccum(Ogua_wet.BT.Z.s, method = "random")

Ogua_wet.BT.Z.sac.df <- data.frame(
  Sites = Ogua_wet.BT.Z.sac$sites,
  Richness = Ogua_wet.BT.Z.sac$richness,
  SD = Ogua_wet.BT.Z.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Zero")



Ogua_wet.BT.combined <-rbind(Ogua_wet.BT.H.sac.df,
                             Ogua_wet.BT.M.sac.df,
                             Ogua_wet.BT.Z.sac.df)


# Plot in ggplot2
Ogua_wet.BT.combined.plot<- ggplot(Ogua_wet.BT.combined, aes(x = Sites, y = Richness,
                                                             fill = Infestation_Gradient )) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),  alpha = 0.4) +
  geom_line( aes(colour =Infestation_Gradient), linewidth = 1.2) +
  geom_point(size = 1,color = "#333333") +
  scale_fill_manual(values = infestation_colors) +
  scale_colour_manual(values = infestation_colors) +
  labs(
    title = "Species Accumulation Curve",
    x = "Number of Samples",
    y = "Family Richness"
  ) +
  theme_classic()


# DRY

Ogua_Dry.BT.H.s <- full_Data %>%   
  filter(Village == "Ogua", 
         Seasons == "Dry", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "High") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ogua_Dry.BT.H.sac <- specaccum(Ogua_Dry.BT.H.s, method = "random")

Ogua_Dry.BT.H.sac.df <- data.frame(
  Sites = Ogua_Dry.BT.H.sac$sites,
  Richness = Ogua_Dry.BT.H.sac$richness,
  SD = Ogua_Dry.BT.H.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "High")



# Mild
Ogua_Dry.BT.M.s <- full_Data %>%   
  filter(Village == "Ogua", 
         Seasons == "Dry", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "Mild") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ogua_Dry.BT.M.sac <- specaccum(Ogua_Dry.BT.M.s, method = "random")

Ogua_Dry.BT.M.sac.df <- data.frame(
  Sites = Ogua_Dry.BT.M.sac$sites,
  Richness = Ogua_Dry.BT.M.sac$richness,
  SD = Ogua_Dry.BT.M.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Mild")


# Zero

Ogua_Dry.BT.Z.s <- full_Data %>%   
  filter(Village == "Ogua", 
         Seasons == "Dry", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "Zero") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ogua_Dry.BT.Z.sac <- specaccum(Ogua_Dry.BT.Z.s, method = "random")

Ogua_Dry.BT.Z.sac.df <- data.frame(
  Sites = Ogua_Dry.BT.Z.sac$sites,
  Richness = Ogua_Dry.BT.Z.sac$richness,
  SD = Ogua_Dry.BT.Z.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Zero")



Ogua_Dry.BT.combined <-rbind(Ogua_Dry.BT.H.sac.df,
                             Ogua_Dry.BT.M.sac.df,
                             Ogua_Dry.BT.Z.sac.df)


# Plot in ggplot2
Ogua_Dry.BT.combined.plot<- ggplot(Ogua_Dry.BT.combined, aes(x = Sites, y = Richness,
                                                             fill = Infestation_Gradient )) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),  alpha = 0.4) +
  geom_line( aes(colour =Infestation_Gradient), linewidth = 1.2) +
  geom_point(size = 1,color = "#333333") +
  scale_fill_manual(values = infestation_colors) +
  scale_colour_manual(values = infestation_colors) +
  labs(
    title = "Species Accumulation Curve",
    x = "Number of Samples",
    y = "Family Richness"
  ) +
  theme_classic()







# Iguovbiobo

# High

Iguovbiobo_wet.PT.H.s <- full_Data %>%   
  filter(Village == "Iguovbiobo", 
         Seasons == "Wet", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "High") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguovbiobo_wet.PT.H.sac <- specaccum(Iguovbiobo_wet.PT.H.s, method = "random")

Iguovbiobo_wet.PT.H.sac.df <- data.frame(
  Sites = Iguovbiobo_wet.PT.H.sac$sites,
  Richness = Iguovbiobo_wet.PT.H.sac$richness,
  SD = Iguovbiobo_wet.PT.H.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "High")



# Mild
Iguovbiobo_wet.PT.M.s <- full_Data %>%   
  filter(Village == "Iguovbiobo", 
         Seasons == "Wet", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "Mild") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguovbiobo_wet.PT.M.sac <- specaccum(Iguovbiobo_wet.PT.M.s, method = "random")

Iguovbiobo_wet.PT.M.sac.df <- data.frame(
  Sites = Iguovbiobo_wet.PT.M.sac$sites,
  Richness = Iguovbiobo_wet.PT.M.sac$richness,
  SD = Iguovbiobo_wet.PT.M.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Mild")


# Zero

Iguovbiobo_wet.PT.Z.s <- full_Data %>%   
  filter(Village == "Iguovbiobo", 
         Seasons == "Wet", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "Zero") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguovbiobo_wet.PT.Z.sac <- specaccum(Iguovbiobo_wet.PT.Z.s, method = "random")

Iguovbiobo_wet.PT.Z.sac.df <- data.frame(
  Sites = Iguovbiobo_wet.PT.Z.sac$sites,
  Richness = Iguovbiobo_wet.PT.Z.sac$richness,
  SD = Iguovbiobo_wet.PT.Z.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Zero")



Iguovbiobo_wet.PT.combined <-rbind(Iguovbiobo_wet.PT.H.sac.df,
                                   Iguovbiobo_wet.PT.M.sac.df,
                                   Iguovbiobo_wet.PT.Z.sac.df)


# Plot in ggplot2
Iguovbiobo_wet.PT.combined.plot<- ggplot(Iguovbiobo_wet.PT.combined, aes(x = Sites, y = Richness,
                                                                         fill = Infestation_Gradient )) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),  alpha = 0.4) +
  geom_line( aes(colour =Infestation_Gradient), linewidth = 1.2) +
  geom_point(size = 1,color = "#333333") +
  labs(
    title = "Species Accumulation Curve",
    x = "Number of Samples",
    y = "Family Richness"
  ) +
  scale_fill_manual(values = infestation_colors) +
  scale_colour_manual(values = infestation_colors) +
  theme_classic()


# DRY

Iguovbiobo_Dry.PT.H.s <- full_Data %>%   
  filter(Village == "Iguovbiobo", 
         Seasons == "Dry", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "High") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguovbiobo_Dry.PT.H.sac <- specaccum(Iguovbiobo_Dry.PT.H.s, method = "random")

Iguovbiobo_Dry.PT.H.sac.df <- data.frame(
  Sites = Iguovbiobo_Dry.PT.H.sac$sites,
  Richness = Iguovbiobo_Dry.PT.H.sac$richness,
  SD = Iguovbiobo_Dry.PT.H.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "High")



# Mild
Iguovbiobo_Dry.PT.M.s <- full_Data %>%   
  filter(Village == "Iguovbiobo", 
         Seasons == "Dry", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "Mild") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguovbiobo_Dry.PT.M.sac <- specaccum(Iguovbiobo_Dry.PT.M.s, method = "random")

Iguovbiobo_Dry.PT.M.sac.df <- data.frame(
  Sites = Iguovbiobo_Dry.PT.M.sac$sites,
  Richness = Iguovbiobo_Dry.PT.M.sac$richness,
  SD = Iguovbiobo_Dry.PT.M.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Mild")


# Zero

Iguovbiobo_Dry.PT.Z.s <- full_Data %>%   
  filter(Village == "Iguovbiobo", 
         Seasons == "Dry", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "Zero") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguovbiobo_Dry.PT.Z.sac <- specaccum(Iguovbiobo_Dry.PT.Z.s, method = "random")

Iguovbiobo_Dry.PT.Z.sac.df <- data.frame(
  Sites = Iguovbiobo_Dry.PT.Z.sac$sites,
  Richness = Iguovbiobo_Dry.PT.Z.sac$richness,
  SD = Iguovbiobo_Dry.PT.Z.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Zero")



Iguovbiobo_Dry.PT.combined <-rbind(Iguovbiobo_Dry.PT.H.sac.df,
                                   Iguovbiobo_Dry.PT.M.sac.df,
                                   Iguovbiobo_Dry.PT.Z.sac.df)


# Plot in ggplot2
Iguovbiobo_Dry.PT.combined.plot<- ggplot(Iguovbiobo_Dry.PT.combined, aes(x = Sites, y = Richness,
                                                                         fill = Infestation_Gradient )) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),  alpha = 0.4) +
  geom_line( aes(colour =Infestation_Gradient), linewidth = 1.2) +
  geom_point(size = 1,color = "#333333") +
  scale_fill_manual(values = infestation_colors) +
  scale_colour_manual(values = infestation_colors) +
  labs(
    title = "Species Accumulation Curve",
    x = "Number of Samples",
    y = "Family Richness"
  ) +
  theme_classic()



Iguovbiobo_wet.BT.H.s <- full_Data %>%   
  filter(Village == "Iguovbiobo", 
         Seasons == "Wet", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "High") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguovbiobo_wet.BT.H.sac <- specaccum(Iguovbiobo_wet.BT.H.s, method = "random")

Iguovbiobo_wet.BT.H.sac.df <- data.frame(
  Sites = Iguovbiobo_wet.BT.H.sac$sites,
  Richness = Iguovbiobo_wet.BT.H.sac$richness,
  SD = Iguovbiobo_wet.BT.H.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "High")



# Mild
Iguovbiobo_wet.BT.M.s <- full_Data %>%   
  filter(Village == "Iguovbiobo", 
         Seasons == "Wet", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "Mild") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguovbiobo_wet.BT.M.sac <- specaccum(Iguovbiobo_wet.BT.M.s, method = "random")

Iguovbiobo_wet.BT.M.sac.df <- data.frame(
  Sites = Iguovbiobo_wet.BT.M.sac$sites,
  Richness = Iguovbiobo_wet.BT.M.sac$richness,
  SD = Iguovbiobo_wet.BT.M.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Mild")


# Zero

Iguovbiobo_wet.BT.Z.s <- full_Data %>%   
  filter(Village == "Iguovbiobo", 
         Seasons == "Wet", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "Zero") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguovbiobo_wet.BT.Z.sac <- specaccum(Iguovbiobo_wet.BT.Z.s, method = "random")

Iguovbiobo_wet.BT.Z.sac.df <- data.frame(
  Sites = Iguovbiobo_wet.BT.Z.sac$sites,
  Richness = Iguovbiobo_wet.BT.Z.sac$richness,
  SD = Iguovbiobo_wet.BT.Z.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Zero")



Iguovbiobo_wet.BT.combined <-rbind(Iguovbiobo_wet.BT.H.sac.df,
                                   Iguovbiobo_wet.BT.M.sac.df,
                                   Iguovbiobo_wet.BT.Z.sac.df)


# Plot in ggplot2
Iguovbiobo_wet.BT.combined.plot<- ggplot(Iguovbiobo_wet.BT.combined, aes(x = Sites, y = Richness,
                                                                         fill = Infestation_Gradient )) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),  alpha = 0.4) +
  geom_line( aes(colour =Infestation_Gradient), linewidth = 1.2) +
  geom_point(size = 1,color = "#333333") +
  scale_fill_manual(values = infestation_colors) +
  scale_colour_manual(values = infestation_colors) +
  labs(
    title = "Species Accumulation Curve",
    x = "Number of Samples",
    y = "Family Richness"
  ) +
  theme_classic()


# DRY

Iguovbiobo_Dry.BT.H.s <- full_Data %>%   
  filter(Village == "Iguovbiobo", 
         Seasons == "Dry", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "High") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguovbiobo_Dry.BT.H.sac <- specaccum(Iguovbiobo_Dry.BT.H.s, method = "random")

Iguovbiobo_Dry.BT.H.sac.df <- data.frame(
  Sites = Iguovbiobo_Dry.BT.H.sac$sites,
  Richness = Iguovbiobo_Dry.BT.H.sac$richness,
  SD = Iguovbiobo_Dry.BT.H.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "High")



# Mild
Iguovbiobo_Dry.BT.M.s <- full_Data %>%   
  filter(Village == "Iguovbiobo", 
         Seasons == "Dry", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "Mild") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguovbiobo_Dry.BT.M.sac <- specaccum(Iguovbiobo_Dry.BT.M.s, method = "random")

Iguovbiobo_Dry.BT.M.sac.df <- data.frame(
  Sites = Iguovbiobo_Dry.BT.M.sac$sites,
  Richness = Iguovbiobo_Dry.BT.M.sac$richness,
  SD = Iguovbiobo_Dry.BT.M.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Mild")


# Zero

Iguovbiobo_Dry.BT.Z.s <- full_Data %>%   
  filter(Village == "Iguovbiobo", 
         Seasons == "Dry", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "Zero") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Iguovbiobo_Dry.BT.Z.sac <- specaccum(Iguovbiobo_Dry.BT.Z.s, method = "random")

Iguovbiobo_Dry.BT.Z.sac.df <- data.frame(
  Sites = Iguovbiobo_Dry.BT.Z.sac$sites,
  Richness = Iguovbiobo_Dry.BT.Z.sac$richness,
  SD = Iguovbiobo_Dry.BT.Z.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Zero")



Iguovbiobo_Dry.BT.combined <-rbind(Iguovbiobo_Dry.BT.H.sac.df,
                                   Iguovbiobo_Dry.BT.M.sac.df,
                                   Iguovbiobo_Dry.BT.Z.sac.df)


# Plot in ggplot2
Iguovbiobo_Dry.BT.combined.plot<- ggplot(Iguovbiobo_Dry.BT.combined, aes(x = Sites, y = Richness,
                                                                         fill = Infestation_Gradient )) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),  alpha = 0.4) +
  geom_line( aes(colour =Infestation_Gradient), linewidth = 1.2) +
  geom_point(size = 1,color = "#333333") +
  scale_fill_manual(values = infestation_colors) +
  scale_colour_manual(values = infestation_colors) +
  labs(
    title = "Species Accumulation Curve",
    x = "Number of Samples",
    y = "Family Richness"
  ) +
  theme_classic()





# Ahor_Urokosa

# High

Ahor_Urokosa_wet.PT.H.s <- full_Data %>%   
  filter(Village == "Ahor-Urokosa", 
         Seasons == "Wet", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "High") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ahor_Urokosa_wet.PT.H.sac <- specaccum(Ahor_Urokosa_wet.PT.H.s, method = "random")

Ahor_Urokosa_wet.PT.H.sac.df <- data.frame(
  Sites = Ahor_Urokosa_wet.PT.H.sac$sites,
  Richness = Ahor_Urokosa_wet.PT.H.sac$richness,
  SD = Ahor_Urokosa_wet.PT.H.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "High")



# Mild
Ahor_Urokosa_wet.PT.M.s <- full_Data %>%   
  filter(Village == "Ahor-Urokosa", 
         Seasons == "Wet", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "Mild") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ahor_Urokosa_wet.PT.M.sac <- specaccum(Ahor_Urokosa_wet.PT.M.s, method = "random")

Ahor_Urokosa_wet.PT.M.sac.df <- data.frame(
  Sites = Ahor_Urokosa_wet.PT.M.sac$sites,
  Richness = Ahor_Urokosa_wet.PT.M.sac$richness,
  SD = Ahor_Urokosa_wet.PT.M.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Mild")


# Zero

Ahor_Urokosa_wet.PT.Z.s <- full_Data %>%   
  filter(Village == "Ahor-Urokosa", 
         Seasons == "Wet", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "Zero") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ahor_Urokosa_wet.PT.Z.sac <- specaccum(Ahor_Urokosa_wet.PT.Z.s, method = "random")

Ahor_Urokosa_wet.PT.Z.sac.df <- data.frame(
  Sites = Ahor_Urokosa_wet.PT.Z.sac$sites,
  Richness = Ahor_Urokosa_wet.PT.Z.sac$richness,
  SD = Ahor_Urokosa_wet.PT.Z.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Zero")



Ahor_Urokosa_wet.PT.combined <-rbind(Ahor_Urokosa_wet.PT.H.sac.df,
                                     Ahor_Urokosa_wet.PT.M.sac.df,
                                     Ahor_Urokosa_wet.PT.Z.sac.df)


# Plot in ggplot2
Ahor_Urokosa_wet.PT.combined.plot<- ggplot(Ahor_Urokosa_wet.PT.combined, aes(x = Sites, y = Richness,
                                                                             fill = Infestation_Gradient )) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),  alpha = 0.4) +
  geom_line( aes(colour =Infestation_Gradient), linewidth = 1.2) +
  geom_point(size = 1,color = "#333333") +
  labs(
    title = "Species Accumulation Curve",
    x = "Number of Samples",
    y = "Family Richness"
  ) +
  scale_fill_manual(values = infestation_colors) +
  scale_colour_manual(values = infestation_colors) +
  theme_classic()


# DRY

Ahor_Urokosa_Dry.PT.H.s <- full_Data %>%   
  filter(Village == "Ahor-Urokosa", 
         Seasons == "Dry", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "High") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ahor_Urokosa_Dry.PT.H.sac <- specaccum(Ahor_Urokosa_Dry.PT.H.s, method = "random")

Ahor_Urokosa_Dry.PT.H.sac.df <- data.frame(
  Sites = Ahor_Urokosa_Dry.PT.H.sac$sites,
  Richness = Ahor_Urokosa_Dry.PT.H.sac$richness,
  SD = Ahor_Urokosa_Dry.PT.H.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "High")



# Mild
Ahor_Urokosa_Dry.PT.M.s <- full_Data %>%   
  filter(Village == "Ahor-Urokosa", 
         Seasons == "Dry", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "Mild") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ahor_Urokosa_Dry.PT.M.sac <- specaccum(Ahor_Urokosa_Dry.PT.M.s, method = "random")

Ahor_Urokosa_Dry.PT.M.sac.df <- data.frame(
  Sites = Ahor_Urokosa_Dry.PT.M.sac$sites,
  Richness = Ahor_Urokosa_Dry.PT.M.sac$richness,
  SD = Ahor_Urokosa_Dry.PT.M.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Mild")


# Zero

Ahor_Urokosa_Dry.PT.Z.s <- full_Data %>%   
  filter(Village == "Ahor-Urokosa", 
         Seasons == "Dry", 
         Collection_method == "Pitfall",
         Infestation_Gradient == "Zero") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ahor_Urokosa_Dry.PT.Z.sac <- specaccum(Ahor_Urokosa_Dry.PT.Z.s, method = "random")

Ahor_Urokosa_Dry.PT.Z.sac.df <- data.frame(
  Sites = Ahor_Urokosa_Dry.PT.Z.sac$sites,
  Richness = Ahor_Urokosa_Dry.PT.Z.sac$richness,
  SD = Ahor_Urokosa_Dry.PT.Z.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Zero")



Ahor_Urokosa_Dry.PT.combined <-rbind(Ahor_Urokosa_Dry.PT.H.sac.df,
                                     Ahor_Urokosa_Dry.PT.M.sac.df,
                                     Ahor_Urokosa_Dry.PT.Z.sac.df)


# Plot in ggplot2
Ahor_Urokosa_Dry.PT.combined.plot<- ggplot(Ahor_Urokosa_Dry.PT.combined, aes(x = Sites, y = Richness,
                                                                             fill = Infestation_Gradient )) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),  alpha = 0.4) +
  geom_line( aes(colour =Infestation_Gradient), linewidth = 1.2) +
  geom_point(size = 1,color = "#333333") +
  scale_fill_manual(values = infestation_colors) +
  scale_colour_manual(values = infestation_colors) +
  labs(
    title = "Species Accumulation Curve",
    x = "Number of Samples",
    y = "Family Richness"
  ) +
  theme_classic()



Ahor_Urokosa_wet.BT.H.s <- full_Data %>%   
  filter(Village == "Ahor-Urokosa", 
         Seasons == "Wet", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "High") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ahor_Urokosa_wet.BT.H.sac <- specaccum(Ahor_Urokosa_wet.BT.H.s, method = "random")

Ahor_Urokosa_wet.BT.H.sac.df <- data.frame(
  Sites = Ahor_Urokosa_wet.BT.H.sac$sites,
  Richness = Ahor_Urokosa_wet.BT.H.sac$richness,
  SD = Ahor_Urokosa_wet.BT.H.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "High")



# Mild
Ahor_Urokosa_wet.BT.M.s <- full_Data %>%   
  filter(Village == "Ahor-Urokosa", 
         Seasons == "Wet", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "Mild") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ahor_Urokosa_wet.BT.M.sac <- specaccum(Ahor_Urokosa_wet.BT.M.s, method = "random")

Ahor_Urokosa_wet.BT.M.sac.df <- data.frame(
  Sites = Ahor_Urokosa_wet.BT.M.sac$sites,
  Richness = Ahor_Urokosa_wet.BT.M.sac$richness,
  SD = Ahor_Urokosa_wet.BT.M.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Mild")


# Zero

Ahor_Urokosa_wet.BT.Z.s <- full_Data %>%   
  filter(Village == "Ahor-Urokosa", 
         Seasons == "Wet", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "Zero") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ahor_Urokosa_wet.BT.Z.sac <- specaccum(Ahor_Urokosa_wet.BT.Z.s, method = "random")

Ahor_Urokosa_wet.BT.Z.sac.df <- data.frame(
  Sites = Ahor_Urokosa_wet.BT.Z.sac$sites,
  Richness = Ahor_Urokosa_wet.BT.Z.sac$richness,
  SD = Ahor_Urokosa_wet.BT.Z.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Zero")



Ahor_Urokosa_wet.BT.combined <-rbind(Ahor_Urokosa_wet.BT.H.sac.df,
                                     Ahor_Urokosa_wet.BT.M.sac.df,
                                     Ahor_Urokosa_wet.BT.Z.sac.df)


# Plot in ggplot2
Ahor_Urokosa_wet.BT.combined.plot<- ggplot(Ahor_Urokosa_wet.BT.combined, aes(x = Sites, y = Richness,
                                                                             fill = Infestation_Gradient )) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),  alpha = 0.4) +
  geom_line( aes(colour =Infestation_Gradient), linewidth = 1.2) +
  geom_point(size = 1,color = "#333333") +
  scale_fill_manual(values = infestation_colors) +
  scale_colour_manual(values = infestation_colors) +
  labs(
    title = "Species Accumulation Curve",
    x = "Number of Samples",
    y = "Family Richness"
  ) +
  theme_classic()


# DRY

Ahor_Urokosa_Dry.BT.H.s <- full_Data %>%   
  filter(Village == "Ahor-Urokosa", 
         Seasons == "Dry", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "High") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ahor_Urokosa_Dry.BT.H.sac <- specaccum(Ahor_Urokosa_Dry.BT.H.s, method = "random")

Ahor_Urokosa_Dry.BT.H.sac.df <- data.frame(
  Sites = Ahor_Urokosa_Dry.BT.H.sac$sites,
  Richness = Ahor_Urokosa_Dry.BT.H.sac$richness,
  SD = Ahor_Urokosa_Dry.BT.H.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "High")



# Mild
Ahor_Urokosa_Dry.BT.M.s <- full_Data %>%   
  filter(Village == "Ahor-Urokosa", 
         Seasons == "Dry", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "Mild") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ahor_Urokosa_Dry.BT.M.sac <- specaccum(Ahor_Urokosa_Dry.BT.M.s, method = "random")

Ahor_Urokosa_Dry.BT.M.sac.df <- data.frame(
  Sites = Ahor_Urokosa_Dry.BT.M.sac$sites,
  Richness = Ahor_Urokosa_Dry.BT.M.sac$richness,
  SD = Ahor_Urokosa_Dry.BT.M.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Mild")


# Zero

Ahor_Urokosa_Dry.BT.Z.s <- full_Data %>%   
  filter(Village == "Ahor-Urokosa", 
         Seasons == "Dry", 
         Collection_method == "Beating tray",
         Infestation_Gradient == "Zero") %>%
  ungroup() %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  
  select(where(is.numeric)) %>%   
  mutate(across(everything(), ~ as.numeric(.x))) %>%   
  mutate(across(everything(), ~ replace_na(.x, 0)))   

Ahor_Urokosa_Dry.BT.Z.sac <- specaccum(Ahor_Urokosa_Dry.BT.Z.s, method = "random")

Ahor_Urokosa_Dry.BT.Z.sac.df <- data.frame(
  Sites = Ahor_Urokosa_Dry.BT.Z.sac$sites,
  Richness = Ahor_Urokosa_Dry.BT.Z.sac$richness,
  SD = Ahor_Urokosa_Dry.BT.Z.sac$sd
) %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )%>%
  mutate(Infestation_Gradient = "Zero")



Ahor_Urokosa_Dry.BT.combined <-rbind(Ahor_Urokosa_Dry.BT.H.sac.df,
                                     Ahor_Urokosa_Dry.BT.M.sac.df,
                                     Ahor_Urokosa_Dry.BT.Z.sac.df)


# Plot in ggplot2
Ahor_Urokosa_Dry.BT.combined.plot<- ggplot(Ahor_Urokosa_Dry.BT.combined, aes(x = Sites, y = Richness,
                                                                             fill = Infestation_Gradient )) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),  alpha = 0.4) +
  geom_line( aes(colour =Infestation_Gradient), linewidth = 1.2) +
  geom_point(size = 1,color = "#333333") +
  scale_fill_manual(values = infestation_colors) +
  scale_colour_manual(values = infestation_colors) +
  labs(
    title = "Species Accumulation Curve",
    x = "Number of Samples",
    y = "Family Richness"
  ) +
  theme_classic()








































































