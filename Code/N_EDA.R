library(tidyverse)
library(readxl)


pt.abundance <- read_excel("C:\\Users\\DELL\\Desktop\\Jane PhD\\Nosa_Pitfall.xlsx", 
                      sheet = "Abundance")
pt.tax <- read_excel("C:\\Users\\DELL\\Desktop\\Jane PhD\\Nosa_Pitfall.xlsx", 
                           sheet = "Taxonomy")
bt.abundance <- read_excel( "C:\\Users\\DELL\\Desktop\\Jane PhD\\Nosa_Beating_Tray.xlsx",
                           sheet = "Abundance")
bt.tax <- read_excel( "C:\\Users\\DELL\\Desktop\\Jane PhD\\Nosa_Beating_Tray.xlsx",
                            sheet = "Taxonomy")

# Data cleaning

pt.tax <- pt.tax %>% 
  mutate(Family = ifelse(is.na(Family), Order, Family),
         Species = ifelse(is.na(Species), Family, Species))

bt.tax <- bt.tax %>% 
  mutate(Family = ifelse(is.na(Family), Order, Family),
         Species = ifelse(is.na(Species), Family, Species))


semi_join(pt.tax, bt.tax,by = "ID")   # Looks good!
anti_join(pt.tax, bt.tax,by = "ID")   # Just Four IDs
anti_join(bt.tax, pt.tax, by = "ID")  # Just three IDs


bt.family <- as.data.frame(unique(bt.tax$Family)) %>%
  rename(Family = 1)

pt.family <- as.data.frame(unique(pt.tax$Family)) %>%
  rename(Family = 1)

full_families <- full_join(bt.family, pt.family, by = "Family")
write.csv(full_families, file = "C:\\Users\\DELL\\Desktop\\Jane PhD\\full_families.csv", 
          row.names = FALSE)


correct_fam <- read.csv("C:\\Users\\DELL\\Desktop\\Jane PhD\\corrected_family_names.csv") %>% 
  select(c(1,2))

correct_fam<- correct_fam %>% 
  rename("Family" = "Original")

# update the taxonomy in pt and bt
pt.tax_update <- left_join(pt.tax, correct_fam, by = "Family") %>% 
  select(-c("Family")) %>% 
  rename("Family" = "Corrected")

bt.tax_update <- left_join(bt.tax, correct_fam, by = "Family") %>% 
  select(-c("Family")) %>% 
  rename("Family" = "Corrected")





pt.abundance_L<- pt.abundance %>% # Long format
  rename(Sample = ID) %>%
  mutate(across(starts_with("Msp"), as.numeric)) %>%
  pivot_longer(cols = starts_with("Msp"),
               names_to = "ID",
               values_to = "Abundance") %>% 
  as.data.frame() %>% 
  mutate(Abundance = ifelse(is.na(Abundance), 0, Abundance)) # Deals with NAs

#  sum(is.na(pt.abundance_L))

bt.abundance_L<- bt.abundance %>% # Long format
  rename(Sample = ID) %>%
  mutate(across(starts_with("Msp"), as.numeric)) %>%
  pivot_longer(cols = starts_with("Msp"),
               names_to = "ID",
               values_to = "Abundance") %>% 
  as.data.frame() %>% 
  mutate(Abundance = ifelse(is.na(Abundance), 0, Abundance))

pt.abundance_LT <- left_join(pt.abundance_L, pt.tax_update, by  = "ID")
bt.abundance_LT <- left_join(bt.abundance_L, bt.tax_update, by = "ID")
 

# Work with Family-level data

pt.final <- pt.abundance_LT %>% 
  select(State,LGA,Village,Infestation_Gradient,Seasons,Sample,Family,Abundance) %>% 
  group_by(State,LGA,Village,Infestation_Gradient,Seasons,Sample,Family) %>% 
  summarise(Abundance = sum(Abundance)) %>% 
  pivot_wider(names_from = "Family",
              values_from = "Abundance")

pt.final <- pt.final %>%
  mutate(Sample = case_when(
    Sample %in% c("Pt1", "PT1", "Pf1") ~ "PT1",
    Sample %in% c("Pt2", "PT2", "Pf2") ~ "PT2",
    Sample %in% c("Pt3", "PT3", "Pf3") ~ "PT3",
    Sample %in% c("Pt4", "PT4", "Pf4") ~ "PT4",
    Sample %in% c("Pt5", "PT5", "Pf5") ~ "PT5",
    Sample %in% c("Pt6", "PT6", "Pf6") ~ "PT6",
    Sample %in% c("Pt7", "PT7", "Pf7") ~ "PT7",
    Sample %in% c("Pt8", "PT8", "Pf8") ~ "PT8",
    Sample %in% c("Pt9", "PT9", "Pf9") ~ "PT9",
    Sample %in% c("Pt10", "PT10", "Pf10") ~ "PT10",
    Sample %in% c("Pt11", "PT11", "Pf11") ~ "PT11",
    Sample %in% c("Pt12", "PT12", "Pf12") ~ "PT12",
    Sample %in% c("Pt13", "PT13", "Pf13") ~ "PT13",
    Sample %in% c("Pt14", "PT14", "Pf14") ~ "PT14",
    Sample %in% c("Pt15", "PT15", "Pf15") ~ "PT15",
    TRUE ~ Sample
  ))

# length(unique(pt.final$Sample))  Great - We expect 15 of them!

bt.final <- bt.abundance_LT %>% 
  select(State,`Local Government Area`, Village,Infestion_Gradient,Season,Sample,Family,Abundance) %>% 
  rename("LGA" = "Local Government Area",
         "Infestation_Gradient" = "Infestion_Gradient",
         "Seasons" = "Season") %>% 
  group_by(State,LGA,Village,Infestation_Gradient,Seasons,Sample,Family) %>% 
  summarise(Abundance = sum(Abundance)) %>% 
  pivot_wider(names_from = "Family",
              values_from = "Abundance")

# length(unique(bt.final$Sample))# Great
   

   
   pt.final <- pt.final %>%
     select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0))
   
 
 full_Data <-  rbind(
   pt.abundance_LT %>% 
          select(State,LGA,Village,Infestation_Gradient,Seasons,Sample,Family,Abundance) %>% 
          group_by(State,LGA,Village,Infestation_Gradient,Seasons,Sample,Family) %>% 
          summarise(Abundance = sum(Abundance)),
   
   bt.abundance_LT %>% 
          select(State,`Local Government Area`, Village,Infestion_Gradient,Season,Sample,Family,Abundance) %>% 
          rename("LGA" = "Local Government Area",
                 "Infestation_Gradient" = "Infestion_Gradient",
                 "Seasons" = "Season") %>% 
          group_by(State,LGA,Village,Infestation_Gradient,Seasons,Sample,Family) %>% 
          summarise(Abundance = sum(Abundance))
        )  %>%
   mutate(Sample = case_when(
     Sample %in% c("Pt1", "PT1", "Pf1") ~ "PT1",
     Sample %in% c("Pt2", "PT2", "Pf2") ~ "PT2",
     Sample %in% c("Pt3", "PT3", "Pf3") ~ "PT3",
     Sample %in% c("Pt4", "PT4", "Pf4") ~ "PT4",
     Sample %in% c("Pt5", "PT5", "Pf5") ~ "PT5",
     Sample %in% c("Pt6", "PT6", "Pf6") ~ "PT6",
     Sample %in% c("Pt7", "PT7", "Pf7") ~ "PT7",
     Sample %in% c("Pt8", "PT8", "Pf8") ~ "PT8",
     Sample %in% c("Pt9", "PT9", "Pf9") ~ "PT9",
     Sample %in% c("Pt10", "PT10", "Pf10") ~ "PT10",
     Sample %in% c("Pt11", "PT11", "Pf11") ~ "PT11",
     Sample %in% c("Pt12", "PT12", "Pf12") ~ "PT12",
     Sample %in% c("Pt13", "PT13", "Pf13") ~ "PT13",
     Sample %in% c("Pt14", "PT14", "Pf14") ~ "PT14",
     Sample %in% c("Pt15", "PT15", "Pf15") ~ "PT15",
     TRUE ~ Sample
   )) %>% 
   pivot_wider(names_from = "Family",
                 values_from = "Abundance")

 
 full_Data <- full_Data %>%
   mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>% 
   mutate(LGA = ifelse(LGA == "Ovia North East", "Ovia NE", LGA))%>%
   select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%  # drop column its sum = 0
   mutate(Collection_method = ifelse(str_starts(Sample, "P"),
                                     "Pitfall",
                                     "Beating tray")) %>% 
   mutate(Village = ifelse((Village=="Iguoviobo"), "Iguovbiobo", Village))
 
 
 #-------###################################################################################
 
 
 write.csv(full_Data, 
           file = "C:\\Users\\DELL\\Desktop\\Jane PhD\\full_Data.csv", 
           row.names = FALSE)
 

 
 
 
 
 
 
 
### --- Lets prepare the dataset

 # Village data and Seasons
 
 Iguegosagie_wet <- full_Data %>% 
   filter(Village== "Iguegosagie", Seasons == "Wet") %>%
   select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0), # this should be a default in the codes. 
          Infestation_Gradient, Sample, Collection_method) 
   
  

 Ovie_NE_dry <- full_Data %>% 
  filter(LGA== "Ovia NE",
         Seasons == "Dry") %>% 
   select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0), # this should be a default in the codes. 
          Infestation_Gradient, Sample, Collection_method) 
 
 Uhunmwonde_PT <- full_Data %>% 
   filter(LGA == "Uhunmwonde",
          Collection_method == "Pitfall") %>% 
   select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0), # this should be a default in the codes. 
          Infestation_Gradient, Sample, Seasons) 
 # this should be a default in the codes. 
 

 
 Iguegosagie <- full_Data %>% 
   filter(Village== "Iguegosagie") %>%
   select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0))
   
 
 Iguegosagie_nmds <- Iguegosagie %>% 
   ungroup() %>% 
   select(where(is.numeric))
 
 
 library(vegan)
 
 Iguegosagie_nmds_hell <- decostand(Iguegosagie_nmds, method = "hellinger")
 Iguegosagie_bray_nmds <- metaMDS(Iguegosagie_nmds_hell,
                                  distance = "bray", k=2, na.rm = TRUE)
 
 Iguegosagie_bray_nmds$stress
 stressplot(Iguegosagie_bray_nmds)
 
 scores(Iguegosagie_bray_nmds) 
 
 
 fly_distance<- vegdist(fly_nmds_hell, method = "bray")
 anova(betadisper(fly_distance, fly_cat$Collector)) # PERMDISP
 
 adonis2 (fly_nmds~Collector, 
          data = fly_cat, permutations = 9999, # fly_cat from line 21
          method = "bray")
 
 pairwise1 <- pairwise.adonis(fly_distance,fly_cat$Collector)
 pairwise1
 
 
 
 Uhunmwonde_PT.sp <- Uhunmwonde_PT %>% 
   ungroup() %>% 
   select(where(is.numeric))
 
 biodiversity_indices <- data.frame(
   Shannon = diversity(species_data, index = "shannon"),
   Simpson = diversity(species_data, index = "simpson"),
   Dominance = 1 - diversity(species_data, index = "simpson")
 )
 
 
 
 
 
 library(vegan)
 
 data(dune)
 species_data <- dune
 
 # Built-in vegan diversity indices
 shannon <- diversity(species_data, index = "shannon")
 simpson <- diversity(species_data, index = "simpson")
 richness <- specnumber(species_data)
 total_abundance <- rowSums(species_data)
 margalef <- (richness - 1) / log(total_abundance)

 
 # Combine into data frame
 biodiversity_indices <- data.frame(
   Shannon = shannon,
   Simpson = simpson,
   Margalef = margalef,
   Richness = richness,
   Abundance = total_abundance
 )
 
 biodiversity_indices
 

 

 sac <- specaccum(species_data, method = "random")
 
 sac_df <- data.frame(
   Sites = sac$sites,
   Richness = sac$richness,
   SD = sac$sd
 ) %>%
   mutate(
     Lower = Richness - SD,
     Upper = Richness + SD
   )
 
 # Plot in ggplot2
 ggplot(sac_df, aes(x = Sites, y = Richness)) +
   geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "lightblue", alpha = 0.4) +
   geom_line(color = "blue", size = 1.2) +
   geom_point(size = 2, color = "blue") +
   labs(
     title = "Species Accumulation Curve",
     x = "Number of Sites",
     y = "Species Richness"
   ) +
   theme_minimal(base_size = 14)
 