library(readxl)
library(tidyverse)
library(vegan)

mgt <- read_excel("C:\\Users\\DELL\\Desktop\\Jane PhD\\Chromolena_management.xlsx",
                  sheet = "Sheet1")
mgt.fam.tax<- read.csv("C:\\Users\\DELL\\Desktop\\Jane PhD\\Mgt_family_names.csv")

mgt.fam <- left_join(mgt, mgt.fam.tax,
                by = "Family") %>%
  select(-Family) %>% 
  rename("Family" = "Corrected_Family") %>% 
  select(Site, Period, Treatment, Class, Order,
         Family, MSp, P1, P2, P3)%>% 
  filter(!is.na(Class)) %>% 
  mutate(Order = ifelse(is.na(Order), Class, Order),
  Family = ifelse(is.na(Family), Order, Family),
  mutate(across(everything(), ~ replace_na(.x, 0)))) %>% 
  pivot_longer(cols = -c(1:7),
               values_to  = "Abundance",
               names_to = "Pitfall") %>% 
  group_by(Site, Treatment, Period, Pitfall, Family) %>% 
  summarise(Abundance = sum(Abundance)) %>% 
  pivot_wider(names_from = "Family",
              values_from = "Abundance")



mgt.summary<- mgt.fam %>% 
  select(where(~ !is.numeric(.x) || sum(.x, na.rm = TRUE) != 0)) %>%
  group_by(Treatment, Period, Site, Pitfall) %>% 
  summarise(across(where(is.numeric), sum))

mgt.summary.c<- St1.Before%>% 
  ungroup() %>% 
  select(1:4)

mgt.summary.d <- St1.Before%>% 
  ungroup() %>% 
  select(-c(1:4))


# Built-in vegan diversity indices
shannon <- diversity(mgt.summary.d, index = "shannon")
simpson <- diversity(mgt.summary.d, index = "simpson")
richness <- specnumber(mgt.summary.d)
total_abundance <- rowSums(mgt.summary.d)
margalef <- (richness - 1) / log(total_abundance)

# Combine into data frame
indices.mgt<- data.frame(
  Shannon = shannon,
  Simpson = simpson,
  Margalef = margalef,
  Richness = richness,
  Abundance = total_abundance) %>%
  cbind(mgt.summary.c)



indices.mgt <- indices.mgt %>% 
  mutate(Treatment = factor(Treatment,
                            levels = c("Positive control",
                                       "Negative control",
                                       "Slashing",
                                       "Herbicide",
                                       "Burning")),
         Period = factor(Period,
                         levels = c("Before",
                                    "1 DAT",
                                    "7 DAT",
                                    "14 DAT",
                                    "21 DAT")),
         period_n = case_when(
           Period == "Before" ~ 0,
           Period == "1 DAT"  ~ 1,
           Period == "7 DAT"  ~ 7,
           Period == "14 DAT" ~ 14,
           Period == "21 DAT" ~ 21
         ),
         Site = factor(Site,
                       levels = c ("Site 1",
                                   "Site 2",
                                   "Site 3",
                                   "Site 4")))



indices.mgt %>% 
ggplot() +
  geom_boxplot(aes(x = Treatment, y = Abundance, fill = Treatment),
               notch = FALSE,
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = Abundance, shape = Site, fill = Treatment),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.45) +
  labs(x= "Treatment", y= "Abundance")+
  theme(text = element_text(family = "Times New Roman", size = 24))+
  theme_bw()+
  coord_cartesian(ylim = c(0, 120)) +
  theme(legend.position = "none")+
  facet_wrap(~Period,nrow = 2, ncol = 3)





