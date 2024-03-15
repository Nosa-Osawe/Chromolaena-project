library(tidyverse)

bt_all <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\Benin_BT.csv")
pf_all <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\Benin_PF.csv")

view(pf_all)
view(bt_all)

unique(bt_all$Location)

attach(pf_all)
attach(bt_all)

###########-----------------------------------------------------------------------------
Ogua_pf <- pf_all %>% 
  filter(Location == "Ogua") 

length(Ogua_pf$Location)

ogua_bt <- bt_all %>% 
  filter(Location=="Ogua")

head(ogua_bt, 5)

ogua_bt_by_fam_by_invation <-ogua_bt %>% 
  group_by(Family,Chromolaena_level) %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  select(-SN)

view(ogua_bt_by_fam_by_invation)

length(ogua_bt_by_fam_by_invation$Family)
length(ogua_pf_by_fam_by_invation$Family)

ogua_pf_by_fam_by_invation<- Ogua_pf %>% 
  group_by(Family, Chromolaena_level) %>% 
  summarise(across(where(is.numeric), sum)) %>%
  select(-SN)

view(ogua_pf_by_fam_by_invation)

write.csv(ogua_pf_by_fam_by_invation,
          "C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\ogua_pf_fam.csv")

write.csv(ogua_bt_by_fam_by_invation,
          "C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\ogua_bt_fam.csv")

ogua_c<- merge(ogua_bt_by_fam_by_invation, ogua_pf_by_fam_by_invation,
            by = c("Family","Chromolaena_level"))
ogua_c <- as.data.frame(ogua_c)
view(ogua_c)

write.csv(ogua_c, "C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\ogua_combined.csv")



###########-----------------------------------------------------------------------------

Igueosagie_pf <- pf_all %>% 
  filter(Location == "Igueosagie") 

length(Igueosagie_pf$Location)

Igueosagie_bt <- bt_all %>% 
  filter(Location=="Igueosagie")

length(Igueosagie_bt$Location)

Igueosagie_bt_by_fam_by_invation <-Igueosagie_bt %>% 
  group_by(Family,Chromolaena_level) %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  select(-SN)

view(Igueosagie_bt_by_fam_by_invation)

length(Igueosagie_bt_by_fam_by_invation$Family)

Igueosagie_pf_by_fam_by_invation<- Igueosagie_pf %>% 
  group_by(Family, Chromolaena_level) %>% 
  summarise(across(where(is.numeric), sum)) %>%
  select(-SN)
length(Igueosagie_pf_by_fam_by_invation$Family)


Igueosagie_c<- merge(Igueosagie_bt_by_fam_by_invation,
                     Igueosagie_pf_by_fam_by_invation,
               by = c("Family","Chromolaena_level"))
Igueosagie_c <- as.data.frame(Igueosagie_c)
view(Igueosagie_c)

write.csv(Igueosagie_c, 
          "C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\Igueosagie_combined.csv")






