library(tidyverse)

bt_all <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\Benin_BT.csv")
pf_all <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\Benin_PF.csv")

view(pf_all)
view(bt_all)

unique(bt_all$Location)

attach(pf_all)
attach(bt_all)

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

ogua_pf_by_fam_by_invation<- Ogua_pf %>% 
  group_by(Family, Chromolaena_level) %>% 
  summarise(across(where(is.numeric), sum)) %>%
  select(-SN)

view(ogua_pf_by_fam_by_invation)

write.csv(ogua_pf_by_fam_by_invation,
          "C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\ogua_pf_fam.csv")

write.csv(ogua_bt_by_fam_by_invation,
          "C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\ogua_bt_fam.csv")



