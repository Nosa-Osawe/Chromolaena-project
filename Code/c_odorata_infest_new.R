library(tidyverse)
library(readxl)

c_path = "C:\\Users\\DELL\\Documents\\Git in R\\Chromolaena-project\\Data\\c_odorata_infest_new.xlsx"

Pt_wet <- read_excel(c_path, sheet = 'Table1_Pt_wet')
Pt_dry <- read_excel(c_path, sheet = 'Table2_Pt_Dry')
Bt_wet <- read_excel(c_path, sheet = 'Table3_Bt_wet')
Bt_dry <- read_excel(c_path, sheet = 'Table4_Bt_Dry')


# Lets work with Table 1:Arthropods collected by PT during the wet season  

view(Pt_wet)

unique(Pt_wet$Family) %>% 
  sort() %>% view()

Pt_wet_c4nmds_fam <- Pt_wet %>% 
  filter(!str_detect(str_to_lower(Family), "unknown")) %>% 
  pivot_longer(
    cols = c(Zero, Mild, High),
    names_to = "Levels",
    values_to = "Count"
  ) %>% 
  select(Family, Levels, Count) %>% 
  pivot_wider(
    names_from = "Family",
    values_from = "Count"
  )

View(Pt_wet_c4nmds_fam)

