library(tidyverse)
Ogua_PF_0 <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\C_Benin_Ogua_PF0%.csv")
Ogua_PF_1 <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\C_Benin_Ogua_PF_less_than_50%.csv")
Ogua_PF_2 <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\C_Benin_Ogua_PF_greater_than_50%.csv")
Igue_PF_0 <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\C_Benin_Igueosagie_PF0%.csv")
Igue_PF_1 <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\C_Benin_Igueosagie_less_than_PF50%.csv")
Igue_PF_2 <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\C_Benin_Igueosagie_PF_greater_than_50%.csv")

 # check missing values
sum(is.na(Ogua_PF_0))
sum(is.na(Ogua_PF_1))
sum(is.na(Ogua_PF_2))

Ogua_PF_0$SN <- seq_along(Ogua_PF_0$ID)
Ogua_PF_1$SN <- seq_along(Ogua_PF_1$ID)
Ogua_PF_2$SN <- seq_along(Ogua_PF_2$ID)
Igue_PF_0$SN <- seq_along(Igue_PF_0$ID)
Igue_PF_1$SN <- seq_along(Igue_PF_1$ID)
Igue_PF_2$SN <- seq_along(Igue_PF_2$ID)

# add location and Chromolaena level as columns

Ogua_PF_0 <- Ogua_PF_0 %>%
  mutate(Location = "Ogua",
         Chromolaena_level = "0%")

Ogua_PF_1 <- Ogua_PF_1 %>%
  mutate(Location = "Ogua",
         Chromolaena_level = "<50%")

Ogua_PF_2 <- Ogua_PF_2 %>%
  mutate(Location = "Ogua",
         Chromolaena_level = ">50%")
## try not to mutate ore than once, else you add more columns


Ogua_PF <- rbind(Ogua_PF_0, Ogua_PF_1, Ogua_PF_2)
view(Ogua_PF)

Igue_PF_0 <- Igue_PF_0 %>%  # rename column headers appropriately 
  rename(PT1 = Pt1,
         PT2 = Pt2,
         PT3 = Pt3,
         PT4 = Pt4,
         PT5 = Pt5,
         PT6 = Pt6,
         PT7 = Pt7,
         PT8 = Pt8,
         PT9 = Pt9,
         PT10 = Pt10,
         PT11 = Pt11,
         PT12 = Pt12,
         PT13 = Pt13,
         PT14 = Pt14,
         PT15 = Pt15)

Igue_PF_0 <- Ogua_PF_0 %>%
  mutate(Location = "Igueosagie",
         Chromolaena_level = "0%")

Igue_PF_1 <- Ogua_PF_1 %>%
  mutate(Location = "Igueosagie",
         Chromolaena_level = "<50%")

Igue_PF_2 <- Ogua_PF_2 %>%
  mutate(Location = "Igueosagie",
         Chromolaena_level = ">50%")

Igue_PF <- rbind(Igue_PF_0, Igue_PF_1, Igue_PF_2)
view(Ogua_PF)
View(Igue_PF)

Benin <- rbind(Ogua_PF, Igue_PF)
view(Benin)
# we spotted repetitions in the data which needs filtering

filtered_Benin <- Benin %>% 
  filter(!(SN >= 441 & SN <= 453))

Benin <- filtered_Benin

# remove the SN for now since its not correct for this combined data frame
Benin <- Benin %>%
  select(-SN)
#### lets give a new serial number (SN)

Benin$SN <- seq_along(Benin$ID)
Max_SN <- max(Benin$SN)
# Count the number of distinct ID
count_distinct_ID<- Benin %>%distinct(ID) %>% count()

count_ID<- Benin %>%
  group_by(ID) %>%
  summarize(count = n()) ## most should be 6

# Filter rows where count is not equal to 6
filtered_count_ID <- count_ID %>%
  filter(count != 6)

#  number of rows where count is not equal to 6
rows_not_equal_to_6 <- nrow(filtered_count_ID)

# Print the count of rows where count is not equal to 6
print(rows_not_equal_to_6)

Benin <- edit(Benin)
view(Benin)

# one more check
sum(is.na(Benin)) ### good

## This the corrected Benin pitfall collection data that will be used for this analysis
write.csv(Benin, 
          file = "C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\Benin_PF.csv", 
          row.names = FALSE)

