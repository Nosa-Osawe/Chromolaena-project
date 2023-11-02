library(tidyverse)
Ogua_PF_0 <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\C_Benin_Ogua_PF0%.csv")
Ogua_PF_1 <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\C_Benin_Ogua_PF_less_than_50%.csv")
Ogua_PF_2 <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\C_Benin_Ogua_PF_greater_than_50%.csv")
Igue_PF_0 <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\C_Benin_Igueosagie_PF0%.csv")
Igue_PF_1 <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\C_Benin_Igueosagie_less_than_PF50%.csv")
Igue_PF_2 <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\C_Benin_Igueosagie_PF_greater_than_50%.csv")


view(Ogua_PF_0)   # check missing values
sum(is.na(Ogua_PF_0))
sum(is.na(Ogua_PF_1))
sum(is.na(Ogua_PF_2))

summary(Ogua_PF_0)

Ogua_PF_0$SN <- seq_along(Ogua_PF_0$ID) ## assign serial number to data frame
max_Ogua_PF_0_SN <-max(Ogua_PF_0$SN)

# Specify the starting serial number
starting_number1 <- max_Ogua_PF_0_SN + 1

# Create a new column with serial numbers
Ogua_PF_1$SN <- seq(starting_number1, starting_number1 + nrow(Ogua_PF_1) - 1)
view(Ogua_PF_1)   # ok
max_Ogua_PF_1_SN <- max(Ogua_PF_1$SN)
# Create a new column with serial numbers
starting_number2 <- max_Ogua_PF_1_SN + 1

Ogua_PF_2$SN <- seq(starting_number2, starting_number2 + nrow(Ogua_PF_2) - 1)
view(Ogua_PF_2) 

max_Ogua_PF_2_SN <- max(Ogua_PF_2$SN)
# Create a new column with serial numbers
starting_number3 <- max_Ogua_PF_2_SN + 1

Igue_PF_0$SN <- seq(starting_number3, starting_number3 + nrow(Igue_PF_0) - 1)

max_Igue_PF_0_SN <- max(Igue_PF_0$SN)
# Create a new column with serial numbers
starting_number4 <- max_Igue_PF_0_SN + 1

Igue_PF_1$SN <- seq(starting_number4, starting_number4 + nrow(Igue_PF_1) - 1)


max_Igue_PF_1_SN <- max(Igue_PF_1$SN)
# Create a new column with serial numbers
starting_number5 <- max_Igue_PF_1_SN + 1

Igue_PF_2$SN <- seq(starting_number5, starting_number5 + nrow(Igue_PF_2) - 1)

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

Benin <- Benin %>%
  select(-SN)


