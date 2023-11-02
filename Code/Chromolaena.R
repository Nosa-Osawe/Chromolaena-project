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



