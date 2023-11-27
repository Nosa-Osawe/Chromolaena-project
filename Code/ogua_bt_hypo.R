
library(tidyverse)
library(dunn.test)
library(FSA)

BT_diversity <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\bt_div.csv",
                         stringsAsFactors = TRUE)
View(BT_diversity)
attach(BT_diversity)

bt_div_ogua <- BT_diversity %>% 
  filter(Location== "Ogua")

bt_div_igue <- BT_diversity %>% 
  filter(Location== "Igueosagie")


##################### ====   Ogua

kruskal.test(bt_div_ogua$Shannon_H ~ bt_div_ogua$Site)  
dunn.test(bt_div_ogua$Shannon_H, bt_div_ogua$Site, method = "bonferroni")
Shannon <- aggregate(bt_div_ogua$Shannon_H,
                     by = list(bt_div_ogua$Site),
                     FUN = function(x) c(median = median(x, na.rm = TRUE), 
                                         mean = mean(x, na.rm = TRUE),
                                         sd = sd(x, na.rm = TRUE)))
Shannon

kruskal.test(bt_div_ogua$Simpson_1.D ~ bt_div_ogua$Site)  
dunn.test(bt_div_ogua$Simpson_1.D, bt_div_ogua$Site, method = "bonferroni")
Simpson_1 <- aggregate(bt_div_ogua$Simpson_1.D,
                       by = list(bt_div_ogua$Site),
                       FUN = function(x)  c(median = median(x, na.rm = TRUE), 
                                            mean = mean(x, na.rm = TRUE),
                                            sd = sd(x, na.rm = TRUE)))
Simpson_1

kruskal.test(bt_div_ogua$Individuals ~ bt_div_ogua$Site)  
dunn.test(bt_div_ogua$Individuals, bt_div_ogua$Site, method = "bonferroni")
abundance <- aggregate(bt_div_ogua$Individuals,
                       by = list(bt_div_ogua$Site),
                       FUN = function(x) c(median = median(x, na.rm = TRUE), 
                                           mean = mean(x, na.rm = TRUE),
                                           sd = sd(x, na.rm = TRUE)))
abundance


kruskal.test(bt_div_ogua$Dominance_D ~ bt_div_ogua$Site)  
dunn.test(bt_div_ogua$Dominance_D, bt_div_ogua$Site, method = "bonferroni")
Dominance <- aggregate(bt_div_ogua$Dominance_D,
                       by = list(bt_div_ogua$Site),
                       FUN = function(x) c(median = median(x, na.rm = TRUE), 
                                           mean = mean(x, na.rm = TRUE),
                                           sd = sd(x, na.rm = TRUE)))
Dominance


kruskal.test(bt_div_ogua$Dominance_D ~ bt_div_ogua$Site)  
dunn.test(bt_div_ogua$Dominance_D, bt_div_ogua$Site, method = "bonferroni")
Dominance <- aggregate(bt_div_ogua$Dominance_D,
                       by = list(bt_div_ogua$Site),
                       FUN = function(x) c(median = median(x, na.rm = TRUE), 
                                           mean = mean(x, na.rm = TRUE),
                                           sd = sd(x, na.rm = TRUE)))
Dominance

kruskal.test(bt_div_ogua$Taxa_S ~ bt_div_ogua$Site)  
dunn.test(bt_div_ogua$Taxa_S, bt_div_ogua$Site, method = "bonferroni")
taxa.summary <- aggregate(bt_div_ogua$Taxa_S,
                       by = list(bt_div_ogua$Site),
                       FUN = function(x) c(median = median(x, na.rm = TRUE), 
                                           mean = mean(x, na.rm = TRUE),
                                           sd = sd(x, na.rm = TRUE)))
taxa.summary


kruskal.test(bt_div_ogua$Evenness_e.H.S ~ bt_div_ogua$Site)  
dunn.test(bt_div_ogua$Evenness_e.H.S, bt_div_ogua$Site, method = "bonferroni")
Evenness.summary <- aggregate(bt_div_ogua$Evenness_e.H.S,
                          by = list(bt_div_ogua$Site),
                          FUN = function(x) c(median = median(x, na.rm = TRUE), 
                                              mean = mean(x, na.rm = TRUE),
                                              sd = sd(x, na.rm = TRUE)))
Evenness.summary



kruskal.test(bt_div_ogua$Margalef ~ bt_div_ogua$Site)  
dunn.test(bt_div_ogua$Margalef, bt_div_ogua$Site, method = "bonferroni")
Margalef.summary <- aggregate(bt_div_ogua$Margalef,
                              by = list(bt_div_ogua$Site),
                              FUN = function(x) c(median = median(x, na.rm = TRUE), 
                                                  mean = mean(x, na.rm = TRUE),
                                                  sd = sd(x, na.rm = TRUE)))
Margalef.summary




