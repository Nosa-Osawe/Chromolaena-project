
library(tidyverse)
library(dunn.test)
library(FSA)

BT_diversity <- read.csv("C:\\Users\\HP\\Desktop\\bt_div.csv", stringsAsFactors = TRUE)
View(BT_diversity)
attach(BT_diversity)

bt_div_igue <- BT_diversity %>% 
  filter(Location== "Igueosagie")


##################### ====   Igueosagie

kruskal.test(bt_div_igue$Shannon_H ~ bt_div_igue$Site)  
dunn.test(bt_div_igue$Shannon_H, bt_div_igue$Site, method = "bonferroni")
Shannon2 <- aggregate(bt_div_igue$Shannon_H,
                     by = list(bt_div_igue$Site),
                     FUN = function(x) c(median = median(x, na.rm = TRUE), 
                                         mean = mean(x, na.rm = TRUE),
                                         sd = sd(x, na.rm = TRUE)))
Shannon2

kruskal.test(bt_div_igue$Simpson_1.D ~ bt_div_igue$Site)  
dunn.test(bt_div_igue$Simpson_1.D, bt_div_igue$Site, method = "bonferroni")
Simpson_12 <- aggregate(bt_div_igue$Simpson_1.D,
                       by = list(bt_div_igue$Site),
                       FUN = function(x) c(median = median(x), 
                                           mean = mean(x), sd = sd(x)))
Simpson_12

kruskal.test(bt_div_igue$Individuals ~ bt_div_igue$Site)  
dunn.test(bt_div_igue$Individuals, bt_div_igue$Site, method = "bonferroni")
abundance2 <- aggregate(bt_div_igue$Individuals,
                       by = list(bt_div_igue$Site),
                       FUN = function(x) c(median = median(x, na.rm = TRUE), 
                                           mean = mean(x, na.rm = TRUE),
                                           sd = sd(x, na.rm = TRUE)))
abundance2


kruskal.test(bt_div_igue$Dominance_D ~ bt_div_igue$Site)  
dunn.test(bt_div_igue$Dominance_D, bt_div_igue$Site, method = "bonferroni")
Dominance2 <- aggregate(bt_div_igue$Dominance_D,
                       by = list(bt_div_igue$Site),
                       FUN = function(x) c(median = median(x, na.rm = TRUE), 
                                           mean = mean(x, na.rm = TRUE),
                                           sd = sd(x, na.rm = TRUE)))
Dominance2


kruskal.test(bt_div_igue$Dominance_D ~ bt_div_igue$Site)  
dunn.test(bt_div_igue$Dominance_D, bt_div_igue$Site, method = "bonferroni")
Dominance2 <- aggregate(bt_div_igue$Dominance_D,
                       by = list(bt_div_igue$Site),
                       FUN = function(x) c(median = median(x, na.rm = TRUE), 
                                           mean = mean(x, na.rm = TRUE),
                                           sd = sd(x, na.rm = TRUE)))
Dominance2

kruskal.test(bt_div_igue$Taxa_S ~ bt_div_igue$Site)  
dunn.test(bt_div_igue$Taxa_S, bt_div_igue$Site, method = "bonferroni")
taxa.summary2 <- aggregate(bt_div_igue$Taxa_S,
                          by = list(bt_div_igue$Site),
                          FUN = function(x) c(median = median(x, na.rm = TRUE), 
                                              mean = mean(x, na.rm = TRUE),
                                              sd = sd(x, na.rm = TRUE)))
taxa.summary2


kruskal.test(bt_div_igue$Evenness_e.H.S ~ bt_div_igue$Site)  
dunn.test(bt_div_igue$Evenness_e.H.S, bt_div_igue$Site, method = "bonferroni")
Evenness.summary2 <- aggregate(bt_div_igue$Evenness_e.H.S,
                              by = list(bt_div_igue$Site),
                              FUN = function(x) c(median = median(x, na.rm = TRUE), 
                                                  mean = mean(x, na.rm = TRUE),
                                                  sd = sd(x, na.rm = TRUE)))
Evenness.summary2



kruskal.test(bt_div_igue$Margalef ~ bt_div_igue$Site)  
dunn.test(bt_div_igue$Margalef, bt_div_igue$Site, method = "bonferroni")
Margalef.summary2 <- aggregate(bt_div_igue$Margalef,
                              by = list(bt_div_igue$Site),
                              FUN = function(x) c(median = median(x, na.rm = TRUE), 
                                                  mean = mean(x, na.rm = TRUE),
                                                  sd = sd(x, na.rm = TRUE)))
Margalef.summary2




