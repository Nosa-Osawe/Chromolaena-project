 
library(tidyverse)
library(dunn.test)
library(FSA)

PT_diversity <- read.csv("C:\\Users\\HP\\Desktop\\Pt_diversity.csv", stringsAsFactors = TRUE)
View(PT_diversity)
attach(PT_diversity)

pt_div_ogua <- PT_diversity %>% 
  filter(Location== "Ogua")

pt_div_igue <- PT_diversity %>% 
  filter(Location== "Igue")


##################### ====   Ogua

kruskal.test(pt_div_ogua$Shannon_H ~ pt_div_ogua$Site)  
dunn.test(pt_div_ogua$Shannon_H, pt_div_ogua$Site, method = "bonferroni")
Shannon <- aggregate(pt_div_ogua$Shannon_H,
                           by = list(pt_div_ogua$Site),
                           FUN = function(x) c(median = median(x), 
                                               mean = mean(x), sd = sd(x)))
Shannon

kruskal.test(pt_div_ogua$Individuals ~ pt_div_ogua$Site)  
dunn.test(pt_div_ogua$Individuals, pt_div_ogua$Site, method = "bonferroni")
abundance <- aggregate(pt_div_ogua$Individuals,
                     by = list(pt_div_ogua$Site),
                     FUN = function(x) c(median = median(x), 
                                         mean = mean(x), sd = sd(x)))
abundance


kruskal.test(pt_div_ogua$Dominance_D ~ pt_div_ogua$Site)  
dunn.test(pt_div_ogua$Dominance_D, pt_div_ogua$Site, method = "bonferroni")
Dominance <- aggregate(pt_div_ogua$Dominance_D,
                     by = list(pt_div_ogua$Site),
                     FUN = function(x) c(median = median(x), 
                                         mean = mean(x), sd = sd(x)))
Dominance


kruskal.test(pt_div_ogua$Dominance_D ~ pt_div_ogua$Site)  
dunn.test(pt_div_ogua$Dominance_D, pt_div_ogua$Site, method = "bonferroni")
Dominance <- aggregate(pt_div_ogua$Dominance_D,
                       by = list(pt_div_ogua$Site),
                       FUN = function(x) c(median = median(x), 
                                           mean = mean(x), sd = sd(x)))
Dominance


taxa <- aov(pt_div_ogua$Taxa_S ~ pt_div_ogua$Site, data = pt_div_ogua)
summary(taxa)
TukeyHSD(taxa)
taxa.summary <- aggregate(pt_div_ogua$Taxa_S,
                                       by = list(pt_div_ogua$Site),
                                       FUN = function(x) c(median = median(x), 
                                                           mean = mean(x), sd = sd(x)))
taxa.summary

Evenness <- aov(pt_div_ogua$Evenness_e.H.S ~ pt_div_ogua$Site, data = pt_div_ogua)
summary(Evenness)
TukeyHSD(Evenness)
evenness.summary <- aggregate(pt_div_ogua$Evenness_e.H.S,
                          by = list(pt_div_ogua$Site),
                          FUN = function(x) c(median = median(x), 
                                              mean = mean(x), sd = sd(x)))
evenness.summary



Margalefindex <- aov(pt_div_ogua$Margalef ~ pt_div_ogua$Site, data = pt_div_ogua)
summary(Margalefindex)
TukeyHSD(Margalefindex)
margalef.summary <- aggregate(pt_div_ogua$Margalef,
                              by = list(pt_div_ogua$Site),
                              FUN = function(x) c(median = median(x), 
                                                  mean = mean(x), sd = sd(x)))
margalef.summary


##################### ====   igue
pt_div_igue

kruskal.test(pt_div_igue$Shannon_H ~ pt_div_igue$Site)  
dunn.test(pt_div_igue$Shannon_H, pt_div_igue$Site, method = "bonferroni")
Shannon <- aggregate(pt_div_igue$Shannon_H,
                     by = list(pt_div_igue$Site),
                     FUN = function(x) c(median = median(x), 
                                         mean = mean(x), sd = sd(x)))
Shannon

kruskal.test(pt_div_igue$Individuals ~ pt_div_igue$Site)  
dunn.test(pt_div_igue$Individuals, pt_div_igue$Site, method = "bonferroni")
abundance <- aggregate(pt_div_igue$Individuals,
                       by = list(pt_div_igue$Site),
                       FUN = function(x) c(median = median(x), 
                                           mean = mean(x), sd = sd(x)))
abundance


kruskal.test(pt_div_igue$Dominance_D ~ pt_div_igue$Site)  
dunn.test(pt_div_igue$Dominance_D, pt_div_igue$Site, method = "bonferroni")
Dominance <- aggregate(pt_div_igue$Dominance_D,
                       by = list(pt_div_igue$Site),
                       FUN = function(x) c(median = median(x), 
                                           mean = mean(x), sd = sd(x)))
Dominance


taxa <- aov(pt_div_igue$Taxa_S ~ pt_div_igue$Site, data = pt_div_igue)
summary(taxa)
TukeyHSD(taxa)
taxa.summary <- aggregate(pt_div_igue$Taxa_S,
                          by = list(pt_div_igue$Site),
                          FUN = function(x) c(median = median(x), 
                                              mean = mean(x), sd = sd(x)))
taxa.summary

Evenness <- aov(pt_div_igue$Evenness_e.H.S ~ pt_div_igue$Site, data = pt_div_igue)
summary(Evenness)
TukeyHSD(Evenness)
evenness.summary <- aggregate(pt_div_igue$Evenness_e.H.S,
                              by = list(pt_div_igue$Site),
                              FUN = function(x) c(median = median(x), 
                                                  mean = mean(x), sd = sd(x)))
evenness.summary



Margalefindex <- aov(pt_div_igue$Margalef ~ pt_div_igue$Site, data = pt_div_igue)
summary(Margalefindex)
TukeyHSD(Margalefindex)
margalef.summary <- aggregate(pt_div_igue$Margalef,
                              by = list(pt_div_igue$Site),
                              FUN = function(x) c(median = median(x), 
                                                  mean = mean(x), sd = sd(x)))
margalef.summary




