library(tidyverse)
library(dunn.test)
library(FSA)

bt_order <- read.csv( "C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\bt_order.csv",
                      stringsAsFactors = TRUE)
View(bt_order)
attach(bt_order)

bt_order_ogua <- bt_order %>% 
  filter(Location== "Ogua")

kruskal.test(bt_order_ogua$Araneae ~ bt_order_ogua$Site)  
dunn.test(bt_order_ogua$Araneae, bt_order_ogua$Site, method = "bonferroni")
Araneae3 <- aggregate(bt_order_ogua$Araneae,
                      by = list(bt_order_ogua$Site),
                      FUN = function(x) c(median = median(x), 
                                          sd = sd(x),
                                          mean = mean(x), se= se(x)))
Araneae3


kruskal.test(bt_order_ogua$Blattodea ~ bt_order_ogua$Site)  
dunn.test(bt_order_ogua$Blattodea, bt_order_ogua$Site, method = "bonferroni")
Blattodae3 <- aggregate(bt_order_ogua$Blattodea,
                        by = list(bt_order_ogua$Site),
                        FUN = function(x)c(median = median(x), 
                                           sd = sd(x),
                                           mean = mean(x), se= se(x)))
Blattodae3


kruskal.test(bt_order_ogua$Coleoptera ~ bt_order_ogua$Site)  
dunn.test(bt_order_ogua$Coleoptera, bt_order_ogua$Site, method = "bonferroni")
Coleoptera3 <- aggregate(bt_order_ogua$Coleoptera,
                         by = list(bt_order_ogua$Site),
                         FUN = function(x)c(median = median(x), 
                                            sd = sd(x),
                                            mean = mean(x), se= se(x)))
Coleoptera3


kruskal.test(bt_order_ogua$Dermaptera ~ bt_order_ogua$Site)  
dunn.test(bt_order_ogua$Dermaptera, bt_order_ogua$Site, method = "bonferroni")
Dermaptera3 <- aggregate(bt_order_ogua$Dermaptera,
                         by = list(bt_order_ogua$Site),
                         FUN = function(x) c(median = median(x), 
                                             sd = sd(x),
                                             mean = mean(x), se= se(x)))
Dermaptera3



kruskal.test(bt_order_ogua$Hemiptera ~ bt_order_ogua$Site)  
dunn.test(bt_order_ogua$Hemiptera, bt_order_ogua$Site, method = "bonferroni")
Hemiptera3 <- aggregate(bt_order_ogua$Hemiptera,
                        by = list(bt_order_ogua$Site),
                        FUN = function(x) c(median = median(x), 
                                            sd = sd(x),
                                            mean = mean(x), se= se(x)))
Hemiptera3


kruskal.test(bt_order_ogua$Hymenoptera ~ bt_order_ogua$Site)  
dunn.test(bt_order_ogua$Hymenoptera, bt_order_ogua$Site, method = "bonferroni")
Hymenoptera3 <- aggregate(bt_order_ogua$Hymenoptera,
                          by = list(bt_order_ogua$Site),
                          FUN = function(x) c(median = median(x), 
                                             sd = sd(x),
                                             mean = mean(x), se= se(x)))
Hymenoptera3


kruskal.test(bt_order_ogua$Isopoda ~ bt_order_ogua$Site)  
dunn.test(bt_order_ogua$Isopoda, bt_order_ogua$Site, method = "bonferroni")
Isopoda3 <- aggregate(bt_order_ogua$Isopoda,
                      by = list(bt_order_ogua$Site),
                      FUN = function(x) c(median = median(x), 
                                          sd = sd(x),
                                          mean = mean(x), se= se(x)))
Isopoda3



kruskal.test(bt_order_ogua$Orthoptera ~ bt_order_ogua$Site)  
dunn.test(bt_order_ogua$Orthoptera, bt_order_ogua$Site, method = "bonferroni")
Orthoptera3 <- aggregate(bt_order_ogua$Orthoptera,
                         by = list(bt_order_ogua$Site),
                         FUN = function(x) c(median = median(x), 
                                             sd = sd(x),
                                             mean = mean(x), se= se(x)))
Orthoptera3

kruskal.test(bt_order_ogua$Spirostreptida ~ bt_order_ogua$Site)  
dunn.test(bt_order_ogua$Spirostreptida, bt_order_ogua$Site, method = "bonferroni")
Spirostreptida3 <- aggregate(bt_order_ogua$Spirostreptida,
                             by = list(bt_order_ogua$Site),
                             FUN = function(x) c(median = median(x), 
                                                 sd = sd(x),
                                                 mean = mean(x), se= se(x)))
Spirostreptida3


kruskal.test(bt_order_ogua$Unknown ~ bt_order_ogua$Site)  
dunn.test(bt_order_ogua$Unknown, bt_order_ogua$Site, method = "bonferroni")
Unknown3 <- aggregate(bt_order_ogua$Unknown,
                      by = list(bt_order_ogua$Site),
                      FUN = function(x) c(median = median(x), 
                                          sd = sd(x),
                                          mean = mean(x), se= se(x)))
Unknown3




bt_order_igue <- bt_order %>% 
  filter(Location== "Igueosagie")


kruskal.test(bt_order_igue$Araneae ~ bt_order_igue$Site)  
dunn.test(bt_order_igue$Araneae, bt_order_igue$Site, method = "bonferroni")
Araneae4 <- aggregate(bt_order_igue$Araneae,
                      by = list(bt_order_igue$Site),
                      FUN = function(x) c(median = median(x), 
                                          sd = sd(x),
                                          mean = mean(x), se= se(x)))
Araneae4


kruskal.test(bt_order_igue$Blattodea ~ bt_order_igue$Site)  
dunn.test(bt_order_igue$Blattodea, bt_order_igue$Site, method = "bonferroni")
Blattodae4 <- aggregate(bt_order_igue$Blattodea,
                        by = list(bt_order_igue$Site),
                        FUN = function(x) c(median = median(x), 
                                            sd = sd(x),
                                            mean = mean(x), se= se(x)))
Blattodae4


kruskal.test(bt_order_igue$Coleoptera ~ bt_order_igue$Site)  
dunn.test(bt_order_igue$Coleoptera, bt_order_igue$Site, method = "bonferroni")
Coleoptera4 <- aggregate(bt_order_igue$Coleoptera,
                         by = list(bt_order_igue$Site),
                         FUN = function(x) c(median = median(x), 
                                            sd = sd(x),
                                            mean = mean(x), se= se(x)))
Coleoptera4


kruskal.test(bt_order_igue$Dermaptera ~ bt_order_igue$Site)  
dunn.test(bt_order_igue$Dermaptera, bt_order_igue$Site, method = "bonferroni")
Dermaptera4 <- aggregate(bt_order_igue$Dermaptera,
                         by = list(bt_order_igue$Site),
                         FUN = function(x) c(median = median(x), 
                                             sd = sd(x),
                                             mean = mean(x), se= se(x)))
Dermaptera4



kruskal.test(bt_order_igue$Hemiptera ~ bt_order_igue$Site)  
dunn.test(bt_order_igue$Hemiptera, bt_order_igue$Site, method = "bonferroni")
Hemiptera4 <- aggregate(bt_order_igue$Hemiptera,
                        by = list(bt_order_igue$Site),
                        FUN = function(x) c(median = median(x), 
                                            sd = sd(x),
                                            mean = mean(x), se= se(x)))
Hemiptera4


kruskal.test(bt_order_igue$Hymenoptera ~ bt_order_igue$Site)  
dunn.test(bt_order_igue$Hymenoptera, bt_order_igue$Site, method = "bonferroni")
Hymenoptera4 <- aggregate(bt_order_igue$Hymenoptera,
                          by = list(bt_order_igue$Site),
                          FUN = function(x) c(median = median(x), 
                                              sd = sd(x),
                                              mean = mean(x), se= se(x)))
Hymenoptera4


kruskal.test(bt_order_igue$Isopoda ~ bt_order_igue$Site)  
dunn.test(bt_order_igue$Isopoda, bt_order_igue$Site, method = "bonferroni")
Isopoda4 <- aggregate(bt_order_igue$Isopoda,
                      by = list(bt_order_igue$Site),
                      FUN = function(x) c(median = median(x), 
                                          sd = sd(x),
                                          mean = mean(x), se= se(x)))
Isopoda4



kruskal.test(bt_order_igue$Orthoptera ~ bt_order_igue$Site)  
dunn.test(bt_order_igue$Orthoptera, bt_order_igue$Site, method = "bonferroni")
Orthoptera4 <- aggregate(bt_order_igue$Orthoptera,
                         by = list(bt_order_igue$Site),
                         FUN = function(x) c(median = median(x), 
                                             sd = sd(x),
                                             mean = mean(x), se= se(x)))
Orthoptera4

kruskal.test(bt_order_igue$Spirostreptida ~ bt_order_igue$Site)  
dunn.test(bt_order_igue$Spirostreptida, bt_order_igue$Site, method = "bonferroni")
Spirostreptida4 <- aggregate(bt_order_igue$Spirostreptida,
                             by = list(bt_order_igue$Site),
                             FUN = function(x) c(median = median(x), 
                                                 sd = sd(x),
                                                 mean = mean(x), se= se(x)))
Spirostreptida4


kruskal.test(bt_order_igue$Unknown ~ bt_order_igue$Site)  
dunn.test(bt_order_igue$Unknown, bt_order_igue$Site, method = "bonferroni")
Unknown4 <- aggregate(bt_order_igue$Unknown,
                      by = list(bt_order_igue$Site),
                      FUN = function(x) c(median = median(x), 
                                          sd = sd(x),
                                          mean = mean(x), se= se(x)))
Unknown4
