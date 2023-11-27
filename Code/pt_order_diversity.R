library(tidyverse)
library(dunn.test)
library(FSA)

pt_order <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\pt_order_div.csv",
                     stringsAsFactors = TRUE)
View(pt_order)
attach(pt_order)

pt_order_ogua <- pt_order %>% 
  filter(Location== "Ogua")

pt_order_igue <- pt_order %>% 
  filter(Location== "Igueosagie")

####################### OGUA


kruskal.test(pt_order_ogua$Araneae ~ pt_order_ogua$Site)  
dunn.test(pt_order_ogua$Araneae, pt_order_ogua$Site, method = "bonferroni")
Araneae1 <- aggregate(pt_order_ogua$Araneae,
                     by = list(pt_order_ogua$Site),
                     FUN = function(x) c(median = median(x), 
                                         mean = mean(x), sd = sd(x), se= se(x)))
Araneae1


kruskal.test(pt_order_ogua$Blattodae ~ pt_order_ogua$Site)  
dunn.test(pt_order_ogua$Blattodae, pt_order_ogua$Site, method = "bonferroni")
Blattodae1 <- aggregate(pt_order_ogua$Blattodae,
                      by = list(pt_order_ogua$Site),
                      FUN = function(x) c(median = median(x), 
                                          mean = mean(x), sd = sd(x), se= se(x)))
Blattodae1


kruskal.test(pt_order_ogua$Coleoptera ~ pt_order_ogua$Site)  
dunn.test(pt_order_ogua$Coleoptera, pt_order_ogua$Site, method = "bonferroni")
Coleoptera1 <- aggregate(pt_order_ogua$Coleoptera,
                        by = list(pt_order_ogua$Site),
                        FUN = function(x) c(median = median(x), 
                                            mean = mean(x), sd = sd(x), se= se(x)))
Coleoptera1


kruskal.test(pt_order_ogua$Dermaptera ~ pt_order_ogua$Site)  
dunn.test(pt_order_ogua$Dermaptera, pt_order_ogua$Site, method = "bonferroni")
Dermaptera1 <- aggregate(pt_order_ogua$Dermaptera,
                         by = list(pt_order_ogua$Site),
                         FUN = function(x) c(median = median(x), 
                                             mean = mean(x), sd = sd(x), se= se(x)))
Dermaptera1


kruskal.test(pt_order_ogua$Diptera ~ pt_order_ogua$Site)  
dunn.test(pt_order_ogua$Diptera, pt_order_ogua$Site, method = "bonferroni")
Diptera1 <- aggregate(pt_order_ogua$Diptera,
                         by = list(pt_order_ogua$Site),
                         FUN = function(x) c(median = median(x), 
                                             mean = mean(x), sd = sd(x), se= se(x)))
Diptera1


kruskal.test(pt_order_ogua$Hemiptera ~ pt_order_ogua$Site)  
dunn.test(pt_order_ogua$Hemiptera, pt_order_ogua$Site, method = "bonferroni")
Hemiptera1 <- aggregate(pt_order_ogua$Hemiptera,
                      by = list(pt_order_ogua$Site),
                      FUN = function(x) c(median = median(x), 
                                          mean = mean(x), sd = sd(x), se= se(x)))
Hemiptera1


kruskal.test(pt_order_ogua$Hymenoptera ~ pt_order_ogua$Site)  
dunn.test(pt_order_ogua$Hymenoptera, pt_order_ogua$Site, method = "bonferroni")
Hymenoptera1 <- aggregate(pt_order_ogua$Hymenoptera,
                        by = list(pt_order_ogua$Site),
                        FUN = function(x) c(median = median(x), 
                                            sd = sd(x),
                                            mean = mean(x), se= se(x)))
Hymenoptera1


kruskal.test(pt_order_ogua$Isopoda ~ pt_order_ogua$Site)  
dunn.test(pt_order_ogua$Isopoda, pt_order_ogua$Site, method = "bonferroni")
Isopoda1 <- aggregate(pt_order_ogua$Isopoda,
                          by = list(pt_order_ogua$Site),
                          FUN = function(x) c(median = median(x), 
                                              sd = sd(x),
                                              mean = mean(x), se= se(x)))
Isopoda1

kruskal.test(pt_order_ogua$Lepidoptera ~ pt_order_ogua$Site)  
dunn.test(pt_order_ogua$Lepidoptera, pt_order_ogua$Site, method = "bonferroni")
Lepidoptera1 <- aggregate(pt_order_ogua$Lepidoptera,
                      by = list(pt_order_ogua$Site),
                      FUN = function(x) c(median = median(x), 
                                          sd = sd(x),
                                          mean = mean(x), se= se(x)))
Lepidoptera1

kruskal.test(pt_order_ogua$Lithoblomorpha ~ pt_order_ogua$Site)  
dunn.test(pt_order_ogua$Lithoblomorpha, pt_order_ogua$Site, method = "bonferroni")
Lithoblomorpha1 <- aggregate(pt_order_ogua$Lithoblomorpha,
                          by = list(pt_order_ogua$Site),
                          FUN = function(x) c(median = median(x), 
                                              sd = sd(x),
                                              mean = mean(x), se= se(x)))
Lithoblomorpha1

kruskal.test(pt_order_ogua$Neuroptera ~ pt_order_ogua$Site)  
dunn.test(pt_order_ogua$Neuroptera, pt_order_ogua$Site, method = "bonferroni")
Neuroptera1 <- aggregate(pt_order_ogua$Neuroptera,
                             by = list(pt_order_ogua$Site),
                             FUN = function(x) c(median = median(x), 
                                                 sd = sd(x),
                                                 mean = mean(x), se= se(x)))
Neuroptera1

kruskal.test(pt_order_ogua$Oribatida ~ pt_order_ogua$Site)  
dunn.test(pt_order_ogua$Oribatida, pt_order_ogua$Site, method = "bonferroni")
Oribatida1 <- aggregate(pt_order_ogua$Oribatida,
                         by = list(pt_order_ogua$Site),
                         FUN = function(x) c(median = median(x), 
                                             sd = sd(x),
                                             mean = mean(x), se= se(x)))
Oribatida1

kruskal.test(pt_order_ogua$Orthoptera ~ pt_order_ogua$Site)  
dunn.test(pt_order_ogua$Orthoptera, pt_order_ogua$Site, method = "bonferroni")
Orthoptera1 <- aggregate(pt_order_ogua$Orthoptera,
                        by = list(pt_order_ogua$Site),
                        FUN = function(x) c(median = median(x), 
                                            sd = sd(x),
                                            mean = mean(x), se= se(x)))
Orthoptera1

kruskal.test(pt_order_ogua$Polydesmida ~ pt_order_ogua$Site)  
dunn.test(pt_order_ogua$Polydesmida, pt_order_ogua$Site, method = "bonferroni")
Polydesmida1 <- aggregate(pt_order_ogua$Polydesmida,
                         by = list(pt_order_ogua$Site),
                         FUN = function(x) c(median = median(x), 
                                             sd = sd(x),
                                             mean = mean(x), se= se(x)))
Polydesmida1

kruskal.test(pt_order_ogua$Scolopendromorpha ~ pt_order_ogua$Site)  
dunn.test(pt_order_ogua$Scolopendromorpha, pt_order_ogua$Site, method = "bonferroni")
Scolopendromorpha1 <- aggregate(pt_order_ogua$Scolopendromorpha,
                          by = list(pt_order_ogua$Site),
                          FUN = function(x) c(median = median(x), 
                                              sd = sd(x),
                                              mean = mean(x), se= se(x)))
Scolopendromorpha1

kruskal.test(pt_order_ogua$Spirostreptida ~ pt_order_ogua$Site)  
dunn.test(pt_order_ogua$Spirostreptida, pt_order_ogua$Site, method = "bonferroni")
Spirostreptida1 <- aggregate(pt_order_ogua$Spirostreptida,
                                by = list(pt_order_ogua$Site),
                                FUN = function(x) c(median = median(x), 
                                                    sd = sd(x),
                                                    mean = mean(x), se= se(x)))
Spirostreptida1


kruskal.test(pt_order_ogua$Thysanura ~ pt_order_ogua$Site)  
dunn.test(pt_order_ogua$Thysanura, pt_order_ogua$Site, method = "bonferroni")
Thysanura1 <- aggregate(pt_order_ogua$Thysanura,
                             by = list(pt_order_ogua$Site),
                             FUN = function(x) c(median = median(x), 
                                                 sd = sd(x),
                                                 mean = mean(x), se= se(x)))
Thysanura1

kruskal.test(pt_order_ogua$Unknown ~ pt_order_ogua$Site)  
dunn.test(pt_order_ogua$Unknown, pt_order_ogua$Site, method = "bonferroni")
Unknown1 <- aggregate(pt_order_ogua$Unknown,
                        by = list(pt_order_ogua$Site),
                        FUN = function(x) c(median = median(x), 
                                            sd = sd(x),
                                            mean = mean(x), se= se(x)))
Unknown1


##########  Igue pt


kruskal.test(pt_order_igue$Araneae ~ pt_order_igue$Site)  
dunn.test(pt_order_igue$Araneae, pt_order_igue$Site, method = "bonferroni")
Araneae2 <- aggregate(pt_order_igue$Araneae,
                      by = list(pt_order_igue$Site),
                      FUN = function(x) c(median = median(x), 
                                          sd = sd(x),  mean = mean(x),
                                          se= se(x)))
Araneae2


kruskal.test(pt_order_igue$Blattodae ~ pt_order_igue$Site)  
dunn.test(pt_order_igue$Blattodae, pt_order_igue$Site, method = "bonferroni")
Blattodae2 <- aggregate(pt_order_igue$Blattodae,
                        by = list(pt_order_igue$Site),
                        FUN = function(x) c(median = median(x), 
                                            sd = sd(x),  mean = mean(x),
                                            se= se(x)))
Blattodae2


kruskal.test(pt_order_igue$Coleoptera ~ pt_order_igue$Site)  
dunn.test(pt_order_igue$Coleoptera, pt_order_igue$Site, method = "bonferroni")
Coleoptera2 <- aggregate(pt_order_igue$Coleoptera,
                         by = list(pt_order_igue$Site),
                         FUN = function(x) c(median = median(x), 
                                             sd = sd(x),  mean = mean(x),
                                             se= se(x)))
Coleoptera2


kruskal.test(pt_order_igue$Dermaptera ~ pt_order_igue$Site)  
dunn.test(pt_order_igue$Dermaptera, pt_order_igue$Site, method = "bonferroni")
Dermaptera2 <- aggregate(pt_order_igue$Dermaptera,
                         by = list(pt_order_igue$Site),
                         FUN = function(x) c(median = median(x), 
                                             sd = sd(x),  mean = mean(x),
                                             se= se(x)))
Dermaptera2


kruskal.test(pt_order_igue$Diptera ~ pt_order_igue$Site)  
dunn.test(pt_order_igue$Diptera, pt_order_igue$Site, method = "bonferroni")
Diptera2 <- aggregate(pt_order_igue$Diptera,
                      by = list(pt_order_igue$Site),
                      FUN = function(x) c(median = median(x), 
                                          sd = sd(x),  mean = mean(x),
                                          se= se(x)))
Diptera2


kruskal.test(pt_order_igue$Hemiptera ~ pt_order_igue$Site)  
dunn.test(pt_order_igue$Hemiptera, pt_order_igue$Site, method = "bonferroni")
Hemiptera2 <- aggregate(pt_order_igue$Hemiptera,
                        by = list(pt_order_igue$Site),
                        FUN = function(x) c(median = median(x), 
                                            sd = sd(x),  mean = mean(x),
                                            se= se(x)))
Hemiptera2


kruskal.test(pt_order_igue$Hymenoptera ~ pt_order_igue$Site)  
dunn.test(pt_order_igue$Hymenoptera, pt_order_igue$Site, method = "bonferroni")
Hymenoptera2 <- aggregate(pt_order_igue$Hymenoptera,
                          by = list(pt_order_igue$Site),
                          FUN = function(x) c(median = median(x), 
                                              sd = sd(x),
                                              mean = mean(x), se= se(x)))
Hymenoptera2


kruskal.test(pt_order_igue$Isopoda ~ pt_order_igue$Site)  
dunn.test(pt_order_igue$Isopoda, pt_order_igue$Site, method = "bonferroni")
Isopoda2 <- aggregate(pt_order_igue$Isopoda,
                      by = list(pt_order_igue$Site),
                      FUN = function(x) c(median = median(x), 
                                          sd = sd(x),
                                          mean = mean(x), se= se(x)))
Isopoda2

kruskal.test(pt_order_igue$Lepidoptera ~ pt_order_igue$Site)  
dunn.test(pt_order_igue$Lepidoptera, pt_order_igue$Site, method = "bonferroni")
Lepidoptera2 <- aggregate(pt_order_igue$Lepidoptera,
                          by = list(pt_order_igue$Site),
                          FUN = function(x) c(median = median(x), 
                                              sd = sd(x),
                                              mean = mean(x), se= se(x)))
Lepidoptera2

kruskal.test(pt_order_igue$Lithoblomorpha ~ pt_order_igue$Site)  
dunn.test(pt_order_igue$Lithoblomorpha, pt_order_igue$Site, method = "bonferroni")
Lithoblomorpha2 <- aggregate(pt_order_igue$Lithoblomorpha,
                             by = list(pt_order_igue$Site),
                             FUN = function(x) c(median = median(x), 
                                                 sd = sd(x),
                                                 mean = mean(x), se= se(x)))
Lithoblomorpha2

kruskal.test(pt_order_igue$Neuroptera ~ pt_order_igue$Site)  
dunn.test(pt_order_igue$Neuroptera, pt_order_igue$Site, method = "bonferroni")
Neuroptera2 <- aggregate(pt_order_igue$Neuroptera,
                         by = list(pt_order_igue$Site),
                         FUN = function(x) c(median = median(x), 
                                             sd = sd(x),
                                             mean = mean(x), se= se(x)))
Neuroptera2

kruskal.test(pt_order_igue$Oribatida ~ pt_order_igue$Site)  
dunn.test(pt_order_igue$Oribatida, pt_order_igue$Site, method = "bonferroni")
Oribatida2 <- aggregate(pt_order_igue$Oribatida,
                        by = list(pt_order_igue$Site),
                        FUN = function(x) c(median = median(x), 
                                            sd = sd(x),
                                            mean = mean(x), se= se(x)))
Oribatida2

kruskal.test(pt_order_igue$Orthoptera ~ pt_order_igue$Site)  
dunn.test(pt_order_igue$Orthoptera, pt_order_igue$Site, method = "bonferroni")
Orthoptera2 <- aggregate(pt_order_igue$Orthoptera,
                         by = list(pt_order_igue$Site),
                         FUN = function(x) c(median = median(x), 
                                             sd = sd(x),
                                             mean = mean(x), se= se(x)))
Orthoptera2

kruskal.test(pt_order_igue$Polydesmida ~ pt_order_igue$Site)  
dunn.test(pt_order_igue$Polydesmida, pt_order_igue$Site, method = "bonferroni")
Polydesmida2 <- aggregate(pt_order_igue$Polydesmida,
                          by = list(pt_order_igue$Site),
                          FUN = function(x) c(median = median(x), 
                                              sd = sd(x),
                                              mean = mean(x), se= se(x)))
Polydesmida2

kruskal.test(pt_order_igue$Scolopendromorpha ~ pt_order_igue$Site)  
dunn.test(pt_order_igue$Scolopendromorpha, pt_order_igue$Site, method = "bonferroni")
Scolopendromorpha2 <- aggregate(pt_order_igue$Scolopendromorpha,
                                by = list(pt_order_igue$Site),
                                FUN = function(x) c(median = median(x), 
                                                    sd = sd(x),
                                                    mean = mean(x), se= se(x)))
Scolopendromorpha2

kruskal.test(pt_order_igue$Spirostreptida ~ pt_order_igue$Site)  
dunn.test(pt_order_igue$Spirostreptida, pt_order_igue$Site, method = "bonferroni")
Spirostreptida2 <- aggregate(pt_order_igue$Spirostreptida,
                             by = list(pt_order_igue$Site),
                             FUN = function(x) c(median = median(x), 
                                                 sd = sd(x),
                                                 mean = mean(x), se= se(x)))
Spirostreptida2


kruskal.test(pt_order_igue$Thysanura ~ pt_order_igue$Site)  
dunn.test(pt_order_igue$Thysanura, pt_order_igue$Site, method = "bonferroni")
Thysanura2 <- aggregate(pt_order_igue$Thysanura,
                        by = list(pt_order_igue$Site),
                        FUN = function(x) c(median = median(x), 
                                            sd = sd(x),
                                            mean = mean(x), se= se(x)))
Thysanura2

kruskal.test(pt_order_igue$Unknown ~ pt_order_igue$Site)  
dunn.test(pt_order_igue$Unknown, pt_order_igue$Site, method = "bonferroni")
Unknown2 <- aggregate(pt_order_igue$Unknown,
                      by = list(pt_order_igue$Site),
                      FUN = function(x) c(median = median(x), 
                                          sd = sd(x),
                                          mean = mean(x), se= se(x)))
Unknown2

