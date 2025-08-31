library(indicspecies)
library(vegan)
set.seed(123) 


# iguegosagie

iguegosagie.p.sim <- simper(iguegosagie.p %>%
                              dplyr::ungroup() %>%
                              dplyr::select(where(is.numeric)), 
              group = iguegosagie.p$Infestation_Gradient, permutations = 999)
summary(iguegosagie.p.sim)

iguegosagie.p.sim_summary <- summary(iguegosagie.p.sim)
iguegosagie.p.simper_df <- do.call(rbind, lapply(iguegosagie.p.sim_summary,
                                   function(x) as.data.frame(x)))
iguegosagie.p.sig <- iguegosagie.p.simper_df %>% filter(p <= 0.05)

iguegosagie.p.ind <- multipatt(iguegosagie.p %>%
                                      dplyr::ungroup() %>%
                                      dplyr::select(where(is.numeric)), 
                                    iguegosagie.p$Season_Infestation,
                                    func = "IndVal.g", control = how(nperm=999))

summary(iguegosagie.p.ind, indvalcomp = TRUE)


iguegosagie.b.sim <- simper(iguegosagie.b %>%
                              dplyr::ungroup() %>%
                              dplyr::select(where(is.numeric)), 
                            group = iguegosagie.b$Infestation_Gradient, permutations = 999)
summary(iguegosagie.b.sim)

iguegosagie.b.sim_summary <- summary(iguegosagie.b.sim)
iguegosagie.b.simper_df <- do.call(rbind, lapply(iguegosagie.b.sim_summary,
                                                 function(x) as.data.frame(x)))
iguegosagie.b.sig <- iguegosagie.b.simper_df %>% filter(p <= 0.05)

iguegosagie.b.ind <- multipatt(iguegosagie.b %>%
                                 dplyr::ungroup() %>%
                                 dplyr::select(where(is.numeric)), 
                               iguegosagie.b$Season_Infestation,
                               func = "IndVal.g", control = how(nperm=999))

summary(iguegosagie.b.ind, indvalcomp = TRUE)



# Ogua

Ogua.b.sim <- simper(Ogua.b %>%
                       dplyr::ungroup() %>%
                       dplyr::select(where(is.numeric)), 
                     group = Ogua.b$Infestation_Gradient, permutations = 999)
summary(Ogua.b.sim)

Ogua.b.sim_summary <- summary(Ogua.b.sim)
Ogua.b.simper_df <- do.call(rbind, lapply(Ogua.b.sim_summary,
                                          function(x) as.data.frame(x)))
Ogua.b.sig <- Ogua.b.simper_df %>% filter(p <= 0.05)

Ogua.b.ind <- multipatt(Ogua.b %>%
                          dplyr::ungroup() %>%
                          dplyr::select(where(is.numeric)), 
                        Ogua.b$Season_Infestation,
                        func = "IndVal.g", control = how(nperm=999))

summary(Ogua.b.ind, indvalcomp = TRUE)


Ogua.p.sim <- simper(Ogua.p %>%
                       dplyr::ungroup() %>%
                       dplyr::select(where(is.numeric)), 
                     group = Ogua.p$Infestation_Gradient, permutations = 999)
summary(Ogua.p.sim)

Ogua.p.sim_summary <- summary(Ogua.p.sim)
Ogua.p.simper_df <- do.call(rbind, lapply(Ogua.p.sim_summary,
                                          function(x) as.data.frame(x)))
Ogua.p.sig <- Ogua.p.simper_df %>% filter(p <= 0.05)

Ogua.p.ind <- multipatt(Ogua.p %>%
                          dplyr::ungroup() %>%
                          dplyr::select(where(is.numeric)), 
                        Ogua.p$Season_Infestation,
                        func = "IndVal.g", control = how(nperm=999))

summary(Ogua.p.ind, indvalcomp = TRUE)


# Iguovbiobo

Iguovbiobo.p.sim <- simper(Iguovbiobo.p %>%
                             dplyr::ungroup() %>%
                             dplyr::select(where(is.numeric)), 
                           group = Iguovbiobo.p$Infestation_Gradient, permutations = 999)
summary(Iguovbiobo.p.sim)

Iguovbiobo.p.sim_summary <- summary(Iguovbiobo.p.sim)
Iguovbiobo.p.simper_df <- do.call(rbind, lapply(Iguovbiobo.p.sim_summary,
                                                function(x) as.data.frame(x)))
Iguovbiobo.p.sig <- Iguovbiobo.p.simper_df %>% filter(p <= 0.05)

Iguovbiobo.p.ind <- multipatt(Iguovbiobo.p %>%
                                dplyr::ungroup() %>%
                                dplyr::select(where(is.numeric)), 
                              Iguovbiobo.p$Season_Infestation,
                              func = "IndVal.g", control = how(nperm=999))

summary(Iguovbiobo.p.ind, indvalcomp = TRUE)


Iguovbiobo.b.sim <- simper(Iguovbiobo.b %>%
                             dplyr::ungroup() %>%
                             dplyr::select(where(is.numeric)), 
                           group = Iguovbiobo.b$Infestation_Gradient, permutations = 999)
summary(Iguovbiobo.b.sim)

Iguovbiobo.b.sim_summary <- summary(Iguovbiobo.b.sim)
Iguovbiobo.b.simper_df <- do.call(rbind, lapply(Iguovbiobo.b.sim_summary,
                                                function(x) as.data.frame(x)))
Iguovbiobo.b.sig <- Iguovbiobo.b.simper_df %>% filter(p <= 0.05)

Iguovbiobo.b.ind <- multipatt(Iguovbiobo.b %>%
                                dplyr::ungroup() %>%
                                dplyr::select(where(is.numeric)), 
                              Iguovbiobo.b$Season_Infestation,
                              func = "IndVal.g", control = how(nperm=999))

summary(Iguovbiobo.b.ind, indvalcomp = TRUE)




###   Ahor Urokosa

Ahor_Urokosa.p.sim <- simper(Ahor_Urokosa.p %>%
                               dplyr::ungroup() %>%
                               dplyr::select(where(is.numeric)), 
                             group = Ahor_Urokosa.p$Infestation_Gradient, permutations = 999)
summary(Ahor_Urokosa.p.sim)

Ahor_Urokosa.p.sim_summary <- summary(Ahor_Urokosa.p.sim)
Ahor_Urokosa.p.simper_df <- do.call(rbind, lapply(Ahor_Urokosa.p.sim_summary,
                                                  function(x) as.data.frame(x)))
Ahor_Urokosa.p.sig <- Ahor_Urokosa.p.simper_df %>% filter(p <= 0.05)

Ahor_Urokosa.p.ind <- multipatt(Ahor_Urokosa.p %>%
                                  dplyr::ungroup() %>%
                                  dplyr::select(where(is.numeric)), 
                                Ahor_Urokosa.p$Season_Infestation,
                                func = "IndVal.g", control = how(nperm=999))

summary(Ahor_Urokosa.p.ind, indvalcomp = TRUE)




Ahor_Urokosa.b.sim <- simper(Ahor_Urokosa.b %>%
                               dplyr::ungroup() %>%
                               dplyr::select(where(is.numeric)), 
                             group = Ahor_Urokosa.b$Infestation_Gradient, permutations = 999)
summary(Ahor_Urokosa.b.sim)

Ahor_Urokosa.b.sim_summary <- summary(Ahor_Urokosa.b.sim)
Ahor_Urokosa.b.simper_df <- do.call(rbind, lapply(Ahor_Urokosa.b.sim_summary,
                                                  function(x) as.data.frame(x)))
Ahor_Urokosa.b.sig <- Ahor_Urokosa.b.simper_df %>% filter(p <= 0.05)

Ahor_Urokosa.b.ind <- multipatt(Ahor_Urokosa.b %>%
                                  dplyr::ungroup() %>%
                                  dplyr::select(where(is.numeric)), 
                                Ahor_Urokosa.b$Season_Infestation,
                                func = "IndVal.g", control = how(nperm=999))

summary(Ahor_Urokosa.b.ind, indvalcomp = TRUE)