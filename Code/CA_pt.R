library(factoextra)
library(FactoMineR)


library(dplyr)
igue_pt <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\IgueNMDS_pt.csv")
ogua_pt <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\oguaNMDS_pt.csv")

ogua_pt$Site <-factor(ogua_pt$Site)

ogua.table <-ogua_pt %>%
  group_by(Site) %>%
  summarise(across(-Sample, sum))
ogua.table<- as.data.frame(ogua.table)

row.names(ogua.table) <- ogua.table$Site

# Remove the first column (if needed)
ogua.table <- ogua.table[, -1]

chisq.ogua <- chisq.test(ogua.table)

res.ca.ogua <- CA(ogua.table, graph = FALSE)
summary(res.ca.ogua)

get.eig.val <- get_eigenvalue(res.ca.ogua)

fviz_screeplot(res.ca.ogua, addlabels = TRUE, ylim = c(0, 50)) ## 100


biplot_ogua <-fviz_ca_biplot(res.ca.ogua, alpha.col = 0.5,
               map ="colgreen", arrow = c(TRUE, FALSE),
               repel = TRUE,
               col.col = "black", 
               col.row = "red",
               pointsize = 2.5, size.text = 1)+
  theme_classic()

ogua.desc <- dimdesc(res.ca.ogua, axes = c(1,2))


##########################################################################

igue.table <-igue_pt %>%
  group_by(Site) %>%
  summarise(across(-Sample, sum))
igue.table<- as.data.frame(igue.table)

row.names(igue.table) <- igue.table$Site

igue.table <- igue.table[, -1]

chisq.igue <- chisq.test(igue.table)

res.ca.igue <- CA(igue.table, graph = FALSE)
summary(res.ca.igue)

get.eig.val.igue <- get_eigenvalue(res.ca.igue)

fviz_screeplot(res.ca.igue, addlabels = TRUE, ylim = c(0, 50)) ## 100


biplot_igue<- fviz_ca_biplot(res.ca.igue, alpha.col = 0.5,
               map ="colgreen", arrow = c(TRUE, FALSE),
               repel = TRUE,
               col.col = "black", 
               col.row = "red",
               pointsize = 2.5,
               size.text = 1)+
  theme_classic()

################### Beating tray #########################################3
ogua_CA_BT <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\Ogua CA.csv")
Igue_CA_BT <- read.csv("C:\\Users\\HP\\Documents\\Chromolaena-project\\Data\\Igue CA.csv")
ogua_pt$Site <-factor(ogua_pt$Site)

row.names(ogua_CA_BT) <- ogua_CA_BT$Level
ogua_CA_BT <- ogua_CA_BT[, -1]

row.names(Igue_CA_BT) <- Igue_CA_BT$Level
Igue_CA_BT <- Igue_CA_BT[, -1]  #### careful here with the first column


res.ca.ogua_bt <- CA(ogua_CA_BT, graph = FALSE)
res.ca.igue_bt <- CA(Igue_CA_BT, graph = FALSE)

biplot_ogua_bt<- fviz_ca_biplot(res.ca.ogua_bt, alpha.col = 0.5,
                                map ="colgreen", arrow = c(TRUE, FALSE),
                                repel = TRUE,
                                col.col = "black", 
                                col.row = "red",
                                pointsize = 2.5,
                                size.text = 1)+
  theme_classic()


biplot_igue_bt<- fviz_ca_biplot(res.ca.igue_bt, alpha.col = 0.5,
                             map ="colgreen", arrow = c(TRUE, FALSE),
                             repel = TRUE,
                             col.col = "black", 
                             col.row = "red",
                             pointsize = 2.5,
                             size.text = 1)+
  theme_classic()

