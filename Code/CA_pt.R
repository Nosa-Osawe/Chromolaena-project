library(factoextra)
library(FactoMineR)


library(dplyr)
igue_pt <- read.csv("C:\\Users\\HP\\Desktop\\IgueNMDS_pt.csv")
ogua_pt <- read.csv("C:\\Users\\HP\\Desktop\\oguaNMDS_pt.csv")

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


fviz_ca_biplot(res.ca.ogua, alpha.col = 0.5,
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


fviz_ca_biplot(res.ca.igue, alpha.col = 0.5,
               map ="colgreen", arrow = c(TRUE, FALSE),
               repel = TRUE,
               col.col = "black", 
               col.row = "red",
               pointsize = 2.5,
               size.text = 1)+
  theme_classic()






