setwd("") #please set yout preferable working directory

#library####
library(readxl)
library(dplyr)
library(ggplot2)
library(geobr)
library(ggthemes)
library(ggsignif)
#library(factoextra)
#library(corrplot)
#library(RColorBrewer)
#library(tibble)
#library(tidyr)

#data####
read_xlsx('./dbcap2_clean.xlsx')-> dbcap2_clean #read to R the .xlsx file with all variables

#Analisys####
#PCA 2006 ####
dbcap2_clean%>%
  select(1, ends_with(c("06","00", "08")))%>%
  glimpse()-> dbcap2_06.2

prcomp(dbcap2_06.2[,-c(1,2)],scale. = T, center = T)->pca_06.2
summary(pca_06.2)
pca_06.2$rotation
ncomp_06.2<-12
pca.varimax_06.2 <- varimax(pca_06.2$rotation[, 1:ncomp_06.2])
pca.varimax_06.2$loadings 
str(pca.varimax_06.2$loadings)
rot_load_06.2<- data.frame(matrix(as.numeric(pca.varimax_06.2$loadings), attributes(pca.varimax_06.2$loadings)$dim, dimnames=attributes(pca.varimax_06.2$loadings)$dimnames))
rawLoadings_06.2<- pca_06.2$rotation[,1:ncomp_06.2] %*% diag(pca_06.2$sdev, ncomp_06.2, ncomp_06.2)
rotatedLoadings_06.2 <- varimax(rawLoadings_06.2)$loadings
invLoadings_06.2     <- t(pracma::pinv(rotatedLoadings_06.2))
scores_06.2          <- scale(dbcap2_06.2[,-c(1:2)]) %*% invLoadings_06.2
scores_06.2
mun_scores_06<-as_tibble(data.frame(dbcap2_06.2[,1], scores_06.2))
colnames(mun_scores_06)
mun_scores_06 
write.csv(mun_scores_06, "mun_scores_06.csv")

#PCA 2017####
dbcap2_clean%>%
  select(1, ends_with(c("17", "10", "15")))%>%
  glimpse()->dbcap2_17.2

prcomp(dbcap2_17.2[,-1:-2],scale. = T, center = T)->pca_17.2
summary(pca_17.2)
pca_17.2$rotation
ncomp_17.2<-12
pca.varimax_17.2 <- varimax(pca_17.2$rotation[, 1:ncomp_17.2])
pca.varimax_17.2$loadings 
str(pca.varimax_17.2$loadings)
rot_load_17.2<- data.frame(matrix(as.numeric(pca.varimax_17.2$loadings), attributes(pca.varimax_17.2$loadings)$dim,
                                  dimnames=attributes(pca.varimax_17.2$loadings)$dimnames))
rawLoadings_17.2<- pca_17.2$rotation[,1:ncomp_17.2] %*% diag(pca_17.2$sdev, ncomp_17.2, ncomp_17.2)
rotatedLoadings_17.2 <- varimax(rawLoadings_17.2)$loadings
invLoadings_17.2     <- t(pracma::pinv(rotatedLoadings_17.2))
scores_17.2          <- scale(dbcap2_17.2[,-c(1:2)]) %*% invLoadings_17.2
scores_17.2
mun_scores_17<-as_tibble(data.frame(dbcap2_17.2[,1], scores_17.2))
colnames(mun_scores_17)
mun_scores_17
write.csv(mun_scores_17, "mun_scores_17.csv")



#############################         Maps          #####################################
#Data and file for maps####
#Individual dimensions of PCA and final aggregated score - 2006####
read_municipality()-> mun_caat #All "read_" function from geobr packages might take a while to load
inner_join(mun_caat, mun_scores_06, by = "code_muni") ->map_scores_06
map_scores_06 %>%
  mutate(X1= scale(X1)) %>% 
  select(X1, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X1, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim1_06
map_scores_06 %>%
  mutate(X2= scale(X2)) %>%
  dplyr::select(X2, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X2, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim2_06
map_scores_06 %>%
  mutate(X3= scale(X3*-1)) %>% #We inverted the sign of some dimension according to the contribution of the variable that composed the PC towards food security
  dplyr::select(X3, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X3, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim3_06
map_scores_06 %>%
  mutate(X4= scale(X4*-1)) %>%
  dplyr::select(X4, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X4, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim4_06
map_scores_06 %>%
  mutate(X5= scale(X5)) %>%
  dplyr::select(X5, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X5, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim5_06
map_scores_06 %>%
  mutate(X6= scale(X6*-1)) %>%
  dplyr::select(X6, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X6, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim6_06
map_scores_06 %>%
  mutate(X7= scale(X7*-1)) %>%
  dplyr::select(X7, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X7, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim7_06
map_scores_06 %>%
  mutate(X8= scale(X8*-1)) %>%
  dplyr::select(X8, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X8, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim8_06
map_scores_06 %>%
  mutate(X9= scale(X9)) %>%
  dplyr::select(X9, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X9, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim9_06
map_scores_06 %>%
  mutate(X10= scale(X10*-1)) %>%
  dplyr::select(X10, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X10, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim10_06
map_scores_06 %>%
  mutate(X11= scale(X11*-1)) %>%
  dplyr::select(X11, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X11, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim11_06
map_scores_06 %>%
  mutate(X12= scale(X12*-1)) %>%
  dplyr::select(X12, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X12, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim12_06

read_country() ->brazil 

read_state() -> uf
uf %>% 
  filter(abbrev_state %in% c("BA","SE","PE", "AL","PB","RN","CE","PI","MG"))->uf_caat

data.frame(dim1_06$X1,dim2_06$X2,dim3_06$X3,dim4_06$X4,dim5_06$X5,dim6_06$X6,dim7_06$X7,dim8_06$X8,dim9_06$X9,
           dim10_06$X10, dim11_06$X11, dim12_06$X12) -> fs_score_06
names(fs_score_06)<-c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9", "PC10", "PC11", "PC12")
fs_score_06$fs_score<-scale(rowSums(fs_score_06))

#Individual dimensions of PCA and final aggregated score - 2017####
inner_join(mun_caat, mun_scores_17, by = "code_muni")-> map_scores_17
map_scores_17 %>%
  mutate(X1= scale(X1*-1)) %>% 
  select(X1, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X1, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim1_17
map_scores_17 %>%
  mutate(X2= scale(X2)) %>%
  dplyr::select(X2, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X2, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim2_17
map_scores_17 %>%
  mutate(X3= scale(X3*-1)) %>%
  dplyr::select(X3, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X3, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim3_17
map_scores_17 %>%
  mutate(X4= scale(X4)) %>%
  dplyr::select(X4, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X4, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim4_17
map_scores_17 %>%
  mutate(X5= scale(X5)) %>%
  dplyr::select(X5, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X5, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim5_17
map_scores_17 %>%
  mutate(X6= scale(X6*-1)) %>%
  dplyr::select(X6, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X6, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim6_17
map_scores_17 %>%
  mutate(X7= scale(X7*-1)) %>%
  dplyr::select(X7, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X7, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim7_17
map_scores_17 %>%
  mutate(X8= scale(X8*-1)) %>%
  dplyr::select(X8, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X8, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim8_17
map_scores_17 %>%
  mutate(X9= scale(X9*-1)) %>%
  dplyr::select(X9, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X9, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim9_17
map_scores_17 %>%
  mutate(X10= scale(X10*-1)) %>%
  dplyr::select(X10, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X10, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim10_17
map_scores_17 %>%
  mutate(X11= scale(X11*-1)) %>%
  dplyr::select(X11, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X11, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim11_17
map_scores_17 %>%
  mutate(X12= scale(X12*-1)) %>%
  dplyr::select(X12, geom) %>% 
  as.data.frame() %>% 
  mutate(category=cut(X12, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("very low","low","middle","high", "very high")))->dim12_17

data.frame(dim1_17$X1,dim2_17$X2,dim3_17$X3,dim4_17$X4,dim5_17$X5,dim6_17$X6,dim7_17$X7,dim8_17$X8,dim9_17$X9,
           dim10_17$X10, dim11_17$X11, dim12_17$X12) -> fs_score_17
names(fs_score_17)<-c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9", "PC10", "PC11", "PC12")
fs_score_17$fs_score<-scale(rowSums(fs_score_17))

inner_join(mun_caat, mun_scores_06, by = "code_muni")-> map_fsc

dbcap2_clean%>%
  mutate(fcc = nvcPerc_17 - nvcPerc_06)%>%
  select(1,2,fcc)%>%
  left_join(x = ., y = map_fsc)%>%
  glimpse() ->map_fsc

map_fsc%>%
  mutate(netFor = as.factor(if_else(condition = fcc > 0, true = "Positive", false = "Negative")))%>%
  glimpse()->map_fsc

data.frame(dbcap2_17.2[,1], fs_score_06$fs_score, fs_score_17$fs_score) -> fsc

fsc%>%
  rename(fs_06 = fs_score_06.fs_score, fs_17 = fs_score_17.fs_score)%>%
  mutate(fsc = fs_17 - fs_06)%>%
  glimpse()-> fsc.score

fsc.score %>% 
  mutate(category=cut(fsc, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                      labels=c("High lost","Lost","Stable","Gain", "High Gain")))->fsc.score_class
inner_join(mun_caat, fsc.score_class, by = "code_muni")-> map_fsc

dbcap2_clean%>%
  mutate(fcc = nvcPerc_17 - nvcPerc_06)%>%
  select(1,2,fcc)%>%
  left_join(x = ., y = map_fsc)%>%
  glimpse() ->map_fsc

map_fsc%>%
  mutate(relation = as.factor(if_else(condition = fcc > 0 & fsc>0, true = "win-win",
                                      false = if_else(condition = fcc > 0 & fsc < 0, true = "win-lose",
                                                      false = if_else(condition = fcc< 0 & fsc > 0, true = "lose-win",
                                                                      false = if_else(condition = fcc<0 & fsc <0, true = "lose-lose",
                                                                                      false = "pizza"))))))%>%
  glimpse()-> map_fsc

#Map of Brazil with Caatinga highlighted####
ggplot()+
  geom_sf(data = brazil, fill = "antiquewhite1")+
  geom_sf(data = dim1_06$geom, aes(fill = "grey50"), lwd = 0)+
  scale_fill_manual(values = "grey50", labels = "Caatinga municipalities")+
  guides(fill=guide_legend(title=NULL))+
  geom_sf(data = uf, fill = "transparent")+
  scale_x_continuous(breaks = c(-70, -55, -40))+
  scale_y_continuous(breaks = c(0, -15, -30))+
  theme(text = element_text(family = '', size = 6),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.8, 0.2),
        legend.key.size = unit(2, "mm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) -> fig1a
fig1a

#Map of Forest cover change####
ggplot()+
  geom_sf(data=map_fsc$geom, aes(fill = map_fsc$netFor), lwd=0)+
  scale_fill_manual(values = c("#dfc27d", "#2c7bb6"),name="Forest Cover\nchange")+
  geom_sf(data=uf_caat, fill="transparent")+
  coord_sf(xlim = c(-48, -34), ylim = c(-17.1, -3))+
  geom_text(data = uf_caat, aes(x= c(-42, -39.5,-36.5,-35.5, -34.5, -34.4, -36, -39,-42.4),
                                y = c(-16.8, -15, -11, -10, -8.5, -7, -4.7, -2.9, -5),
                                label = c("MG", "BA", "SE", "AL", "PE", "PB", "RN", "CE", "PI")),
            size = 2)+
  scale_x_continuous(breaks = c(-48, -42, -36))+
  scale_y_continuous(breaks = c(-4,-10, -16))+
  theme(text = element_text(family = '', size = 6),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.key.size = unit(2, "mm"),
        legend.position = c(0.8, 0.2),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8))-> fcc.map
fcc.map

#Map of food security change####
ggplot()+
  geom_sf(data=map_fsc$geom, aes(fill = (fsc.score_class$category)),lwd=0)+
  scale_fill_brewer(palette="RdYlBu", name = "Food Security\nchange")+
  geom_sf(data=uf_caat, fill="transparent")+
  coord_sf(xlim = c(-48, -34), ylim = c(-17.1, -3))+
  geom_text(data = uf_caat, aes(x= c(-42, -39.5,-36.5,-35.5, -34.5, -34.4, -36, -39,-42.4),
                                y = c(-16.8, -15, -11, -10, -8.5, -7, -4.7, -2.9, -5),
                                label = c("MG", "BA", "SE", "AL", "PE", "PB", "RN", "CE", "PI")),
            size = 2)+
  scale_x_continuous(breaks = c(-48, -42, -36))+
  scale_y_continuous(breaks = c(-4,-10, -16))+
  theme(text = element_text(family = '', size = 6),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.key.size = unit(2, "mm"),
        legend.position = c(0.9, 0.2),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8))-> fsc.map
fsc.map

#Map of forest cover and food security relationship####
ggplot()+
  geom_sf(data=map_fsc$geom, aes(fill = map_fsc$relation), lwd=0)+ # chamar map_fsc$relation do script analysis_cap2.R
  scale_fill_manual(values = c("#d7191c",  "#fdae61", "#abd9e9", "#2c7bb6"), 
                    name="Forest-Food\nrelationship")+
  geom_sf(data=uf_caat, fill="transparent")+
  coord_sf(xlim = c(-48, -34), ylim = c(-17.1, -3))+
  geom_text(data = uf_caat, aes(x= c(-42, -39.5,-36.5,-35.5, -34.5, -34.4, -36, -39,-42.4),
                                y = c(-16.8, -15, -11, -10, -8.5, -7, -4.7, -2.9, -5),
                                label = c("MG", "BA", "SE", "AL", "PE", "PB", "RN", "CE", "PI")),
            size = 2)+
  scale_x_continuous(breaks = c(-48, -42, -36))+
  scale_y_continuous(breaks = c(-4,-10, -16))+
  theme(text = element_text(family = '', size = 6),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.key.size = unit(2, "mm"),
        legend.position = c(0.9, 0.2),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8))-> fcc_fsc.map
fcc_fsc.map

#Generalized Linear Models####
map_fsc%>%
  inner_join(x = ., y = select(dbcap2_clean, code_muni, nvcPerc_06, nvcPerc_17))%>%
  glimpse() ->a

#2006
glm(data = a, fs_06 ~ nvcPerc_06 + I(nvcPerc_06^2)) -> glmq.fs06_fc06
summary(glmq.fs06_fc06)

ggplot(a, aes(x = nvcPerc_06, y = fs_06))+
  geom_point(colour = "grey50", size= 1, alpha = 1/3)+
  geom_abline(slope = 0, intercept = 0)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.5, fill = "grey20")+
  theme_base()+
  theme(legend.position = "none")+
  labs(x = "Forest cover in 2006 (%)", y = "Food security in 2006")+
  theme(text = element_text(family = '', size = 8),
        plot.background = element_blank())-> fs_fc_06
fs_fc_06

#2017
glm(data = a, fs_17 ~ nvcPerc_17 + I(nvcPerc_17^2)) -> glmq.fs17_fc17
summary(glmq.fs17_fc17)

ggplot(a, aes(x = nvcPerc_17, y = fs_17))+
  geom_point(colour = "grey50", size= 1, alpha = 1/3)+
  geom_abline(slope = 0, intercept = 0)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.5, fill = "grey20")+
  theme_base()+
  theme(legend.position = "none")+
  labs(x = "Forest cover in 2017 (%)", y = "Food security in 2017")+
  theme(text = element_text(family = '', size = 8),
        plot.background = element_blank())-> fs_fc_17
fs_fc_17

#ANOVA and Boxplots of inequality and poverty####
a%>%
  inner_join(y = mun_scores_17)%>%
  mutate(forCateg=cut(nvcPerc_17, breaks=c(0, 25, 50, 75, 100), 
                      labels=c("0-25","25-50","50-75","75-100")))%>%
  glimpse()-> a.pcs17

#PC 1 - Poverty
lm(data = a.pcs17, X1 ~ forCateg)-> x1_17
summary(x1_17)
aov(x1_17)-> aov_x1_17
TukeyHSD(aov_x1_17)

ggplot(a.pcs17, aes(x = forCateg, y = X1))+
  geom_boxplot()+
  ylim (-4, 3.5)+
  xlab("Forest cover 2017 (%)")+
  ylab("Poverty (PC1 - 26.22%)")+
  geom_signif(y_position = c(3.2, 3.2, 3.2, 3.2), 
              xmin = c(0.95, 1.95, 2.95, 3.95),
              xmax = c(1.05, 2.05, 3.05, 4.05),
              annotations = c("ac","ab","b", "c"),
              tip_length = 0,
              textsize = 3)+
  theme(text = element_text(family = '', size = 8),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA))-> bp_pov
bp_pov

#PC2 - Inequality
lm(data = a.pcs17, X2 ~ forCateg)-> x2_17
summary(x2_17)
aov(x2_17)-> aov_x2_17
TukeyHSD(aov_x2_17)

ggplot(a.pcs17, aes(x = forCateg, y = X2))+
  geom_boxplot()+
  ylim (-5, 5)+
  xlab("Forest cover 2017 (%)")+
  ylab("Inequality (PC2 - 8.25%)")+
  geom_signif(y_position = c(2.8, 3.8, 3.8, 3.5), 
              xmin = c(0.95, 1.95, 2.95, 3.95),
              xmax = c(1.05, 2.05, 3.05, 4.05),
              annotations = c("a","ab","b", "c"),
              tip_length = 0,
              textsize = 3)+
  theme(text = element_text(family = '', size = 8),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA))->bp_ineq
bp_ineq
