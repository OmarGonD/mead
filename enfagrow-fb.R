#setwd("D:\\Starcom\\mead\\2017\\feb")

setwd("C:\\d\\Mead & Johnson\\2017\\feb\\mead")


library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(hrbrthemes)


mead_feb17 <- read_excel("2017-feb-mead.xlsx", sheet = 1)

colnames(mead_feb17)[2] <- "Campana"
colnames(mead_feb17)[3] <- "Conjunto de anuncios"
colnames(mead_feb17)[4] <- "Anuncio"
colnames(mead_feb17)[6] <- "Impresiones"
colnames(mead_feb17)[10] <- "Engagement"
colnames(mead_feb17)[10] <- "Frecuencia"
colnames(mead_feb17)[11] <- "Costo"
colnames(mead_feb17)[12] <- "Alcance"


# mead_feb17_resumen <- mead_feb17 %>%
#                     group_by(Campana,`Conjunto de anuncios`,`Anuncio`) %>%
#                     summarise(Alcance = sum(Reach),
#                               `Costo por resultados` = sum(`Costo por resultados`),
#                               `Importe gastado (USD)` = sum(`Importe gastado (USD)`),
#                               `Clics (todos)` = sum(`Clics (todos)`))








mead_feb17$Alcance <- as.numeric(mead_feb17$Alcance)
#mead_feb17$`Costo por resultados` <- as.numeric(mead_feb17$`Costo por resultados`)
mead_feb17$`Costo` <- as.numeric(mead_feb17$`Costo`)
mead_feb17$`Clicks` <- as.numeric(mead_feb17$Clicks)
mead_feb17$Impresiones <- as.numeric(mead_feb17$Impresiones)
mead_feb17$Frecuencia <- as.numeric(mead_feb17$Frecuencia)




# mead_feb17 <- mead_feb17 %>%
#             mutate(Frecuencia = Frecuencia/100)





mead_feb17$Anuncio <- gsub("(^PP[A-Z].*- |Carrusel.*- )", "", mead_feb17$Anuncio)




mead_feb17$Anuncio <- gsub("Promo mi vasito y mi platito", "Promo mi vasito \ny mi platito", mead_feb17$Anuncio)




unique(mead_feb17$Anuncio)


mead_feb17_long <- gather(mead_feb17, condition, measurement, `Impresiones planeadas`:CTR, factor_key=FALSE)




mead_feb17_long <- mead_feb17_long %>%
                   filter(complete.cases(mead_feb17_long))





mead_feb17_ppa <- mead_feb17_long %>%
                       filter(grepl("PPA", `Conjunto de anuncios`))


mead_feb17_ppl <- mead_feb17_long %>%
                  filter(grepl("PPL", `Conjunto de anuncios`))




mead_feb17_carrusel <- mead_feb17_long %>%
                     filter(grepl("Carrusel", `Conjunto de anuncios`))



#################
### Gráficos ####
#################








#PPLs ordenando anuncios como factores


# mead_feb17_ppl$`Anuncio` <- factor(mead_feb17_ppl$`Anuncio`, levels = c("Época familiar", "Tarjeta navideña"),
#                                           ordered = TRUE)
# 
# 






### PPLs

#Impresiones




mead_feb17_ppl_imp <- mead_feb17_ppl %>%
                    filter(grepl("Impres",condition))


mead_feb17_ppl_imp$measurement <- as.numeric(mead_feb17_ppl_imp$measurement)






#####################################
### Cuando existe más de 1 anuncio ##
#####################################


# mead_feb17_ppa_imp$`Anuncio` <- factor(mead_feb17_ppa_imp$`Anuncio`,
#                                                 levels = rev(c("Promo vasito",
#                                                            "Saluda a tu pediatra",
#                                                            "Promo Vasito",
#                                                            "Cuentos",
#                                                            "Promoción escolar")),
#                                                 ordered = TRUE)


#####################################


mead_feb17_ppl_imp$condition <- factor(mead_feb17_ppl_imp$condition,
                                                levels = rev(c("Impresiones",
                                                               "Impresiones planeadas")),
                                                ordered = TRUE)



ggplot(mead_feb17_ppl_imp, aes(x=factor(`Anuncio`), y=measurement, label=comma(round(measurement, 0)))) +
  facet_grid(. ~ condition) +
  geom_col(fill = "#20398C") +
  theme_ipsum(grid="Y") +
  labs(title="Enfabebé - PPLs - Impresiones", x = "", y = "") +
  theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
        plot.title = element_text(face = "bold", vjust=2, size = 22), 
        legend.title = element_text(colour="grey40", size=8, face="bold"),
        legend.text = element_text(colour="grey10", size=12, face="bold"),
        strip.text.x = element_text(size = 14,
                                    hjust = 0.1, vjust = 0.5)) +
  scale_y_continuous(labels = comma, limits = c(0, 2000000)) +
  geom_text(hjust=0.5, vjust=-0.5, size = 6) 
  




ggsave("ppls-imp.jpg", width = 12)


### PPLs Clicks ###




mead_feb17_ppl_clicks <- mead_feb17_ppl %>%
                         filter(grepl("Clicks",condition))


mead_feb17_ppl_clicks$measurement <- as.numeric(mead_feb17_ppl_clicks$measurement)





#####################################
### Cuando existe más de 1 anuncio ##
#####################################


# mead_feb17_ppa_imp$`Anuncio` <- factor(mead_feb17_ppa_imp$`Anuncio`,
#                                                 levels = rev(c("Promo vasito",
#                                                            "Saluda a tu pediatra",
#                                                            "Promo Vasito",
#                                                            "Cuentos",
#                                                            "Promoción escolar")),
#                                                 ordered = TRUE)


#####################################






mead_feb17_ppl_clicks$condition <- factor(mead_feb17_ppl_clicks$condition,
                                       levels = rev(c("Clicks",
                                                      "Clicks planeados")),
                                       ordered = TRUE)






ggplot(mead_feb17_ppl_clicks, aes(x=factor(`Anuncio`), y=measurement, label=comma(round(measurement, 0)))) +
  facet_grid(. ~ condition) +
  geom_col(fill = "#E5B07C") +
  theme_ipsum(grid="Y") +
  labs(title="Enfabebé - PPLs - Clicks", x = "", y = "") +
  theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
        plot.title = element_text(face = "bold", vjust=2, size = 22), 
        legend.title = element_text(colour="grey40", size=8, face="bold"),
        legend.text = element_text(colour="grey10", size=12, face="bold"),
        strip.text.x = element_text(size = 16,
                                    hjust = 0.1, vjust = 0.5)) +
  scale_y_continuous(labels = comma, limits = c(0, 25000)) +
  geom_text(hjust=0.5, vjust=-0.5, size = 8) 
   




ggsave("ppls-clicks.jpg", width = 12)


### TERMINA CLICKS ###



  
#CTR





mead_feb17_ppl_ctr <- mead_feb17_ppl %>%
                      filter(grepl("CTR",condition))




unique(mead_feb17_ppa_ctr$`Anuncio`)

# 
# 
# mead_feb17_ppl_ctr$`Anuncio` <- factor(mead_feb17_ppl_ctr$`Anuncio`,
#                                                   levels = rev(c("Promo vasito",
#                                                                  "Saluda a tu pediatra",
#                                                                  "Promo Vasito",
#                                                                  "Cuentos",
#                                                                  "Promoción escolar")),
#                                                   ordered = TRUE)


unique(mead_feb17_ppa_clics$condition)





ggplot(mead_feb17_ppl_ctr, aes(x=factor(`Anuncio`), y=measurement, label=percent(round(measurement, 4)))) +
  facet_grid( ~ condition) +
  geom_point(color = "#626262", size = 10) +
  geom_point(color = "#E54994", size = 8) +
  theme_ipsum(grid="Y") +
  labs(title="Enfabebé - PPLs - CTR", x = "", y = "") +
  theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
        plot.title = element_text(face = "bold", vjust=2, size = 22), 
        legend.title = element_text(colour="grey40", size=8, face="bold"),
        legend.text = element_text(colour="grey10", size=12, face="bold"),
        strip.text.x = element_text(size = 22,
                                    hjust = 0.1, vjust = 0.5)) +
  scale_y_continuous(labels = percent, limits = c(0, 0.05)) +
  geom_text(hjust=0.5, vjust=-1.2, size = 8) 
  





ggsave("ppls-ctrs.jpg", width = 6)



# Alcance & Frecuencia




mead_feb17_ppl_alcance <- mead_feb17_ppl %>%
                        filter(grepl("Alcance",condition))




# mead_feb17_ppa_alcance$`Anuncio` <- factor(mead_feb17_ppa_alcance$`Anuncio`,
#                                                 levels = c("Promo vasito",
#                                                                "Saluda a tu pediatra",
#                                                                "Promo Vasito",
#                                                                "Cuentos",
#                                                                "Promoción escolar"),
#                                                 ordered = TRUE)
# 



#Alcance



ggplot(mead_feb17_ppl_alcance, aes(x=factor(`Anuncio`), y=measurement, label=comma(round(measurement, 4)))) +
  facet_grid( ~ condition) +
  geom_col(fill = "#FF9933") +
  theme_ipsum(grid="Y") +
  labs(title="Enfabebé - PPLs - Alcance", x = "", y ="") +
  theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
        plot.title = element_text(face = "bold", vjust=2, size = 22), 
        legend.title = element_text(colour="grey40", size=8, face="bold"),
        legend.text = element_text(colour="grey10", size=12, face="bold"),
        strip.text.x = element_text(size = 22,
                                    hjust = 0.5, vjust = 0.5)) +
  scale_y_continuous(labels = comma, limits = c(0, 2000000)) +
  geom_text(hjust=0.5, vjust=-1.2, size = 8) 
  



ggsave("ppls-alcance.jpg", width = 6)



#Frecuencia





mead_feb17_ppl_freq <- mead_feb17_ppl %>%
                        filter(grepl("Frecuencia",condition))



# 
# 
# mead_feb17_ppa_freq$`Anuncio` <- factor(mead_feb17_ppa_freq$`Anuncio`,
#                                                     levels = c("Promo vasito",
#                                                                "Saluda a tu pediatra",
#                                                                "Promo Vasito",
#                                                                "Cuentos",
#                                                                "Promoción escolar"),
#                                                     ordered = TRUE)
# 






ggplot(mead_feb17_ppl_freq, aes(x=factor(`Anuncio`), y=measurement, label=round(measurement, 2))) +
  facet_grid( ~ condition) +
  theme_ipsum(grid="Y") +
  geom_point(color = "#626262", size = 10) +
  geom_point(color = "#268FE0", size = 8) +
  labs(title="Enfabebé - PPLs - Frecuencia", x = "", y ="") +
  theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
        plot.title = element_text(face = "bold", vjust=2, size = 22), 
        legend.title = element_text(colour="grey40", size=8, face="bold"),
        legend.text = element_text(colour="grey10", size=12, face="bold"),
        strip.text.x = element_text(size = 22,
                                    hjust = 0.1, vjust = 0.5)) +
  scale_y_continuous(labels = comma, limits = c(0, 5)) +
  geom_text(hjust=0.5, vjust=-1.2, size = 8) +
  



ggsave("ppls-frecuencia.jpg", width = 6)



#Engagement



### Revisar Engagement


# 
# 
# 
# mead_feb17_ppl_eng <- mead_feb17_ppl %>%
#                     filter(grepl("Enga",condition))



# 
# 
# mead_feb17_ppa_eng$`Anuncio` <- factor(mead_feb17_ppa_eng$`Anuncio`,
#                                                  levels = c("Promo vasito",
#                                                             "Saluda a tu pediatra",
#                                                             "Promo Vasito",
#                                                             "Cuentos",
#                                                             "Promoción escolar"),
#                                                  ordered = TRUE)




# 
# 
# 
# ggplot(mead_feb17_ppl_eng, aes(x=factor(`Anuncio`), y=measurement, label=percent(round(measurement, 4)))) +
#   facet_grid( ~ condition) +
#   geom_point(color = "#626262", size = 10) +
#   geom_point(color = "#C094C1", size = 8) +
#   theme_light() +
#   labs(title="Enfabebé - PPLs - Engagement", x = "") +
#   theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
#         axis.text.y = element_text(colour="grey20",size=18,hjust=1,vjust=0,face="plain"),  
#         axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
#         axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
#         plot.title = element_text(face = "bold", vjust=2, size = 22), 
#         legend.title = element_text(colour="grey40", size=8, face="bold"),
#         legend.text = element_text(colour="grey10", size=12, face="bold"),
#         strip.text.x = element_text(size = 22,
#                                     hjust = 0.5, vjust = 0.5)) +
#   scale_y_continuous(labels = percent, limits = c(0, 0.08)) +
#   geom_text(hjust=0.5, vjust=-1.2, size = 8)
# 
# 
# 
# ggsave("ppas-engagement.jpg", width = 14)






########################################
########################################

########################################
############# PPAs 28 ##################
########################################


#Impresiones




#Impresiones

unique(mead_feb17_ppa$Anuncio)


mead_feb17_ppa_imp <- mead_feb17_ppa %>%
                    select(-c(Campana,`Conjunto de anuncios`)) %>%
                    group_by(Anuncio, condition) %>%
                    filter(grepl("Impres",condition)) %>%
                    summarise(measurement = sum(measurement))








mead_feb17_ppa_imp$condition <- factor(mead_feb17_ppa_imp$condition,
                                     levels = rev(c("Impresiones",
                                                    "Impresiones planeadas")),
                                     ordered = TRUE)


mead_feb17_ppa_imp$Anuncio <- factor(mead_feb17_ppa_imp$Anuncio,
                                       levels = rev(c("Gatea", "Mamá",
                                                      "Promo mi vasito \ny mi platito")),
                                       ordered = TRUE)



ggplot(mead_feb17_ppa_imp, aes(x=factor(`Anuncio`), y=measurement, label=comma(round(measurement, 0)))) +
  facet_grid(. ~ condition) +
  geom_col(fill = "#20398C") +
  theme_ipsum(grid="Y") +
  labs(title="Enfabebé - PPAs - Impresiones", x = "", y = "") +
  theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,angle = 15,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
        plot.title = element_text(face = "bold", vjust=2, size = 22), 
        legend.title = element_text(colour="grey40", size=8, face="bold"),
        legend.text = element_text(colour="grey10", size=12, face="bold"),
        strip.text.x = element_text(size = 18,
                                    hjust = 0.1, vjust = 0.5)) +
  scale_y_continuous(labels = comma, limits = c(0, 4000000)) +
  geom_text(hjust=+0.5, vjust=-0.5, size = 8) 
   




ggsave("ppas-imp.jpg", width = 12)


#Clicks





mead_feb17_ppa_clicks <- mead_feb17_ppa %>%
                          group_by(Anuncio, condition) %>%
                          filter(grepl("Clicks",condition)) %>%
                          summarise(measurement = sum(measurement))








mead_feb17_ppa_clicks$condition <- factor(mead_feb17_ppa_clicks$condition,
                                       levels = rev(c("Clicks",
                                                      "Clicks planeados")),
                                       ordered = TRUE)




mead_feb17_ppa_clicks$Anuncio <- factor(mead_feb17_ppa_clicks$Anuncio,
                                     levels = rev(c("Gatea", "Mamá",
                                                    "Promo mi vasito \ny mi platito")),
                                     ordered = TRUE)



# 
# mead_feb17_ppa_clicks$Anuncio <- factor(mead_feb17_ppa_clicks$Anuncio,
#                                      levels = rev(c("Gatea",
#                                                     "Promo mi vasito \ny mi platito",
#                                                     "Mamá")),
#                                      ordered = TRUE)



ggplot(mead_feb17_ppa_clicks, aes(x=factor(`Anuncio`), y=measurement, label=comma(round(measurement, 0)))) +
  facet_grid(. ~ condition) +
  geom_col(fill = "#E5B07C") +
  theme_ipsum(grid="Y") +
  labs(title="Enfabebé - PPAs - Clicks", x = "", y = "") +
  theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,angle = 15,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
        plot.title = element_text(face = "bold", vjust=2, size = 22), 
        legend.title = element_text(colour="grey40", size=8, face="bold"),
        legend.text = element_text(colour="grey10", size=12, face="bold"),
        strip.text.x = element_text(size = 18,
                                    hjust = 0.1, vjust = 0.5)) +
  scale_y_continuous(labels = comma, limits = c(0, 80000)) +
  geom_text(hjust=+0.5, vjust=-0.5, size = 8) 
   



ggsave("ppas-clicks.jpg", width = 12)

##### CTR ############



mead_feb17_ppa_ctr <- mead_feb17_ppa %>%
                          select(-c(Campana,`Conjunto de anuncios`)) %>%
                          group_by(Anuncio, condition) %>%
                          filter(grepl("CTR",condition)) %>%
                          summarise(measurement = sum(measurement))





unique(mead_feb17_ppa_ctr$`Anuncio`)

# 
# 
# mead_feb17_ppl_ctr$`Anuncio` <- factor(mead_feb17_ppl_ctr$`Anuncio`,
#                                                   levels = rev(c("Promo vasito",
#                                                                  "Saluda a tu pediatra",
#                                                                  "Promo Vasito",
#                                                                  "Cuentos",
#                                                                  "Promoción escolar")),
#                                                   ordered = TRUE)


unique(mead_feb17_ppa_clics$condition)





ggplot(mead_feb17_ppa_ctr, aes(x=factor(`Anuncio`), y=measurement, label=percent(round(measurement, 4)))) +
  facet_grid( ~ condition) +
  geom_point(color = "#626262", size = 10) +
  geom_point(color = "#E54994", size = 8) +
  theme_ipsum(grid="Y") +
  labs(title="Enfabebé - PPAs - CTR", x = "", y = "") +
  theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
        plot.title = element_text(face = "bold", vjust=2, size = 22), 
        legend.title = element_text(colour="grey40", size=8, face="bold"),
        legend.text = element_text(colour="grey10", size=12, face="bold"),
        strip.text.x = element_text(size = 22,
                                    hjust = 0.1, vjust = 0.5)) +
  scale_y_continuous(labels = percent, limits = c(0, 0.08)) +
  geom_text(hjust=0.5, vjust=-1.2, size = 8) +
  



ggsave("ppas-ctr.jpg", width = 12)

#### ANTIGÜO CTR ####



# Alcance & Frecuencia




mead_feb17_ppa_alcance <- mead_feb17_ppa %>%
                        filter(grepl("Alcance",condition))





mead_feb17_ppa_alcance$Anuncio <- factor(mead_feb17_ppa_alcance$Anuncio,
                                        levels = rev(c("Gatea", "Mamá",
                                                       "Promo mi vasito \ny mi platito")),
                                        ordered = TRUE)

#Alcance



ggplot(mead_feb17_ppa_alcance, aes(x=factor(Anuncio), y=measurement, label=comma(round(measurement, 4)))) +
  #facet_grid( ~ condition) +
  geom_col(fill = "#FF9933") +
  theme_ipsum(grid="Y") +
  labs(title="Enfabebé - PPAs - Alcance", x = "", y = "") +
  theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
        plot.title = element_text(face = "bold", vjust=2, size = 22), 
        legend.title = element_text(colour="grey40", size=8, face="bold"),
        legend.text = element_text(colour="grey10", size=12, face="bold"),
        strip.text.x = element_text(size = 22,
                                    hjust = 0.5, vjust = 0.5)) +
  scale_y_continuous(labels = comma, limits = c(0, 3000000)) +
  geom_text(hjust=0.5, vjust=-1.2, size = 8) 
   




ggsave("ppas-alcance.jpg", width = 8)



#Frecuencia





mead_feb17_ppa_freq <- mead_feb17_ppa %>%
                     filter(grepl("Frecuencia",condition))









ggplot(mead_feb17_ppa_freq, aes(x=factor(Anuncio), y=measurement, label=round(measurement, 2))) +
  #facet_grid( ~ condition) +
  geom_point(color = "#626262", size = 10) +
  geom_point(color = "#268FE0", size = 8) +
  theme_ipsum(grid="Y") +
  labs(title="Enfabebé - PPAs - Frecuencia", x = "", y = "") +
  theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
        plot.title = element_text(face = "bold", vjust=2, size = 22), 
        legend.title = element_text(colour="grey40", size=8, face="bold"),
        legend.text = element_text(colour="grey10", size=12, face="bold"),
        strip.text.x = element_text(size = 22,
                                    hjust = 0.1, vjust = 0.5)) +
  scale_y_continuous(labels = comma, limits = c(0, 5)) +
  geom_text(hjust=0.5, vjust=-1.2, size = 8) 
   



ggsave("ppas-frecuencia.jpg", width = 8)


#Engagement



# 
# 
# mead_feb17_ppa_eng <- mead_feb17_ppa %>%
#                     filter(grepl("Enga",condition))
# 
# 
# 
# 
# 
# 
# 
# ggplot(mead_feb17_ppa_eng, aes(x=factor(condition), y=measurement, label=percent(round(measurement, 4)))) +
#         #facet_grid( ~ condition) +
#         geom_point(color = "#626262", size = 10) +
#         geom_point(color = "#C094C1", size = 8) +
#         theme_light() +
#         labs(title="Enfabebé - PPLs - Engagement", x = "") +
#         theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
#               axis.text.y = element_text(colour="grey20",size=18,hjust=1,vjust=0,face="plain"),  
#               axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
#               axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
#               plot.title = element_text(face = "bold", vjust=2, size = 22), 
#               legend.title = element_text(colour="grey40", size=8, face="bold"),
#               legend.text = element_text(colour="grey10", size=12, face="bold"),
#               strip.text.x = element_text(size = 22,
#                                           hjust = 0.5, vjust = 0.5)) +
#         scale_y_continuous(labels = percent, limits = c(0, 0.08)) +
#         geom_text(hjust=0.5, vjust=-1.2, size = 8)
#       
#       
#       
# 
# ggsave("ppls-engagement.jpg", width = 8)
# 
# 
# 
# 
# 
# 
# #Clics en el enlace vs otros clicks
# 
# 
# 
# mead_feb17_ppa_clics_in_off <- mead_feb17_ppa %>%
#                               filter(condition == "ClicsEnlace"|condition == "ClicsOtros")
# 
# 
# 
# 
# 
# unique(mead_feb17_ppa_clics_in_off$condition)
# 
# 
# 
# mead_feb17_ppa_clics_in_off$condition <- factor(mead_feb17_ppa_clics_in_off$condition,
#                                               levels = rev(c("ClicsEnlace",
#                                                          "ClicsOtros")),
#                                               ordered = TRUE)
# 
# 
# 
# 
# ggplot(mead_feb17_ppa_clics_in_off, aes(x = condition, y = measurement, fill = condition, label = percent(measurement))) +
#   geom_col() +
#   theme_light() +
#   #facet_grid( ~ `Anuncio`) +
#   labs(title="Enfabebé - PPLs\nClics otros vs Clics en el enlace", x = "") +
#   theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
#         axis.text.y = element_text(colour="grey20",size=18,hjust=1,vjust=0,face="plain"),  
#         axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
#         axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
#         plot.title = element_text(face = "bold", vjust=2, size = 22), 
#         legend.title = element_text(colour="grey40", size=8, face="bold"),
#         legend.text = element_text(colour="grey10", size=12, face="bold"),
#         legend.position = "none",
#         strip.text.x = element_text(size = 22,
#                                     hjust = 0.5, vjust = 0.5)) +
#   scale_fill_manual(values = c("#CC0000","#0066CC")) +
#   scale_y_continuous(labels = percent, limits = c(0, 1)) +
#   geom_text(hjust=+0.5, vjust=-1, size = 8)
# 
# 
# 
# 
# ggsave("ppls-clicsenlace-clicsotros.jpg")
