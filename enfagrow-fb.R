setwd("D:\\Starcom\\mead\\2017\\feb")


library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)


mead_feb17 <- read_excel("2017-feb-mead.xlsx", sheet = 1)

colnames(mead_feb17)[2] <- "Campana"
colnames(mead_feb17)[3] <- "Conjunto de anuncios"
colnames(mead_feb17)[4] <- "Anuncio"


mead_feb17_resumen <- mead_feb17 %>%
                    group_by(Campana,`Conjunto de anuncios`,`Anuncio`) %>%
                    summarise(Resultados = sum(Resultados),
                              Alcance = sum(Alcance),
                              `Costo por resultados` = sum(`Costo por resultados`),
                              `Importe gastado (USD)` = sum(`Importe gastado (USD)`),
                              `Clics (todos)` = sum(`Clics (todos)`))


mead_feb17$Resultados <- as.numeric(mead_feb17$Resultados)
mead_feb17$Alcance <- as.numeric(mead_feb17$Alcance)
mead_feb17$`Costo por resultados` <- as.numeric(mead_feb17$`Costo por resultados`)
mead_feb17$`Importe gastado (USD)` <- as.numeric(mead_feb17$`Importe gastado (USD)`)
mead_feb17$`Puntuación de relevancia` <- as.numeric(mead_feb17$`Puntuación de relevancia`)
mead_feb17$`Clics (todos)` <- as.numeric(mead_feb17$`Clics (todos)`)
mead_feb17$`Clics en el enlace` <- as.numeric(mead_feb17$`Clics en el enlace`)
mead_feb17$Impresiones <- as.numeric(mead_feb17$Impresiones)
mead_feb17$Frecuencia <- as.numeric(mead_feb17$Frecuencia)
mead_feb17$`Veces que se compartió la publicación` <- as.numeric(mead_feb17$`Veces que se compartió la publicación`)
mead_feb17$`Reacciones de la publicación` <- as.numeric(mead_feb17$`Reacciones de la publicación`)




mead_feb17 <- mead_feb17 %>%
            mutate(CTR = `Clics (todos)`/Impresiones,
                   Frecuencia = Frecuencia/100,
                   Engagement = (`Veces que se compartió la publicación` + `Comentarios de la publicación` +
                                   `Reacciones de la publicación`) / Impresiones,
                   ClicsEnlace = `Clics en el enlace`/`Clics (todos)`,
                   ClicsOtros = (`Clics (todos)` - `Clics en el enlace`)/`Clics (todos)`)
                   
            



mead_feb17$`Anuncio` <- gsub("(^PP[A-Z].*- |Carrusel.*- )", "", mead_feb17$`Anuncio`)






mead_feb17_long <- gather(mead_feb17, condition, measurement, Resultados:ClicsOtros, factor_key=FALSE)




mead_feb17_long <- mead_feb17_long %>%
                 filter(complete.cases(mead_feb17_long))





mead_feb17_ppl <- mead_feb17_long %>%
                filter(grepl("PPL", `Conjunto de anuncios`))




mead_feb17_carrusel <- mead_feb17_long %>%
                     filter(grepl("Carrusel", `Conjunto de anuncios`))



#################
### Gráficos ####
#################








#PPAs ordenando anuncios como factores


mead_feb17_ppl$`Anuncio` <- factor(mead_feb17_ppl$`Anuncio`, levels = c("Época familiar", "Tarjeta navideña"),
                                          ordered = TRUE)








### PPAs

#Impresiones




mead_feb17_ppl_imp <- mead_feb17_ppl %>%
                    filter(grepl("Impres",condition))


mead_feb17_ppl_imp$measurement <- as.numeric(mead_feb17_ppl_imp$measurement)


str(mead_feb17_ppl_imp)

unique(mead_feb17_ppa_imp$`Anuncio`)



mead_feb17_ppa_imp$`Anuncio` <- factor(mead_feb17_ppa_imp$`Anuncio`,
                                                levels = rev(c("Promo vasito",
                                                           "Saluda a tu pediatra",
                                                           "Promo Vasito",
                                                           "Cuentos",
                                                           "Promoción escolar")),
                                                ordered = TRUE)




mead_feb17_ppa_imp$condition <- factor(mead_feb17_ppa_imp$condition,
                                                levels = rev(c("Impresiones",
                                                               "Impresiones Planeadas")),
                                                ordered = TRUE)



ggplot(mead_feb17_ppl_imp, aes(x=factor(`Anuncio`), y=measurement, label=comma(round(measurement, 0)))) +
  facet_grid(. ~ condition) +
  geom_col(fill = "#20398C") +
  theme_light() +
  labs(title="Enfabebé - PPLs - Impresiones", x = "") +
  theme(axis.text.x = element_text(colour="grey20",size=12,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
        plot.title = element_text(face = "bold", vjust=2, size = 22), 
        legend.title = element_text(colour="grey40", size=8, face="bold"),
        legend.text = element_text(colour="grey10", size=12, face="bold"),
        strip.text.x = element_text(size = 14,
                                    hjust = 0.5, vjust = 0.5)) +
  scale_y_continuous(labels = comma, limits = c(0, 14000000)) +
  geom_text(hjust=-0.4, vjust=0.5, size = 8) +
  coord_flip()




ggsave("ppas-imp.jpg", width = 12)


#Clicks





mead_feb17_ppa_clics <- mead_feb17_ppa %>%
                      filter(grepl("Clics",condition)) %>%
                      filter(condition == "Clics (todos)" | condition == "Clics Planeados")



unique(mead_feb17_ppa_imp$`Anuncio`)



mead_feb17_ppa_clics$`Anuncio` <- factor(mead_feb17_ppa_clics$`Anuncio`,
                                                levels = rev(c("Promo vasito",
                                                               "Saluda a tu pediatra",
                                                               "Promo Vasito",
                                                               "Cuentos",
                                                               "Promoción escolar")),
                                                ordered = TRUE)


unique(mead_feb17_ppa_clics$condition)

mead_feb17_ppa_clics$condition <- factor(mead_feb17_ppa_clics$condition,
                                     levels = rev(c("Clics (todos)",
                                                    "Clics Planeados")),
                                     ordered = TRUE)



ggplot(mead_feb17_ppa_clics, aes(x=factor(`Anuncio`), y=measurement, label=comma(round(measurement, 0)))) +
  facet_grid( ~ condition) +
  geom_col(fill = "#E5B07C") +
  theme_light() +
  labs(title="Enfabebé - PPAs - Clics vs Clics Planeados", x = "") +
  theme(axis.text.x = element_text(colour="grey20",size=12,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
        plot.title = element_text(face = "bold", vjust=2, size = 22), 
        legend.title = element_text(colour="grey40", size=8, face="bold"),
        legend.text = element_text(colour="grey10", size=12, face="bold"),
        strip.text.x = element_text(size = 18,
                                    hjust = 0.5, vjust = 0.5)) +
  scale_y_continuous(labels = comma, limits = c(0, 60000)) +
  geom_text(hjust=-0.2, vjust=0.5, size = 8) +
  coord_flip()





ggsave("ppas-clics.jpg", width = 12)


  
#CTR






mead_feb17_ppa_ctr <- mead_feb17_ppa %>%
                      filter(grepl("CTR",condition))


unique(mead_feb17_ppa_ctr$`Anuncio`)



mead_feb17_ppa_ctr$`Anuncio` <- factor(mead_feb17_ppa_ctr$`Anuncio`,
                                                  levels = rev(c("Promo vasito",
                                                                 "Saluda a tu pediatra",
                                                                 "Promo Vasito",
                                                                 "Cuentos",
                                                                 "Promoción escolar")),
                                                  ordered = TRUE)


unique(mead_feb17_ppa_clics$condition)

mead_feb17_ppa_clics$condition <- factor(mead_feb17_ppa_clics$condition,
                                       levels = rev(c("Clics (todos)",
                                                      "Clics Planeados")),
                                       ordered = TRUE)



ggplot(mead_feb17_ppa_ctr, aes(x=factor(`Anuncio`), y=measurement, label=percent(round(measurement, 4)))) +
  facet_grid( ~ condition) +
  geom_point(color = "#626262", size = 10) +
  geom_point(color = "#E54994", size = 8) +
  theme_light() +
  labs(title="Enfabebé - PPAs - CTRs", x = "") +
  theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
        plot.title = element_text(face = "bold", vjust=2, size = 22), 
        legend.title = element_text(colour="grey40", size=8, face="bold"),
        legend.text = element_text(colour="grey10", size=12, face="bold"),
        strip.text.x = element_text(size = 22,
                                    hjust = 0.5, vjust = 0.5)) +
  scale_y_continuous(labels = percent, limits = c(0, 0.08)) +
  geom_text(hjust=0.5, vjust=-1.2, size = 8)





ggsave("ppas-ctrs.jpg", width = 14)

# Alcance & Frecuencia




mead_feb17_ppa_alcance <- mead_feb17_ppa %>%
                        filter(grepl("Alcance",condition))




mead_feb17_ppa_alcance$`Anuncio` <- factor(mead_feb17_ppa_alcance$`Anuncio`,
                                                levels = c("Promo vasito",
                                                               "Saluda a tu pediatra",
                                                               "Promo Vasito",
                                                               "Cuentos",
                                                               "Promoción escolar"),
                                                ordered = TRUE)




#Alcance



ggplot(mead_feb17_ppa_alcance, aes(x=factor(`Anuncio`), y=measurement, label=comma(round(measurement, 4)))) +
  facet_grid( ~ condition) +
  geom_col(fill = "#FF9933") +
  theme_light() +
  labs(title="Enfabebé - PPAs - Alcance", x = "") +
  theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
        plot.title = element_text(face = "bold", vjust=2, size = 22), 
        legend.title = element_text(colour="grey40", size=8, face="bold"),
        legend.text = element_text(colour="grey10", size=12, face="bold"),
        strip.text.x = element_text(size = 22,
                                    hjust = 0.5, vjust = 0.5)) +
  scale_y_continuous(labels = comma, limits = c(0, 1600000)) +
  geom_text(hjust=0.5, vjust=-1.2, size = 8)



ggsave("ppas-alcance.jpg", width = 14)



#Frecuencia





mead_feb17_ppa_freq <- mead_feb17_ppa %>%
                        filter(grepl("Frecuencia",condition))





mead_feb17_ppa_freq$`Anuncio` <- factor(mead_feb17_ppa_freq$`Anuncio`,
                                                    levels = c("Promo vasito",
                                                               "Saluda a tu pediatra",
                                                               "Promo Vasito",
                                                               "Cuentos",
                                                               "Promoción escolar"),
                                                    ordered = TRUE)







ggplot(mead_feb17_ppa_freq, aes(x=factor(`Anuncio`), y=measurement, label=percent(round(measurement, 4)))) +
  facet_grid( ~ condition) +
  geom_point(color = "#626262", size = 10) +
  geom_point(color = "#268FE0", size = 8) +
  theme_light() +
  labs(title="Enfabebé - PPAs - Frecuencia", x = "") +
  theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
        plot.title = element_text(face = "bold", vjust=2, size = 22), 
        legend.title = element_text(colour="grey40", size=8, face="bold"),
        legend.text = element_text(colour="grey10", size=12, face="bold"),
        strip.text.x = element_text(size = 22,
                                    hjust = 0.5, vjust = 0.5)) +
  scale_y_continuous(labels = percent, limits = c(0, 0.08)) +
  geom_text(hjust=0.5, vjust=-1.2, size = 8)



ggsave("ppas-frecuencia.jpg", width = 14)



#Engagement





mead_feb17_ppa_eng <- mead_feb17_ppa %>%
                    filter(grepl("Enga",condition))





mead_feb17_ppa_eng$`Anuncio` <- factor(mead_feb17_ppa_eng$`Anuncio`,
                                                 levels = c("Promo vasito",
                                                            "Saluda a tu pediatra",
                                                            "Promo Vasito",
                                                            "Cuentos",
                                                            "Promoción escolar"),
                                                 ordered = TRUE)







ggplot(mead_feb17_ppa_eng, aes(x=factor(`Anuncio`), y=measurement, label=percent(round(measurement, 4)))) +
  facet_grid( ~ condition) +
  geom_point(color = "#626262", size = 10) +
  geom_point(color = "#C094C1", size = 8) +
  theme_light() +
  labs(title="Enfabebé - PPAs - Engagement", x = "") +
  theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
        plot.title = element_text(face = "bold", vjust=2, size = 22), 
        legend.title = element_text(colour="grey40", size=8, face="bold"),
        legend.text = element_text(colour="grey10", size=12, face="bold"),
        strip.text.x = element_text(size = 22,
                                    hjust = 0.5, vjust = 0.5)) +
  scale_y_continuous(labels = percent, limits = c(0, 0.08)) +
  geom_text(hjust=0.5, vjust=-1.2, size = 8)



ggsave("ppas-engagement.jpg", width = 14)

########################################
########################################

########################################
############# PPLs #####################
########################################


#Impresiones




#Impresiones




mead_feb17_ppl_imp <- mead_feb17_ppl %>%
                    filter(grepl("Impres",condition))

unique(mead_feb17_ppl_imp$`Anuncio`)








mead_feb17_ppl_imp$condition <- factor(mead_feb17_ppl_imp$condition,
                                     levels = rev(c("Impresiones",
                                                    "Impresiones Planeadas")),
                                     ordered = TRUE)



ggplot(mead_feb17_ppl_imp, aes(x=factor(`Anuncio`), y=measurement, label=comma(round(measurement, 0)))) +
  facet_grid(. ~ condition) +
  geom_col(fill = "#20398C") +
  theme_light() +
  labs(title="Enfabebé - PPLs - Impresiones", x = "") +
  theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
        plot.title = element_text(face = "bold", vjust=2, size = 22), 
        legend.title = element_text(colour="grey40", size=8, face="bold"),
        legend.text = element_text(colour="grey10", size=12, face="bold"),
        strip.text.x = element_text(size = 18,
                                    hjust = 0.5, vjust = 0.5)) +
  scale_y_continuous(labels = comma, limits = c(0, 800000)) +
  geom_text(hjust=+0.5, vjust=-0.5, size = 8) 



ggsave("ppls-imp.jpg")



#Clicks





mead_feb17_ppl_clics <- mead_feb17_ppl %>%
                      filter(grepl("Clics",condition)) %>%
                      filter(condition == "Clics (todos)" | condition == "Clics Planeados")








mead_feb17_ppl_clics$condition <- factor(mead_feb17_ppl_clics$condition,
                                       levels = rev(c("Clics (todos)",
                                                      "Clics Planeados")),
                                       ordered = TRUE)



ggplot(mead_feb17_ppl_clics, aes(x=factor(`Anuncio`), y=measurement, label=comma(round(measurement, 0)))) +
  facet_grid( ~ condition) +
  geom_col(fill = "#E5B07C") +
  theme_light() +
  labs(title="Enfabebé - PPLs - Clics vs Clics Planeados", x = "") +
  theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
        plot.title = element_text(face = "bold", vjust=2, size = 22), 
        legend.title = element_text(colour="grey40", size=8, face="bold"),
        legend.text = element_text(colour="grey10", size=12, face="bold"),
        strip.text.x = element_text(size = 18,
                                    hjust = 0.5, vjust = 0.5)) +
  scale_y_continuous(labels = comma, limits = c(0, 60000)) +
  geom_text(hjust=+0.5, vjust=-0.5, size = 8)





ggsave("ppls-clics.jpg")



#CTR






mead_feb17_ppl_ctr <- mead_feb17_ppl %>%
                    filter(grepl("CTR",condition))



unique(mead_feb17_ppl_ctr$`Anuncio`)











ggplot(mead_feb17_ppl_ctr, aes(x=factor(condition), y=measurement, label=percent(round(measurement, 4)))) +
  #facet_grid( ~ condition) +
  geom_point(color = "#626262", size = 10) +
  geom_point(color = "#E54994", size = 8) +
  theme_light() +
  labs(title="Enfabebé - PPLs - CTRs", x = "") +
  theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
        plot.title = element_text(face = "bold", vjust=2, size = 22), 
        legend.title = element_text(colour="grey40", size=8, face="bold"),
        legend.text = element_text(colour="grey10", size=12, face="bold"),
        strip.text.x = element_text(size = 22,
                                    hjust = 0.5, vjust = 0.5)) +
  scale_y_continuous(labels = percent, limits = c(0, 0.08)) +
  geom_text(hjust=0.5, vjust=-1.2, size = 8)




ggsave("ppls-ctr.jpg", width = 8)



# Alcance & Frecuencia




mead_feb17_ppl_alcance <- mead_feb17_ppl %>%
                        filter(grepl("Alcance",condition))







#Alcance



ggplot(mead_feb17_ppl_alcance, aes(x=factor(condition), y=measurement, label=comma(round(measurement, 4)))) +
  #facet_grid( ~ condition) +
  geom_col(fill = "#FF9933") +
  theme_light() +
  labs(title="Enfabebé - PPLs - Alcance", x = "") +
  theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
        plot.title = element_text(face = "bold", vjust=2, size = 22), 
        legend.title = element_text(colour="grey40", size=8, face="bold"),
        legend.text = element_text(colour="grey10", size=12, face="bold"),
        strip.text.x = element_text(size = 22,
                                    hjust = 0.5, vjust = 0.5)) +
  scale_y_continuous(labels = comma, limits = c(0, 1000000)) +
  geom_text(hjust=0.5, vjust=-1.2, size = 8)



ggsave("ppls-alcance.jpg", width = 8)



#Frecuencia





mead_feb17_ppl_freq <- mead_feb17_ppl %>%
                     filter(grepl("Frecuencia",condition))









ggplot(mead_feb17_ppl_freq, aes(x=factor(condition), y=measurement, label=percent(round(measurement, 4)))) +
  #facet_grid( ~ condition) +
  geom_point(color = "#626262", size = 10) +
  geom_point(color = "#268FE0", size = 8) +
  theme_light() +
  labs(title="Enfabebé - PPLs - Frecuencia", x = "") +
  theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
        plot.title = element_text(face = "bold", vjust=2, size = 22), 
        legend.title = element_text(colour="grey40", size=8, face="bold"),
        legend.text = element_text(colour="grey10", size=12, face="bold"),
        strip.text.x = element_text(size = 22,
                                    hjust = 0.5, vjust = 0.5)) +
  scale_y_continuous(labels = percent, limits = c(0, 0.08)) +
  geom_text(hjust=0.5, vjust=-1.2, size = 8)



ggsave("ppls-frecuencia.jpg", width = 8)


#Engagement





mead_feb17_ppl_eng <- mead_feb17_ppl %>%
                    filter(grepl("Enga",condition))







ggplot(mead_feb17_ppl_eng, aes(x=factor(condition), y=measurement, label=percent(round(measurement, 4)))) +
        #facet_grid( ~ condition) +
        geom_point(color = "#626262", size = 10) +
        geom_point(color = "#C094C1", size = 8) +
        theme_light() +
        labs(title="Enfabebé - PPLs - Engagement", x = "") +
        theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
              axis.text.y = element_text(colour="grey20",size=18,hjust=1,vjust=0,face="plain"),  
              axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
              axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
              plot.title = element_text(face = "bold", vjust=2, size = 22), 
              legend.title = element_text(colour="grey40", size=8, face="bold"),
              legend.text = element_text(colour="grey10", size=12, face="bold"),
              strip.text.x = element_text(size = 22,
                                          hjust = 0.5, vjust = 0.5)) +
        scale_y_continuous(labels = percent, limits = c(0, 0.08)) +
        geom_text(hjust=0.5, vjust=-1.2, size = 8)
      
      
      

ggsave("ppls-engagement.jpg", width = 8)






#Clics en el enlace vs otros clicks



mead_feb17_ppl_clics_in_off <- mead_feb17_ppl %>%
                              filter(condition == "ClicsEnlace"|condition == "ClicsOtros")





unique(mead_feb17_ppl_clics_in_off$condition)



mead_feb17_ppl_clics_in_off$condition <- factor(mead_feb17_ppl_clics_in_off$condition,
                                              levels = rev(c("ClicsEnlace",
                                                         "ClicsOtros")),
                                              ordered = TRUE)




ggplot(mead_feb17_ppl_clics_in_off, aes(x = condition, y = measurement, fill = condition, label = percent(measurement))) +
  geom_col() +
  theme_light() +
  #facet_grid( ~ `Anuncio`) +
  labs(title="Enfabebé - PPLs\nClics otros vs Clics en el enlace", x = "") +
  theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"), 
        plot.title = element_text(face = "bold", vjust=2, size = 22), 
        legend.title = element_text(colour="grey40", size=8, face="bold"),
        legend.text = element_text(colour="grey10", size=12, face="bold"),
        legend.position = "none",
        strip.text.x = element_text(size = 22,
                                    hjust = 0.5, vjust = 0.5)) +
  scale_fill_manual(values = c("#CC0000","#0066CC")) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  geom_text(hjust=+0.5, vjust=-1, size = 8)




ggsave("ppls-clicsenlace-clicsotros.jpg")
