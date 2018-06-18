##############################################################
######## Curso Vizualización de Datos - EMAS 2018 ############
########  Práctica 3 - Introducción a ggplot2     ############
##############################################################

#1. Cargar (e instala) los paquetes que vas a utilizar

#install.packages("sjmisc")
#install.packages("tidyverse")

library(sjmisc)
library(tidyverse)

#Limpia espacio y establece directorio de trabajo
rm(list=ls())
setwd("***")
getwd()
d <- foreign::read.spss("cis3145t.sav", to.data.frame = T)

###########################################################
###########################################################
#Gráfico 1: Comparar relación entre confpub y confpriv. Realizar el análisis por perfil según votó o no en las últimas elecciones.

#G1.a) Preparar variables

#confpub
d <- mutate(d, confpub=ifelse(confparl>97 | confpart>97 | confjudic>97, NA, confparl + confpart + confjudic ))
descr(d$confpub)
hist(d$confpub)

#confpriv
d <- mutate(d, confpriv=ifelse(confong>97 | confbanco>97 | confmedia>97, NA, confbanco + confong + confmedia ))
descr(d$confpriv)
hist(d$confpriv)

#voto16
d$urnas16r <- recode(d$voto16, "NoVoto"="No voto", "NC"="NC", .default="Voto") 
frq(d$urnas16r)

#G1.b) set-up el plot (data + aes)
g1 <- ggplot(d, aes(x=confpub, y=confpriv, col=urnas16r))
g1

#G1.c) añadir geom
g1 <- g1 + geom_point()
g1

#G1.d) set-up el plot (data + aes) subsetting data para excluir NA
g1b <- ggplot(filter(d, !is.na(urnas16r), urnas16r != "NC"), aes(x=confpub, y=confpriv, col=urnas16r))
g1b 

#G1.e) añadir geom (scatter)
g1b <- g1b + geom_point(position=position_jitter(.1), alpha=.3, shape=1)
g1b

#G1.f) añadir geom (línea de ajuste)
g1b <- g1b + geom_smooth(method="lm")
g1b

#G1.g) añadir geom (línea de ajuste general)
g1b <- g1b + geom_smooth( aes(col=factor(1)), method="lm")
g1b

#G1.h) modificar colores escala
g1b <- g1b + scale_colour_manual(values=c("red", "black", "skyblue"), labels=c("Total", "No votó", "Votó"))
g1b

#G1.i) modificar títulos ejes, título y título leyenda
g1b <- g1b + labs(x= "Confianza instituciones privadas", y="Confianza en instituciones públicas", col="Elecciones 2016", title="G1. Confianza instituciones públicas y privadas" )
g1b

###########################################################
###########################################################

#Gráfico 2: Crear un gráfico de barras de recuerdo de voto 2015 para representar diferencias en voto válido

#G2. a) Preparar variables
d$voto15r <- recode(d$voto15, "PP"="PP", "PSOE"="PSOE",
                    "Cs"="Cs", "ECPodem"="Podemos", "Podemos"="Podemos",
                    "Compromis"="Podemos", "En Marea" = "Podemos",
                    "NoVoto"="No voto", "NC"="NC", "NR"="NC", .default="Otros")
frq(d$voto15r)

d2 <- filter(d, voto15r != "No voto", voto15r != "NC")
frq(d2$voto15r)

#G2.b) Set-up ggplot
g2 <- ggplot(d2, aes(x=voto15r))
g2

#G2.c) Añadir geom: barras
g2 <- g2 + geom_bar(aes(y=..count../sum(..count..)*100), fill="gray", width = .75) #porcentajes
#g2 <- g2 + geom_bar(fill="gray", width = .75)
g2

#G2.d) Modificar escala eje y
g2 <- g2 + scale_y_continuous(limits=c(0, 40), breaks = c(0,10,20, 30, 40)) 
#g2 <- g2 + scale_y_continuous(limits=c(0, 1250), breaks = c(250,500,750,1000,1250)) 
g2

#G2.e) Añadir título ejes
g2 <- g2 + labs(x="", y="", title="G2. Recuerdo de voto elecciones 2015 (%)")
g2


###########################################################
###########################################################


#Gráfico 3: Comparar distribución interna de las variables confpub y confpriv

#G3.a) Set-up ggplot
g3 <- ggplot(filter(d, !is.na(confpub), !is.na(confpriv)))
g3

#G3.b) Añadir geoms: histogramas
g3 <- g3 + geom_histogram(aes(x=confpub, y=..density.., fill="skyblue"), alpha=0.2, bins=30) +
            geom_histogram(aes(x=confpriv, y=..density.., fill="red"), alpha=0.2, bins=30)
g3

#G3.c) Añadir geoms: líneas de densidad
g3 <- g3 + geom_density(aes(x=confpub, col="skyblue") ) +
      geom_density(aes(x=confpriv, col="red"))
g3

#G3.d) Modificar leyenda
g3 <- g3 + scale_fill_discrete(labels=c("Privadas", "Públicas"), name="" ) +
  scale_color_discrete(labels=c("Privadas", "Públicas"), name="")
g3

#G3.e) Añadir etiquetas
g3 <- g3 + labs(x="", y="", title="G3. Distribuciones confianza en instituciones \npúblicas y privadas")
g3


###########################################################
###########################################################


#Gráfico 4: análisis visual de la variación de confianza en instituciones públicas para los diferentes de recuerdo de voto 2016

#G4.a) Preparar variables
d$voto16r <-recode(d$voto16, "PP"="PP", "PSOE"="PSOE",
                                 "Cs"="Cs", "ECPodem"="Podemos", "Podemos"="Podemos",
                                 "Compromis"="Podemos", "En Marea" = "Podemos",
                                 "NoVoto"="No voto", "NC"="NC", "NR"="NR", .default="Otros") 
frq(d$voto16r)

d3 <- filter(d, !is.na(d$confpub),  voto16r!='NC') %>%
        select(confpub, voto16r)

#G4.b) set-up ggplot
g4 <- ggplot(d3, aes(x=voto16r, y=confpub))
g4

#G4.c) añadir geom: cajas
g4 <- g4 + geom_boxplot(fill="skyblue")
g4

#G4.d) etiquetas
g4 <- g4 + labs(x="", y="", title="G4. Distribución de confianza en instituciones\npúblicas según voto 2016")
g4

#G4.e) geom_box() resumen (Para reordenar las categorías habría que utilizar los levels del factor)
g4 <- g4 +  geom_boxplot(aes(x=factor("1")), fill="brown") +
  scale_x_discrete(labels=c("España", "PP", "PSOE", "Podemos", "Cs", "Otros", "No votó"))
g4

###########################################################
###########################################################
#Gráfico 5: Crear un gráfico de medias con sus límites de confianza

#G5.a) Preparar variables
#voto16
frq(d$urnas16r)

#G5.b) Set-up ggplot
g5 <- ggplot(filter(d, !is.na(urnas16r)), aes(x = urnas16r, y =confpub))
g5

#G5.c) Añadir stat: summary(mean_cl_normal)
g5 <- g5 + stat_summary(fun.data = mean_cl_normal)
g5

#G5.d) Límites eje x (0 a 30)
g5 <- g5 + coord_cartesian(ylim = c(0, 15))
g5

#G5.e) Etiquetas
g5 <- g5 + labs(x="", y="Confianza en instituciones públicas", title="G2. Media Confianza instituciones públicas \npor comportamiento electoral en 2016" )
g5


