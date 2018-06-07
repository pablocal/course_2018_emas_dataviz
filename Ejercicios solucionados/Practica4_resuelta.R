##############################################################
######## Curso Vizualización de Datos - EMAS 2018 ############
########        Práctica 4 - Más ggplot2          ############
##############################################################

#1. Cargar (e instala) los paquetes que vas a utilizar

#install.packages("foreign")
#install.packages("sjmisc")
#install.packages("tidyverse")
#install.packages("ggthemes")
#install.packages("grid")

library(sjmisc)
library(tidyverse)
library(ggthemes)
library(gridExtra)

#Limpia espacio y establece directorio de trabajo
rm(list=ls())
setwd("****")
getwd()
d <- foreign::read.spss("cis3145t.sav", to.data.frame = T)


###########################################################
###########################################################
#Gráfico 1: Comparar distribución interna de las variables confpub y confpriv

#G1.0) PReparar variables

#confpub y confpriv
d <- mutate(d, confpub=ifelse(confparl>97 | confpart>97 | confjudic>97, NA, confparl + confpart + confjudic ),
            confpriv=ifelse(confong>97 | confbanco>97 | confmedia>97, NA, confbanco + confong + confmedia))
descr(d$confpub)
hist(d$confpub)
descr(d$confpriv)
hist(d$confpriv)

#voto16
d$urnas16r <- recode(d$voto16, "NoVoto"="No voto", "NC"="NC", .default="Voto") 
frq(d$urnas16r)


#G1.a) Set-up ggplot
g1 <- ggplot(d, aes(x = edad, y = confpub))
g1 

#G1.b) Añadir geoms: barras
g1 <- g1 + geom_point(position="jitter", alpha=.3, shape=1)
g1

#G1.c) Añadir geoms: líneas de densidad
g1 <- g1 + geom_smooth(method="lm")
g1

#G1.d) Modificar leyenda
g1 <- g1 + facet_wrap( ~ esta)
g1

#G1.e) Añadir etiquetas
g1 <- g1 + labs(x="Edad", y="Confianza en instituciones públicas", title="G3. Relación entre edad y confianza en instituciones públicas \npor clase social" )
g1

###########################################################
###########################################################
#Gráfico 2: Comparar clase social por comportamiento electoral

d2 <- filter(d, !is.na(urnas16r))

#G2.a) Set-up ggplot
g2 <- ggplot(d2, aes(x = esta, fill=urnas16r))
g2 

#G2.b) Añadir geoms: barras
g2a <- g2 + geom_bar() +
  scale_x_discrete(labels = c("Alta", "Viejas Med", "Nuevas Med.", "Obr. cual", "Obr. no cual", "NC")) +
  labs(x="", y="", fill="")
g2a

#G2.c) Añadir geoms: barras - position="stack"
g2b <- g2 + geom_bar(position="stack") +
          scale_x_discrete(labels = c("Alta", "Viejas Med", "Nuevas Med.", "Obr. cual", "Obr. no cual", "NC")) +
          labs(x="", y="", fill="")
g2b

#G2.d) Añadir geoms: barras - position="dodge"
g2c <- g2 + geom_bar(position="dodge") +
  scale_x_discrete(labels = c("Alta", "Viejas Med", "Nuevas Med.", "Obr. cual", "Obr. no cual", "NC")) +
  labs(x="", y="", fill="")
g2c

#G2.d) Añadir geoms: barras - position="dodge"
g2d <- g2 + geom_bar(position="fill") +
  scale_x_discrete(labels = c("Alta", "Viejas Med", "Nuevas Med.", "Obr. cual", "Obr. no cual", "NC")) +
  labs(x="", y="", fill="")
g2d

#G2.e) Añadir etiquetas
g2comb <- grid.arrange(g2a, g2b, g2c, g2d, ncol=2, top=textGrob("G4. Voto en 2016 por estatus social "))
g2comb

###########################################################
###########################################################
#Gráfico 3: Comparar relación entre confpub y confpriv. Realizar el análisis por perfil según votó o no en las últimas elecciones.

#G3.a) set-up el plot (data + aes) subsetting data para excluir NA
g3 <- ggplot(filter(d, urnas16r != "NC"), aes(x=confpub, y=confpriv, col=urnas16r))
g3 

#G3.b) añadir geom (scatter)
g3 <- g3 + geom_point(position=position_jitter(.1), alpha=.3, shape=1)
g3

#G3.c) añadir stat (línea de ajuste)
g3 <- g3 + stat_smooth(method="lm")
g3

#G3.d) añadir stat (línea de ajuste para todos)
g3 <- g3 + stat_smooth(aes(col=factor(1)), method="lm")
g3


#G3.e) modificar colores escala
g3 <- g3 + scale_colour_manual(values=c("red", "black", "skyblue"), labels=c("Total", "No votó", "Votó"))
g3

#G3.f) modificar títulos ejes, título y título leyenda
g3 <- g3 + labs(x= "Confianza instituciones privadas", y="Confianza en instituciones públicas", col="Elecciones 2016", title="G1. Confianza instituciones públicas y privadas" )
g3


#G3.g) incluir tema
g3
g3 <- g3 +theme(legend.position = "bottom",
                  legend.title = element_blank(),
                  panel.grid = element_blank(),
                  axis.text = element_text(colour = "gray"),
                  axis.title = element_text(colour = "gray"),
                  axis.line = element_line(colour="gray"),
                  axis.ticks = element_line(colour = "gray"),
                  panel.background = element_rect(fill="white"),
                  plot.title = element_text(color="gray31"))

g3

###########################################################
###########################################################
#Gráfico 4: Comparar distribución interna de las variables confpub y confpriv

#G4.a) Set-up ggplot
g4 <- ggplot(filter(d, !is.na(confpub), !is.na(confpriv)))
g4

#G4.b) Añadir geoms: histogramas
g4 <- g4 + geom_histogram(aes(x=confpub, y=..density.., fill="skyblue"), alpha=0.2, bins=30) +
  geom_histogram(aes(x=confpriv, y=..density.., fill="red"), alpha=0.2, bins=30)
g4

#G4.c) Añadir geoms: líneas de densidad
g4 <- g4 + geom_density(aes(x=confpub, col="skyblue") ) +
  geom_density(aes(x=confpriv, col="red"))
g4

#G4.d) Modificar leyenda
g4 <- g4 + scale_fill_discrete(labels=c("Privadas", "Públicas"), name="" ) +
  scale_color_discrete(labels=c("Privadas", "Públicas"), name="")
g4

#G4.e) Añadir etiquetas
g4 <- g4 + labs(x="", y="", title="G3. Distribuciones confianza en instituciones \npúblicas y privadas")
g4

#G4.f) Probar different themes
g4 <- g4 + theme_tufte()

g4






