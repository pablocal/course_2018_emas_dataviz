##############################################################
######## Curso Vizualización de Datos - EMAS 2018 ############
########     Práctica 2 - Introducción a R       ############
##############################################################

#1. Cargar (e instala si es necesario) los paquetes que vas a utilizar

#install.packages("tidyverse")
#install.packages("descr")
#install.packages("foreign")

library(tidyverse)
library(descr)

#2. Limpia espacio y establece directorio de trabajo
rm(list=ls())

setwd("C:/Users____________")
getwd()

#3. Crear vectores.
x <- c(2,4,6)
__ <- c(3,4,7) #compeltar

#4. Operar con vectores
#4a - Calcular medias
meanx <- mean(x)
________________ #completar con meany                  

#4b - Sumar vectores x e y
sumxy <- __ + y #completar
x               #imprime x
__              #imprime y    
__              #imprime sumxy

#4c - multiplica vectores x e y
_________________ #multiplica x y meanx

#4d - Operación lógica meanx y meany
________________   #operación lógica

#5. Crear un data frame con los vectores x e y mydata
_____ <- cbind(__, y) #combinar los dos vectores como columnas para crear mydata
mydata <- as.data.frame(mydata) #convertir la combinación en data frame
__________ #media de la variable X dentro del data frame mydata

#6. Crear un vector con el sexo de los niños
___ <- c("masc", "fem", ____) #crear sex

#7. Añadir vector sex a mydata creando mydata2
___________________ #crear mydata2 usando cbind()
mydata2

#8. Clase de los objetos y transformación de tipo (factor y numérico)
_______ #class vector sex
_____________ #class variable sex en mydata2

_____________ #levels variable sex en mydata2


___________ <- ____________(mydata2$sex) #pasa de factor a numérica
_________________    #comprueba la clase de la variable que acabas de modificar

_____________ <- as.factor(______________) #pasa de numérica a factor
__________________ #comprueba la transformacion class
levels(___________) #comprueba la transformacion levels

levels(mydata2$___) <- c('masc', 'fem') #recupera los niveles del factor
_________________ #comprueba la transformacion levels

#9. Abrir datos (estudio poselectoral CIS 2016) en formato SPSS y asignar a un data frame
d <- read.spss("cis3145t.sav", to.data.frame = T) #abierto usando foreign

#10. Explora los datos usando las función str()


#11. Explora las variables confparl, confpart, confjudic, voto15 y voto16


#12. Transformar datos: recodificar
frq(d$voto16)
d$voto16r <- recode(d$voto16, "PP"="PP", "PSOE"="PSOE",
                    "Cs"="Cs", "ECPodem"="Podemos", "Podemos"="Podemos",
                    "Compromis"="Podemos", "En Marea" = "Podemos",
                    "NoVoto"="No voto", "NC"="NC", "NR"="NR", .default="Otros")
frq(d$voto16r)




#13. Compilar nueva variable - crear índice confianza pública

d <- mutate(d, confpub=ifelse(d$confparl>97 | d$confpart>97 | d$confjudic>97, NA, 
                                          d$confparl + d$confpart + d$confjudic ))


#14. Seleccionar casos



#15. Seleccionar variables
d5 <- select(d, cuest, voto16r)
head(d5)

