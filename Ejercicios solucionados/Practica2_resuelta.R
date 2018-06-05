##############################################################
######## Curso Vizualización de Datos - EMAS 2017 ############
########     Práctica II - Introducción a R       ############
##############################################################

#1. Cargar (e instala si es necesario) los paquetes que vas a utilizar

#install.packages(foreign)
#install.packages(sjmisc)
#install.packages(sjPlot)
#install.packages(gmodels)
#install.packages(reshape2)
#install.packages(dplyr)

library(foreign)
library(sjmisc)
library(sjPlot)
library(gmodels)
library(reshape2)
library(dplyr)

#2. Limpia espacio y establece directorio de trabajo
rm(list=ls())

setwd("C:/Users/USUARIO/Google Drive/PhD/2. Working area/4. Projects/2017_06_Curso visualización datos/Sesion2 - IntroR/prep")
getwd()

#3. Crear vectores.
x <- c(2,4,6)
y <- c(3,4,7)

#4. Operar con vectores
#4a - Calcular medias
meanx <- mean(x)
meany <- mean(y)

#4b - Sumar vectores x e y
sumxy <- x + y

#4c - multiplica vectores x e y
multx <- x*meanx

#4d - Operación lógica meanx y meany
meanx==meany

#5. Crear un data frame con los vectores x e y mydata
mydata <- cbind(x, y) #combinar los dos vectores como columnas
mydata <- as.data.frame(mydata) #convertir la combinación en data frame
mean(mydata$x) #media de la variable X dentro del data frame mydata

#6. Crear un vector con el sexo de los niños
sex <- c("masc", "fem", "masc")

#7. Añadir vector sex a mydata creando mydata2
mydata2 <- cbind(sex, mydata)

#8. Clase de los objetos y transformación de tipo (factor y numérico)
class(sex)
class(mydata2$sex)
levels(mydata2$sex)

mydata2$sex <- as.numeric(mydata2$sex) #pasa de factor a numérica
class(mydata2$sex)

mydata2$sex <- as.factor(mydata2$sex) #pasa de numérica a factor
class(mydata2$sex)
levels(mydata2$sex)

levels(mydata2$sex) <- c('masc', 'fem') #recupera los niveles del factor
levels(mydata2$sex)

#9. Abrir datos (estudio poselectoral CIS 2016) en formato SPSS y asignar a un data frame
d <- read.spss("cis3145t.sav", to.data.frame = T) #abierto usando foreign

#10. Explora los datos usando las funciones str() y view_df()
str(d) #viatazo general a las variables y el tipo
view_df(d, show.frq = T, show.prc = T) #Vista de los códigos y las distribuciones
head(d, n=20) #listar los primeros valores de las variables
tail(d, n=20) #listar los últimos valores de las variables
summary(d) #resumen de las variables

#11. Explora las variables confparl, confpart, confjudic, confmedia, 
#confbanco, confong, voto15 y voto16
myvars <- c("confparl", "confpart", "confjudic", "confmedia", "confbanco", "confong", "voto16", "voto15") 
sjt.frq (d[,myvars]) #tabla de frecuencias SjPlot
descr(d[, myvars])

#12. Transformar datos: recodificar
sjt.frq(d$voto16)
d$voto16r <- recode(d$voto16, "PP"="PP", "PSOE"="PSOE",
                    "Cs"="Cs", "ECPodem"="Podemos", "Podemos"="Podemos",
                    "Compromis"="Podemos", "En Marea" = "Podemos",
                    "NoVoto"="No voto", "NC"="NC", "NR"="NR", .default="Otros")
sjt.frq(d$voto16r)

sjt.frq(d$voto15)
d$voto15r <- recode(d$voto15, "PP"="PP", "PSOE"="PSOE",
                    "Cs"="Cs", "ECPodem"="Podemos", "Podemos"="Podemos",
                    "Compromis"="Podemos", "En Marea" = "Podemos",
                    "NoVoto"="No voto", "NC"="NC", "NR"="NC", .default="Otros") 
sjt.frq(d$voto15r)


#13. Compilar nueva variable - crear índice confianza pública y otro privada
myvars2 <- c("confparl", "confpart", "confjudic", "confong", "confbanco", "confmedia")
sjt.frq(d[,myvars2])

d <- mutate(d, confpub=ifelse(confparl>97 | confpart>97 | confjudic>97, NA, confparl + confpart + confjudic ))

descr(d$confpub) #comprobar variable
myvars3 <- c(myvars2, "confpub")
head(d[myvars3], n=100)
hist(d$confpub)


#14. Asignar valores perdidos
myvars4 <- c(myvars2, "voto15r", "voto16r")
sjt.frq(d[, myvars4])

d$confparl <- set_na(d$confparl, c(98, 99))
d$confpart <- set_na(d$confpart, c(98, 99))
d$confjudic <- set_na(d$confjudic, c(98, 99))
d$voto16r <- set_na(d$voto16r, "NC")
d$voto15r <- set_na(d$voto15r, c("NC"))

sjt.frq(d[, myvars4])

#Volver a recuperar los valores perdidos como validos
#d$voto16r <- as.character(d$voto16r)
#d$voto16r[is.na(d$voto16r)] <- "NC"
#d$voto16r <- factor(d$voto16r, levels=c('PP', 'PSOE', 'Podemos', 'Cs', 'Otros', 'No voto', 'NC'))

#15. Seleccionar casos
d2 <- subset(d, voto16r %in% c("PSOE", "PP"))
sjt.frq(d2$voto16r)

d3 <- subset(d, voto16r %in% c("PSOE", "PP") & confpub>20)
sjt.frq(d3$voto16r)
descr(d3$confpub)

#16. Seleccionar variables
d5 <- subset(d, select = c(cuest, voto16r))
head(d5)

d6 <- subset(d, select = c(cuest, voto16r, confpart, confparl, confjudic, confpub))
head(d6)

