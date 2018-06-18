##############################################################
######## Curso Vizualización de Datos - EMAS 2018 ############
########     Práctica 2 - Introducción a R       ############
##############################################################

#1. Cargar (e instala si es necesario) los paquetes que vas a utilizar

#install.packages("tidyverse")
#install.packages("sjmisc")
#install.packages("foreign")

library(tidyverse)
library(sjmisc)

#2. Limpia espacio y establece directorio de trabajo
rm(list=ls())

setwd("AQUÍ TU RUTA") #Aquí deberás poner todos los datos
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
frq(mydata2$sex)

#9. Abrir datos (estudio poselectoral CIS 2016) en formato SPSS y asignar a un data frame
d <- foreign::read.spss("cis3145t.sav", to.data.frame = T) #abierto usando paquete haven

#10. Explora los datos usando la función str() 
str(d) #viatazo general a las variables y el tipo
head(d, n=20) #listar los primeros valores de las variables
tail(d, n=20) #listar los últimos valores de las variables
summary(d) #resumen de las variables

#11. Explora las variables confparl, confpart, confjudic, confmedia, 
#confbanco, confong, voto15 y voto16
myvars <- c("confparl", "confpart", "confjudic", "confmedia", "confbanco", "confong", "voto16", "voto15") 
                        
select(d, myvars) %>% #tabla de frecuencias SjPlot
  frq() 

select(d, myvars) %>%
  descr()


#12. Transformar datos: recodificar
frq(d$voto16)
d$voto16r <- recode(d$voto16, "PP"="PP", "PSOE"="PSOE",
                    "Cs"="Cs", "ECPodem"="Podemos", "Podemos"="Podemos",
                    "Compromis"="Podemos", "En Marea" = "Podemos",
                    "NoVoto"="No voto", "NC"="NC", "NR"="NR", .default="Otros")
frq(d$voto16r)

frq(d$voto15)
d$voto15r <- recode(d$voto15, "PP"="PP", "PSOE"="PSOE",
                    "Cs"="Cs", "ECPodem"="Podemos", "Podemos"="Podemos",
                    "Compromis"="Podemos", "En Marea" = "Podemos",
                    "NoVoto"="No voto", "NC"="NC", "NR"="NC", .default="Otros") 
frq(d$voto15r)


#13. Compilar nueva variable - crear índice confianza pública y otro privada
myvars2 <- c("confparl", "confpart", "confjudic")
frq(d[,myvars2])

d <- mutate(d, confpub=ifelse(confparl>97 | confpart>97 | confjudic>97, NA, confparl + confpart + confjudic ))

descr(d$confpub) #comprobar variable
myvars3 <- c(myvars2, "confpub")
head(d[myvars3], n=100)
hist(d$confpub)


#14. Seleccionar casos
d2 <- filter(d, voto16r == "PSOE" | voto16r == "PP")
frq(d2$voto16r)

d3 <- filter(d, voto16r == "PSOE" | voto16r == "PP" & confpub>20)
frq(d3$voto16r)
descr(d3$confpub)

#15. Seleccionar variables
d5 <- select(d, cuest, voto16r)
head(d5)

d6 <- select(d, cuest, voto16r, confpart, confparl, confjudic, confpub)
head(d6)

