# Script para estimar múltiples barreras reproductivas
# Hecho por Luis Rodrigo Arce Valdés para el curso de "Especiación y Consecuencias Evolutivas de la Hibridación" - 2025
# Recuerda comenzar a trabar en este script desde el objecto de proyectos de R "bin.Rproj"

# Existe una gran diversidad de métodos para estimar la intensidad de barreras reproductivas entre dos pares de especies, te compartimos anexo una revisión de los métodos más frecuentes y un intento de unificarlos: Sobel&Chen, 2014.
# En este script seguiremos un método simplificado de los análisis hechos por Sánchez-Guillén et al, 2011
# Es clave recordar que estos métodos los tienes que ajustar a la biología de las especies que estudies y a los datos con los que cuentes.

# 00.- Instalación de librerías ####
# tidyr
# ggplot2
# overlapping
# lattice

# 01.- Iniciemos ####
# Limpiando nuestra área de trabajo
rm(list = ls())

# Aprovecharemos que los calculos de las barreras no son mateméticamente complejos para compartiles algunos comandos útiles de R
# Crearemos una lista y almacenaremos los datos de entrada de cada barrera
datos <- list()

# Primero enlistaremos los archivos que tenemos de barreras
temp <- list.files("../data/", pattern = ".csv")
temp

# Ahora editaremos el nombre de estos archivos para quitarle el .csv
temp <- gsub(".csv", "", temp)
temp

# Ahora usaremos un loop for para ir guardar simultaneamente todos nuestros archivos
# En este caso tenemos solo 6 archivos, pero con esta estrategia puedes leer y trabajar con miles de archivos a la vez
for (i in temp) {
  datos[[i]] <- read.csv(paste0("../data/",i,".csv"), header = T)
}
rm(i)

# Tras leer nuestros archivos editaremos el nombre de las barreras quitandoles el numero del inicio
temp <- gsub("^...","",temp) # Este comando utiliza expresiones regulares, si trabajas o piensas trabajar bioinfórmatica te conviene googlear y aprender sobre el tema: https://tldp.org/LDP/abs/html/x17129.html
temp
names(datos) <- temp
rm(temp)

# Ahora tenemos todas las barreras almecenadas en una practica lista :)
summary(datos)

# Los commandos de la familia "apply" nos permiten aplicar simultaneamente un comando a todos los objetos dentro de una lista
lapply(datos, summary)

# 02.- Aislamiento geográfico ####
# Usaremos los datos de nuestra primer barrera
head(datos$Habitat)

# "X" representa presencia y "0" ausencia.
# Definiremos la probabilidad de hibridación como "PH = poblaciones simpatricas/total de poblaciones" 
# Primero crearemos un vector que pegue los registros de elegans con los de graellsii
combinado <- paste0(datos$Habitat$I..elegans, datos$Habitat$I..graellsii)
combinado

# Las dos especies estan presentes en las observaciones con "XX", contaremos cuantas de esas hay
length(grep("XX", combinado))

# Excelente! tenemos 7 poblaciones simpátricas
A.habitat <- length(grep("XX", combinado))/length(combinado)
print("La probabilidad de hibridación por contacto geográfico es:")
A.habitat

# Eliminando elementos intermedios
rm(combinado)

# 03.- Aislamiento temporal ####
# Antes de comenzar a estimar el aslamiento temporal, visualizaremos el traslape en la reproducción usando un gráfico
# Y con ese pretexto aprenderemos a organizar tablas con tidyr y a graficar con ggplot. Ambas paquterias parte del "Tidyverse"


# En una tabla "tidy" cada columna es una variable y cada renglón una observación: https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html
# Nuestros datos no estan "tidy":
head(datos$Temporal)

# Pues la especie (elegans o graellsii), es una variable!
# tidyr nos ayuda a organizar nuestras tablas
library(tidyr)
Temporal.tidy <- tidyr::gather(datos$Temporal, key = "Especie", value = "Hora.Copula", 2:3)
head(Temporal.tidy)

# Cambiemos el nombre de las especies
Temporal.tidy$Especie <- gsub("\\.\\.",". ", Temporal.tidy$Especie)
head(Temporal.tidy)

# Con una tabla tidy podemos hacer uso del poderoso gráficador ggplot
# Para aprender ggplot con mas profundidad
# https://medium.com/idinsight-blog/how-to-make-bar-graphs-using-ggplot2-in-r-9812905df5d2
# https://r-graph-gallery.com/
library(ggplot2)
ggplot(Temporal.tidy) +
  geom_density(aes(x=Hora.Copula, fill=Especie), alpha=0.5) +
  scale_fill_manual(values = c("#377eb8","#4daf4a")) +
  labs(x="Hora de copulas", y="Frecuencia") +
  theme_classic() +
  theme(legend.position = "bottom")
rm(Temporal.tidy)

# Vemos que cada especie tiene su máximo de reproducción, varianzas diferentes y cierto traslape
# Utilizaremos dos librerias adicionales para estimar el area de sobrelape
library(overlapping)
library(lattice)

# Para que las funciones de estas paquterias funcionen emplearemos una lista nueva
B.tiempo <- list(Elegans=datos$Temporal$I..elegans, Graellsii=datos$Temporal$I..graellsii)
sobrelape <- overlapping::overlap(B.tiempo, plot = T, )
sobrelape$OV

# Las curvas se sobrelapan en un 36.6%. Ese valor representa la probabilidad de hibridación por aislamiento temporal.
B.tiempo <- round(as.numeric(sobrelape$OV), 2)
rm(sobrelape)
print("La probabilidad de hibridación por aislamiento temporal es:")
B.tiempo

# 04.- Precigoticas.Precopula ####
# El calculo de estas barreras es muy sencillo

# Definiremos la probabilidad de hibridacion por aislamiento mecánico como:
# PH = # Tandem exitosos / # de intentos de tandem
head(datos$PrecigoticasPrecopula)

# Algo así:
C.mecanica <- sum(datos$PrecigoticasPrecopula$Tandem) / sum(datos$PrecigoticasPrecopula$attempt.T)
C.mecanica <- round(C.mecanica, 2)
print("La probabilidad de hibridación por aislamiento mecánico es:")
C.mecanica

# Y ahora una conductual como:
# PH = # Copulas / # Tandems
D.conductual <- sum(datos$PrecigoticasPrecopula$Mating) / sum(datos$PrecigoticasPrecopula$Tandem)
D.conductual <- round(D.conductual, 2)
print("La probabilidad de hibridación por aislamiento conductual es:")
D.conductual

# 05.- Precigoticas.Postcopula ####
# Aquí tambien calcularemos dos barreras,

# Primero: Oviposición: % de hembras que tras la cópula ovipositaron
# PH = Numero de hembras que ovipositaron / total de hembras que copularon
head(datos$PrecigoticasPostcopula)

# Calculando cuantas hembras ovipositaron; es decir, pusieron 1 o más huevos.
hembras.ovipositaron <- nrow(datos$PrecigoticasPostcopula[datos$PrecigoticasPostcopula$clutches_with_eggs!=0,])

# Ahora estimando cuantas hembras totales copularon
hembras.copularon <- nrow(datos$PostcigoticasPostcopula)

# Ahora podremos saber qué fracción de hembras que copularon sí pusieron huevos exitosamente
E.Oviposicion <- hembras.ovipositaron/hembras.copularon
rm(hembras.copularon, hembras.ovipositaron)

print("La probabilidad de hibridación por oviposicion es:")
E.Oviposicion

# Segundo: Fertilidad: numero promedio del porcentaje de huevos fertiles
fertilidad <- datos$PrecigoticasPostcopula

# Como primer paso quitaremos las hembras que no ovipositaron (esas ya las consideramos en la barrera anterior)
fertilidad <- fertilidad[fertilidad$clutches_with_eggs!=0,]

# Observa nuestros datos, algunos fueron almacenados con NA (missing data):
fertilidad[!complete.cases(fertilidad),]

# Reemplazaremos esos datos por 0
fertilidad[is.na(fertilidad)] <- 0

# Ahora para cada hembra restante calcularemos su fertilidad
fertilidad$Huevos.fertiles <- fertilidad$Fertile_1 + fertilidad$Fertile_2 + fertilidad$Fertile_3
head(fertilidad)

# Ahora calcularemos el numero total de huevos
fertilidad$Huevos.totales <- fertilidad$Huevos.fertiles + fertilidad$Steryl_1 + fertilidad$Steryl_2 + fertilidad$Steryl_3
head(fertilidad)

# Y ahora calcularemos la fertilidad de cada hembra
fertilidad$fertilidad <- fertilidad$Huevos.fertiles / fertilidad$Huevos.totales

# Por ultimo calcularemos la media de este valor
F.Fertilidad <- mean(fertilidad$fertilidad)
rm(fertilidad)

# Este valor representa la probabilidad de hibridacion por fertilidad
F.Fertilidad <- round(F.Fertilidad, 2)
print("La probabilidad de hibridación por oviposicion es:")
F.Fertilidad

# Finalmente unamos nuestras estimaciones en un data.frame
Barreras <- data.frame(Barrera = ls(pattern = "\\."), PH = c(A.habitat,
                                                                     B.tiempo,
                                                                     C.mecanica,
                                                                     D.conductual,
                                                                     E.Oviposicion,
                                                                     F.Fertilidad))
rm(list = ls(pattern = "\\."))

# 06.- Gráfico de PH absoluto ####
ggplot(Barreras) +
  geom_col(aes(x=Barrera, y=PH, fill=Barrera), color="black") +
  scale_y_continuous(limits = c(0,1)) +
  theme_classic() +
  labs(title = "Probabilidad de hibridación por barrera reproductiva") +
  theme(legend.position = "none")

# 07.- Aplicando corrección conespecifica ####
# Estos valores representan la probabilidad de hibridación o de que haya flujo genético entre las dos especies.
# Para que tengan sentido desde un punto de vista biólogico y matemático se debe comparar con la probabilidad reproductiva de cada barrera en cruces conespecíficos.
# Estos datos ya los tenemos estimados para cruces conespecíficos de cada especie
datos$Conespecificos

# *observa como los valores en general son más altos que los que estimamos. Biológicamente, ¿Tiene sentido? 

# Filtremos los datos conespecificos para las barreras que hemos calculado
conespecificos <- datos$Conespecificos[datos$Conespecificos$Barrera %in% Barreras$Barrera,]

# Unamos ambos sets de datos
Barreras <- cbind(Barreras, conespecificos[,-1])
head(Barreras)

# Necesitamos estimar que tan efectivos son los cruces heterospecíficos *relativo* a los cruces conespecificos.
# Podemos estimar eso dividiento la probabilidad de hibridacion entre el promedio de los datos conespecificos
Barreras$Corregido <- Barreras$PH / ((Barreras$Elegans + Barreras$Graellsii)/2)
Barreras

# Las primeras dos barreras no tienen un equivalente conespcifico, así que copiaremos los valores originales
Barreras[1:2,"Corregido"] <- Barreras[1:2,"PH"]
Barreras$Corregido <- as.numeric(Barreras$Corregido)
Barreras

# Grafiquemos estos nuevos resutados
ggplot(Barreras) +
  geom_hline(aes(yintercept = 0), color="black") +
  geom_col(aes(x=Barrera, y=Corregido, fill=Barrera), color="black") +
  labs(y="PH corregido") +
  theme_classic() +
  theme(legend.position = "none")

# *¿Qué significan las barreras con una PH menor a 1?*
# ¿Y aquella en la que el valor es 1.5?

# 09.- De PH a aislamiento reproductivo ####
# El aislamiento reproductivo (AR) es lo opuesto al PH. Mientras una barrera tenga menor PH el AR es mayor, es decir, es una barrera mas efectiva para reducir la hibridación.
# Por lo tanto, podemos estimar AR:
Barreras$Aislamiento <- 1 - Barreras$Corregido
ggplot(Barreras) +
  geom_hline(aes(yintercept = 0), color="black") +
  geom_col(aes(x=Barrera, y=Aislamiento, fill=Barrera), color="black") +
  scale_y_continuous(limits = c(NA,1)) +
  labs(y="PH corregido") +
  theme_classic() +
  theme(legend.position = "none")

# *Aqui también podemos ver que en términos de oviposición el cruce heterospecífico es más exitoso que los conespecíficos.

# 10.- Calculando contribución relativa de cada barrera ####
# ¿Qué tanto cada barrera contribuye al aislamiento entre estas especies?
# ¿Cuál es el aislamiento total entre ellas?
# Aplicaremos el la formula de componentes al aislamiento de Ramsey et al (2003):
# CCn = RIn*(1-∑CCi)

# Generando una función que aplique esta fórmula
Ramsey <- function(absolutos, redondeo=2) {
  Relativos <- vector()
  for(i in 1:length(absolutos)) {
    if (i == 1){
      Relativos <- append(Relativos, absolutos[i])
    }
    else {
      n <- (absolutos[i]*(1-Relativos[i-1])) + Relativos[i-1]
      Relativos <- append(Relativos, n)
    }
  }
  return(round(Relativos, redondeo))
}

# Aplicando la función que acabamos de crear para transformar a relativos
Barreras$Relativas <- Ramsey(Barreras$Aislamiento, redondeo = 3)
head(Barreras)

print("El aislamiento total de las especies es de: ")
Barreras[6,7]

# Graficando
ggplot(Barreras) +
  geom_hline(aes(yintercept = 0), color="black") +
  geom_col(aes(x=Barrera, y=Aislamiento, fill=Barrera), color="black") +
  geom_point(aes(x=Barrera, y=Relativas)) +
  geom_line(aes(x=Barrera, y=Relativas, group=1)) +
  scale_y_continuous(limits = c(-1,1)) +
  labs(y="Aislamiento Reproductivo") +
  theme_classic() +
  theme(legend.position = "none")

# Son especies con un alto aislamiento acumulado!!!

# Se debe a una sola barrera?

# Cual es el mecanismo de aislamiento más importante entre ellas?

# TRABAJO OPCIONAL EXTRA ####
# ¡Excelente ya calculamos todas las barreras precigoticas!
# Sin embargo, nos hace falta realizar el calculo de las postcigoticas
# ¿Puedes hacerlas tu?

# La primera barrera es la barrera de supervivencia,
# ¿Los híbridos viven menos que los organismos puros?
# Definiremos la probabilidad de hibridacion por supervivencia de los hibridos por pecera como:
# Numero de adultos al final de la cría / Número de larvas al inicio de la cría
# Después calcula el promedio de la supervivencia de todas las peceras para tener un solo valor.


# Después se tiene que estimar si el éxito en la reproducción precopula y postcopula de los híbridos es menor o mayor a las especies conespficias
# Para esto puedes reutilizar las formulas precigoticas que ya usamos. Los calculos son los mismos :)


# Finalmente aplica la corrección conespecifica con estas nuevas barreras, estima la contribución relativa de cada barrera y gráfica
# Puedes hacer este ejercicio en R, en Excel o incluso a mano. Como se te facilite más.