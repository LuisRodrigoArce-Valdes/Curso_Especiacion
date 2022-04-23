# Script para estimar múltiples barreras reproductivas
# Hecho por Luis Rodrigo Arce Valdés para el curso de "Especiación y Consecuencias Evolutivas de la Hibridación" - 2022
# Recuerda comenzar a trabar en este script desde el objecto de proyectos de R "bin.Rproj"

# Existe una gran diversidad de métodos para estimar la intensidad de barreras reproductivas entre dos pares de especies,
# te compartimos anexo una revisión de los métodos más frecuentes y un intento de unificarlos (Sobel&Chen,2014).
# En este script seguiremos un método simplificado de los análisis hechos por Sánchez-Guillén_et_al,2011
# Es clave recordad que estos métodos los tienes que ajustar a la biología de las especies que estudies y a los datos con los qu cuentes.

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
# Definiremos el aislamiento geografico como "RI = 1-(poblaciones simpatricas/total de poblaciones) 
# Primero crearemos un vector que pegue los registros de elegans con los de graellsii
combinado <- paste0(datos$Habitat$I..elegans, datos$Habitat$I..graellsii)
combinado

# Las dos especies estan presentes en las observaciones con "XX", contaremos cuantas de esas hay
length(grep("XX", combinado))

# Excelente! tenemos 7 poblaciones simpátricas
A.habitat <- 1 - (length(grep("XX", combinado))/length(combinado))
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

# Las curvas se sobrelapan en un 27% su aislamiento será entonces de 1 - 0.27
B.tiempo <- round(as.numeric(1-sobrelape$OV), 2)
rm(sobrelape)

# 04.- Precigoticas.Precopula ####
# El calculo de estas barreras es muy sencillo

# Definiremos una barrera mecánica como:
# RI = 1 - (# Tandem exitosos / # de intentos de tandem)
head(datos$PrecigoticasPrecopula)

# Algo así:
C.mecanica <- 1 - (sum(datos$PrecigoticasPrecopula$Tandem) / sum(datos$PrecigoticasPrecopula$attempt.T))
C.mecanica <- round(C.mecanica, 2)

# Y ahora una conductual como:
# RI = 1 - (# Copulas / # Tandems)
D.conductual <- 1 - (sum(datos$PrecigoticasPrecopula$Mating) / sum(datos$PrecigoticasPrecopula$Tandem))
D.conductual <- round(D.conductual, 2)

# 05.- Precigoticas.Postcopula ####
# Aquí tambien calcularemos dos barreras,

# Primero: Oviposición: % de hembras que tras la cópula ovipositaron
# RI = 1 - (Numero de hembras que ovipositaron / total de hembras que copularon)
hembras.ovipositaron <- nrow(datos$PrecigoticasPostcopula[datos$PrecigoticasPostcopula$clutches_with_eggs!=0,])
hembras.copularon <- nrow(datos$PostcigoticasPostcopula)
E.Oviposicion <- 1-(hembras.ovipositaron/hembras.copularon)
rm(hembras.copularon, hembras.ovipositaron)

# Segundo: Fertilidad: numero promedio del porcentaje de huevos fertiles
fertilidad <- datos$PrecigoticasPostcopula

# Como primer paso quitaremos las hembras que no ovipositaron (esas ya las consideramos en la barrera anterior)
fertilidad <- fertilidad[fertilidad$clutches_with_eggs!=0,]

# Observa nuestros datos, algunos fueron almacenados con NA (missing data)
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

# Y obtendremos asi nuestro indice de aislamiento por esta barrera
F.Fertilidad <- 1 - (round(F.Fertilidad, 2))

# Finalmente unamos nuestras estimaciones en un data.frame
Barreras <- data.frame(Barrera = ls(pattern = "\\."), Aislamiento = c(A.habitat,
                                                                     B.tiempo,
                                                                     C.mecanica,
                                                                     D.conductual,
                                                                     E.Oviposicion,
                                                                     F.Fertilidad))
rm(list = ls(pattern = "\\."))

# 06.- Gráfico de Barreras absolutas ####
ggplot(Barreras) +
  geom_col(aes(x=Barrera, y=Aislamiento, fill=Barrera), color="black") +
  scale_y_continuous(limits = c(0,1)) +
  theme_classic() +
  theme(legend.position = "none")

# 07.- Aplicando corrección conespecifica ####
# Contamos con los valores de estas mismas barreras calculados en cruzas conespecificas
head(datos$Conespecificos)

# Primero filtremos los datos conespecificos para las barreras que hemos calculado
conespecificos <- datos$Conespecificos[datos$Conespecificos$Barrera %in% Barreras$Barrera, ]

# Unamos ambos sets de datos
Barreras <- cbind(Barreras, conespecificos[,-1])
head(Barreras)

# Dado que ya contamos con el aislamiento en forma de barreras, 
# emplearemos la siguiente formula para aplicar una corrección conespecifica:
# RI = aislamiento heteroespecifico - (asilamiento sp1 + aislamiento sp2)/2
Barreras$Corregido <- Barreras$Aislamiento - ((Barreras$Elegans + Barreras$Graellsii)/2)
head(Barreras)

# Las primeras dos barreras no tienen un equivalente conespcifico, así que copiaremos los valores originales
Barreras[1:2,"Corregido"] <- Barreras[1:2,"Aislamiento"]
Barreras$Corregido <- as.numeric(Barreras$Corregido)
head(Barreras)

# Grafiquemos estos nuevos resutados
ggplot(Barreras) +
  geom_hline(aes(yintercept = 0), color="black") +
  geom_col(aes(x=Barrera, y=Corregido, fill=Barrera), color="black") +
  scale_y_continuous(limits = c(-1,1)) +
  labs(y="Aislamiento Reproductivo") +
  theme_classic() +
  theme(legend.position = "none")

# La escala ahora va de -1 a 1, ¿porqué?
# ¿Qué significa un valor negativo en una barrera reproductiva?

# 08.- Calculando contribución relativa de cada barrera ####
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
Barreras$Relativas <- Ramsey(Barreras$Corregido, redondeo = 3)
head(Barreras)

# Graficando
ggplot(Barreras) +
  geom_hline(aes(yintercept = 0), color="black") +
  geom_col(aes(x=Barrera, y=Corregido, fill=Barrera), color="black") +
  geom_point(aes(x=Barrera, y=Relativas)) +
  geom_line(aes(x=Barrera, y=Relativas, group=1)) +
  scale_y_continuous(limits = c(-1,1)) +
  labs(y="Aislamiento Reproductivo") +
  theme_classic() +
  theme(legend.position = "none")

# TRABAJO OPCIONAL EXTRA ####
# ¡Excelente ya calculamos todas las barreras precigoticas!
# Sin embargo, nos hace falta realizar el calculo de las postcigoticas
# ¿Puedes hacerlas tu?

# La primera barrera es la barrera de supervivencia,
# ¿Los híbridos viven menos que los organismos puros?
# Definiremos la barrera de supervivencia por pecera como:
# 1 - Numero de adultos al final de la cría / Número de larvas al inicio de la cría
# Después calcula el promedio de la supervivencia de todas las peceras para tener un solo valor.


# Después se tiene que estimar si el éxito en la reproducción precopula y postcopula de los híbridos es menor o mayor a las especies conespficias
# Para esto puedes reutilizar las formulas precigoticas que ya usamos. Los calculos son los mismos :)


# Finalmente aplica la corrección conespecifica con estas nuevas barreras, estima la contribución relativa de cada barrera y gráfica
# Puedes hacer este ejercicio en R, en Excel o incluso a mano. Como se te facilite más.
# 1 punto extra de calificación final si lo realizas correctamente!