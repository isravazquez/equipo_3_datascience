# Integrantes:
# Arriaga Palma Fernando, arriaga141@gmail.com
# Hernández Angulo Juan de Jesus, jhernandezangulo@gmail.com
# Martínez Ibarra Hugo, hugomtzib@gmail.com
# Moreno Abrego Bryan Daniel, abre.go@outlook.com
# Silva Tijerina Gilberto, gilberto.silvat2812@gmail.com
# Vazquez Bernal Jaime Israel, israfullshot@gmail.com

# Índice
# Postwork 1, línea 16
# Postwork 2, línea 55
# Postwork 3, línea 105
# Postwork 4, línea 167


# POSTWORK 1

# Importa los datos de soccer de la temporada 2019/2020 de la primera división de la liga española a R, los datos los puedes encontrar en el siguiente enlace:
data_soccer <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

# Del data frame que resulta de importar los datos a R, extrae las columnas que contienen los números de goles anotados por los equipos que jugaron en casa (FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG)
data_scores <- data.frame("Local_x" = data_soccer$FTHG, "Visitante_y" = data_soccer$FTAG)

sample_n(data_scores, size = dim(data_scores)[1], replace = TRUE)

# Consulta cómo funciona la función table en R al ejecutar en la consola ?table
?table

# Posteriormente elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:
(table_scores <- table(data_scores))
# Lo anteior crea una tabla de frecuencias por cada número de goles "x" anotado por equipo que jugaron en casa (lado izquierdo) y la frecuencia del número de goles "y" anotados por equipos que jugaron como visitantes (lado superior).

# Eventos totales. Notar que coincide precisamente con el número total de partidos, lo cual es correcto.
sum(table_scores)

# Se construye la tabla de frecuencias. Esto crea la tabla de frecuencias relativas al calcular las proporciones.
(joint_prob <- prop.table(table_scores))
# Esta tabla, cada celda/entrada, ya representa la probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)

# Para calcular la probabilidad (marginal) de que el equipo que juega en casa anote x goles (x = 0, 1, 2, ...) es suficiente con realizar las sumas por renglón; es decir, para cada número x de goles anotados por el equipo que juega en casa.
(marginal_prob_home <- rowSums(freq_table_scores))

# Para calcular La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...) es suficiente con realizar las sumas por columna; es decir, para cada número y de goles anotados por el equipo que juega como visitante.
(marginal_prob_away <- colSums(freq_table_scores))

# Se puede contruir una tabla que considere todos los resultados anteriores.
(summary_table_probs <- cbind(joint_prob, marginal_prob_home))
# Se crea un vector necesario para construir la matriz.
(marginal_prob_visitor <- c(marginal_prob_away, sum(marginal_prob_home)))
# Se termina de crear la tabla, al pegar los datos anteriores.
(summary_table_probs <- rbind(summary_table_probs, marginal_prob_visitor))



# POSTWORK 2
library(dplyr)

# Ahora vamos a generar un cúmulo de datos mayor al que se tenía, esta es una situación habitual que se puede presentar para complementar
# un análisis, siempre es importante estar revisando las características o tipos de datos que tenemos, por si es necesario realizar alguna
# transformación en las variables y poder hacer operaciones aritméticas si es el caso, además de sólo tener presente algunas de las variables,
# no siempre se requiere el uso de todas para ciertos procesamientos.

# Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 2019/2020 de la primera división de la liga española a R, los datos los puedes encontrar en el siguiente enlace:
url1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
url1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
url1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

# NOTA: Modificar la dirección según el caso donde se deseen guardar y leer los archivos.
setwd("/home/hugomi/Documents/BEDU/Modulo2/Postwork3/DataPW")

download.file(url = url1920, destfile = "D-1920.csv", mode = "wb")
download.file(url = url1819, destfile = "D-1819.csv", mode = "wb")
download.file(url = url1718, destfile = "D-1718.csv", mode = "wb")

# Importa los archivos descargados a R
raw_data <- lapply(dir(), read.csv)

# Revisa la estructura de de los data frames al usar las funciones: str, head, View y summary.
str(raw_data[[1]]); head(raw_data[[1]]); View(raw_data[[1]]); summary(raw_data[[1]])

# Ahora se toman sólo las columnas requeridas.
# Con la función select del paquete dplyr selecciona únicamente las columnas Date, HomeTeam, AwayTeam,
# FTHG, FTAG y FTR; esto para cada uno de los data frames. (Hint: también puedes usar lapply).
data_new <- lapply(raw_data, select, c("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR"))

# Asegúrate de que los elementos de las columnas correspondientes de los nuevos data frames sean del mismo tipo (Hint 1: usa as.Date
# y mutate para arreglar las fechas). Con ayuda de la función rbind forma un único data frame que contenga las seis columnas
# mencionadas en el punto 3 (Hint 2: la función do.call podría ser utilizada).

# Se modificará el formato de fecha de la base de datos "D-1920.csv", porque el año en la fecha sólo contiene los últimos dos dígitos
# y se colcoará la fecha completa
data_mod1 <- lapply(data_new, mutate, Date = gsub("/17$", "/2017", Date))
data_mod1 <- lapply(data_mod1, mutate, Date = gsub("/18$", "/2018", Date))

# Se va a dar el formato adecuado a la fecha.
data_mod2 <- lapply(data_mod1, mutate, Date = as.Date(Date, "%d/%m/%Y"))

# Se combinarán las tres bases de datos en una sola.
data_cleaned <- do.call(rbind, data_mod2)
head(data_cleaned)
dim(data_cleaned)



# POSTWORK 3
library(ggplot2)
# Ahora graficaremos probabilidades (estimadas) marginales y conjuntas para el número de goles que anotan
# en un partido el equipo de casa o el equipo visitante.

# 1. Con el último data frame obtenido en el postwork de la sesión 2,
# elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:

# La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x=0,1,2,, y=0,1,2,)
(table_goals <- table(data_cleaned$FTHG, data_cleaned$FTAG))

# Eventos totales. Notar que coincide precisamente con el número total de partidos, lo cual es correcto.
sum(table_goals)

# Se construye la tabla de frecuencias. Esto crea la tabla de frecuencias relativas al calcular las proporciones.
(joint_proba <- prop.table(table_goals))
# Esta tabla, cada celda/entrada, ya representa la probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)

# Para calcular la probabilidad (marginal) de que el equipo que juega en casa anote x goles (x = 0, 1, 2, ...) es suficiente con realizar las sumas por renglón; es decir, para cada número x de goles anotados por el equipo que juega en casa.
(marginal_p_home <- rowSums(joint_proba))
marginal_proba_home <- data.frame(x = as.character(0:8), Px = marginal_p_home)

ggplot(marginal_proba_home, aes(x, Px)) + geom_col(col = "black", fill = "dodgerblue") +
ggtitle("Gráfica de barras para la probabilidad marginal de goles locales") +
  ylab("Probabilidad") +
  xlab("x goles locales") +
  theme_light()

# Para calcular La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...) es suficiente con realizar las sumas por columna; es decir, para cada número y de goles anotados por el equipo que juega como visitante.
(marginal_p_away <- colSums(joint_proba))
marginal_proba_away <- data.frame(y = as.character(0:6), Py = marginal_p_away)

ggplot(marginal_proba_away, aes(y, Py)) + geom_col(col = "black", fill = "limegreen") +
  ggtitle("Gráfica de barras para la probabilidad marginal de goles de visitante") +
  ylab("Probabilidad") +
  xlab("y goles de visitante") +
  theme_light()

# Un HeatMap para las probabilidades conjuntas estimadas de los números de goles que anotan el equipo de casa y el equipo visitante en un partido.
# Primero se convertira en un data frame la información de la probabilidad conjunta.
joint_proba_goals <- as.data.frame(joint_proba)
joint_proba_goals <- rename(joint_proba_goals, x = Var1, y = Var2, Pxy = Freq)
  
# Se grafica el mapa de calor, la probabilidad conjunta, la probabilidad por cada coordenada x-y.
ggplot(joint_proba_goals, aes(x, y, fill = Pxy)) + geom_tile() +
  ggtitle("Mapa de calor de la probabilidad conjunta") +
  ylab("x (Goles de equipo en casa)") +
  xlab("y (Goles de equipo como visitante)") +
  theme_light()


# La siguiente gráfica representa un análisis de probabilidades por cada gol como local
# al fijar el número de goles de visitante.
ggplot(joint_proba_goals, aes(x, y = Pxy)) + geom_col(col = "black", fill = "orange") +
  facet_wrap("y") +
  ggtitle("Probabilidad por cada gol local fijando el gol de visitante") +
  ylab("Probabilidad") +
  xlab("x goles locales") +
  theme_light()



# POSTWORK 4

# Ahora se investigará la dependencia o independencia del número de goles anotados por el equipo de casa
# y el número de goles anotados por el equipo visitante mediante un procedimiento denominado bootstrap,
# revisa bibliografía en internet para que tengas nociones de este desarrollo.


# Ya se han estimado las probabilidades conjuntas de que el equipo de casa anote X=x goles (x=0,1,... ,8),
# y el equipo visitante anote Y=y goles (y=0,1,... ,6), en un partido.
# Obtén una tabla de cocientes al dividir estas probabilidades conjuntas por el producto de las probabilidades
# marginales correspondientes.

# Función para obtener el cociente de la probabilidad conjunta y el producto de las proababildiades
# marginales

cociente_proba <- function(tabla) # x es la tabla de frecuencias relativa (probabilidad conjunta).
{
  mx<-rowSums(tabla) # Genera la probabilidad marginal de x (local).
  my<-colSums(tabla) # Genera la probabilidad marginal de y (visitante).
  dx<-length(mx)
  dy<-length(my)
  tabla_coc<-matrix(nrow = dx, ncol = dy)
  for(i in 1:dx){
    for(j in 1:dy){
      tabla_coc[i,j]<-tabla[i,j]/(mx[i]*my[j])
    }
  }
  return(tabla_coc)
}

# Esta es la tabla/matriz de cocientes.
cociente_proba(joint_proba)


# Mediante un procedimiento de boostrap, obtén más cocientes similares a los obtenidos en la tabla
# del punto anterior. Esto para tener una idea de las distribuciones de la cual vienen los cocientes
# en la tabla anterior. Menciona en cuáles casos le parece razonable suponer que los cocientes de la
# tabla en el punto 1, son iguales a 1 (en tal caso tendríamos independencia de las variables aleatorias X y Y).

# Primero filtraremos sólo las columnas necesarias del data frame de los datos de goles que resultó de la limpieza.
goles <- select(data_cleaned, FTHG, FTAG)

# Procedimiento bootstrap

# Se generan un muestreo de los datos de goles con una cierta cantidad de repeticiones.
repeticiones <- 100
goles.b <- list()
for(i in 1:repeticiones){
  goles.b[[i]] <- sample_n(goles,dim(goles)[1],replace = T)
}
# sample_n(goles,dim(goles)[1],replace = T) genera un muestreo de un data frame con el mismo número de
# registros que el data frame original, pero con reemplazamiento (algunos datos se pueden repetir),
# que precisamente es en lo que consiste el proceso bootstrap.

# Ahora, se crean las tablas de frecuencia para cada muestra.
frec.b <- lapply(goles.b, table)

# Esta función convierte la tabla de frecuencias absolutas (de arriba) en una tabla de frecuencias relativas.
frec_rel <- function(tabla){
  tabla <- tabla/sum(tabla)
}

# Lo siguiente es aplicar la función anterior a todas las tablas de frecuencias absolutas.
frec.b <- lapply(frec.b, frec_rel)

# Se aplica la función de cociente de probabilidad a todas las tablas de frecuencias relativas.
cocientes <- lapply(frec.b,cociente_proba)

# Se planteará una prueba de hipótesis para determinar si hay suficiente evidencia estadistica
# que determine si la media de estos cocientes es igual a 1 (independencia entre X e Y) o diferente
# de 1 (dependencia entre X e Y).

# A continuación, se obtienen las medias de cada muestra (tabla).
medias <- sapply(cocientes, mean)

# H0: media = 1 (independencia entre las variables X e Y) vs 
# H1: media != 1 (dependencia entre las variables X e Y).

# Obtenemos la media y desviación estándar de las muestras.
media <- mean(medias)
des.est <- sd(medias)/sqrt(length(medias))

# Como el Teorema del límite central indica que las medias se distribuyen de manera normal por lo
# que se procederá a usar el siguiente estadístico:
estadistico <- (media-1)/des.est

# Como es una prueba bilateral se usa el siguiente p valor:
p_valor <- 2*pnorm(abs(estadistico),lower.tail = F)

print(p_valor)

if (p_valor < 0.05){
  print("Se rechaza la hipótesis nula H0; es decir, con un 95% de confianza hay evidencia estadística para decir que la media de los cocientes es diferente de 1 lo cual indica que no existe independencia entre las variables")
} else{
  print("No se rechaza la hipótesis nula H0; es decir, NO hay evidencia estadística para decir que la media de los cocientes es diferente de 1 lo cual indica que existe independencia entre las variables")
}

# Se puede graficar un histograma de las medias de las muestras que apoya el resultado anterior.
hist(medias, col = "gold", border = "black",
     main = "Histograma de medias",
     xlab = "Medias", ylab = "Frecuencias")

