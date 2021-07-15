# Postwork 7
## Data Science on Bedu
## Integrantes:
* Arriaga Palma Fernando, arriaga141@gmail.com
* Hernández Angulo Juan de Jesus, jhernandezangulo@gmail.com
* Martínez Ibarra Hugo, hugomtzib@gmail.com
* Moreno Abrego Bryan Daniel, abre.go@outlook.com
* Silva Tijerina Gilberto, gilberto.silvat2812@gmail.com
* Vazquez Bernal Jaime Israel, israfullshot@gmail.com

---

Utilizando el manejador de BDD Mongodb Compass (previamente instalado), deberás de realizar las siguientes acciones:

```r
# install.packages('mongolite')
library(mongolite)
```

Alojar el fichero match.data.csv en una base de datos llamada _match_games_, nombrando al collection como match

```r
data_match_games <- read.csv(
  "https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-07/Postwork/match.data.csv")
```

Esta instrucción realiza la conexión al servidor local de MongoDB. Se le especifica la colección a la que se conectará y su respectiva base de datos

```r
m <- mongo(collection = "match", db = "match_games")
```

Con `insert` se añaden las filas del _data frame_ a la colección

```r
m$insert(data_match_games)
```

Una vez hecho esto, realizar un count para conocer el número de registros que se tiene en la colección

```r
(m$count())
```

Si se vuelve a cargar los datos a la base de datos, usar la siguiente instrucción para eliminar la base de datos y volver a crearla

```r
m$drop()
```

Para visualizar todos los datos se puede ejecutar la siguiente instrucción (formato JSON).

```r
(m$find('{}'))
```

Realiza una consulta utilizando la sintaxis de Mongodb en la base de datos, para conocer el número de goles que metió el Real Madrid el 20 de diciembre de 2015 y contra que equipo jugó, ¿Perdió o fue goleada?

La consulta requerida se obtiene al usar el formato JSON de la siguiente forma

```r
(consulta <- m$find('{"date" : "2015-12-20", "home_team" : "Real Madrid"}'))
```

Por último, no olvides cerrar la conexión con la BDD

```r
m$disconnect(gc = TRUE)
```
