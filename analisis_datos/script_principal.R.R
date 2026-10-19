
###############################################
# TRABAJO PRÁCTICO R
# ALUMNOS: Martínez, Matías; Videla, Santiago
###############################################

rm(list=ls())
cat("\014") # Limpiamos el espacio

set.seed(155)

#############################
# EJERCICIO ANÁLISIS DE DATOS
#############################


# Vamos a hacer un análisis geográfico de la distribuición de las ciclovías en
# CABA. Para ello de la página oficial data.buenosaires.gob.ar, descargamos los datos.

library(readr)
Ciclovias <- read_csv("Downloads/ciclovias.csv")


# Primero, vamos a visualizar qué comunas tienen la mayor cantidad de cicloviás.

library(tidyverse)
cantidad_por_comuna <- Ciclovias %>% 
  group_by(comuna) %>% 
  summarise(
    cantidad = n()
  ) %>% 
  arrange(desc(cantidad))
head(cantidad_por_comuna)

# Graficamos

ggplot(cantidad_por_comuna, aes(x = comuna, y = cantidad)) +
  geom_col(fill = "steelblue", color = "black") +
  labs(
    title = "Cicloviás por comuna",
    x = "Comuna",
    y = "Cicloviás"
  )

# Segundo, queremos saber qué comunas tienen mayr ratio de longitud por ciclovía, una
# variable que nos dice un poco sobre la forma de los caminos, si son largos en autopistas,
# o cortos en cuadras de mucha concurrencia.

promedio_por_comuna <- Ciclovias %>% 
  group_by(comuna) %>% 
  summarise(
    cantidad = n(),
    km = sum(longitud_m)/1000
  ) %>% 
  mutate( promedio = km/cantidad) %>% 
  arrange(desc(promedio))
head(promedio_por_comuna)

# Graficamos

ggplot(promedio_por_comuna, aes(x = comuna, y = promedio)) +
  geom_col(fill = "tan", color = "black") +
  labs(
    title = "Promedio de KM por ciclovía por comuna",
    x = "Comuna",
    y = "Promedio"
  )

# Vemos que la comuna 1, la que tenía más ciclovías, se empareja con la media en esta variable,
# Mientras que la 14 sigue siendo destacada, lo que nos hace pensar que es la que tiene mayor
# inversión en esta área.

# Por último, veremos como se dostribuyen los bicicleteros, lo que nos da una idea de dónde
# trabajan o pasan su tiempo los usuarios de las bicicletas. Para ellos bajamos los datos de la misa web.

library(readxl)
bicicleteros <- read_excel("Downloads/bicicleteros.xlsx")
bicicleteros <- bicicleteros %>% 
  mutate(comuna =parse_number(comuna)) # Modifico la forma de los datos

bicicleteros_por_comuna <- bicicleteros %>% 
  group_by(comuna) %>% 
  summarise(cantidad_bicicleteros = n()) %>% 
  arrange(desc(cantidad_bicicleteros))
head(bicicleteros_por_comuna)

# Graficamos
ggplot(bicicleteros_por_comuna, aes(x = comuna, y = cantidad_bicicleteros)) +
  geom_col(fill = "seagreen", color = "black") +
  labs(
    title = "Bicicleteros por comuna",
    x = "Comuna",
    y = "Bicicleteros"
  )

# Ya que estamos, hagamos el ratio de bicicleteros por km en cada comuna.

bicicleteros_por_km <- inner_join(promedio_por_comuna, bicicleteros_por_comuna) %>% 
  group_by(comuna) %>% 
  summarise(prm_bic_km = cantidad_bicicleteros / km) %>% 
  arrange(desc(prm_bic_km))
head(bicicleteros_por_km)

# Y graficamos

ggplot(bicicleteros_por_km, aes(x = comuna, y = prm_bic_km)) +
  geom_col(fill = "purple", color = "black") +
  labs(
    title = "Promedio de bicicleteros por km",
    x = "Comuna",
    y = "Promedio"
  )

# Este último ratio nos muestra como se distribuyen los usuarios de bicicletas en las comunas,
# o mejor dicho cuán "vacías" son en realidad esas ciclovías.

# Concluímos que la comuna 14 es donde más inversión se hizo y donde más se aprovecha este
# medio. Por otro lado, las demás comunas con un ratio promedio de km/ cantidad, parecen estar
# "vacías", pues tienen muy poco ratio de bicicleteros, lo que puede indicar que no es un medio usado
# en esas áreas, interesante si se quiere incentivar el uso o invertir en donde sí se usan .
