
###############################################
# TRABAJO PRÁCTICO R
# ALUMNOS: Martínez, Matías; Videla, Santiago
###############################################

rm(list=ls())
cat("\014") # Limpiamos el espacio

set.seed(155)

########################
# EJERCICIO ECONOMETRÍA
########################

# 1)

# Primero, abrimos la base de datos
library(readr)
datos <- read_csv("Downloads/gapminder.csv")

# Aislamos las observaciones de Argentina
library(tidyverse)
argentina <- filter(datos, country == "Argentina")

# Graficamos la evolución de la variable de ingreso por persona
library(ggplot2)
ggplot(argentina, aes(x = year, y = income_per_person)) +
  geom_point(color = "black", size = 2, shape = 16) +
  labs(title = "Ingreso per cápita por año en Argentina",
       x = "Año",
       y = "Ingreso por persona")

# Observamos una tendencia al alza, con una caída en los 80, pero una repuntada en general


# 2)

# Retiro de la muestra los últimos 10 años para el entrenamiento

arg_entrenamiento <- argentina %>% 
  filter(year < 2000)

# Modelo a:
mod_a <- lm(income_per_person ~ year, data = arg_entrenamiento)
sum_a <- summary(mod_a)
print(sum_a)

plot(arg_entrenamiento$year, arg_entrenamiento$income_per_person,
     main = "Modelo a",
     xlab = "Año",
     ylab = "Ingreso por persona")
abline(mod_a, col = "red")

# Modelo b:
mod_b <- lm(income_per_person ~ poly(year, 2, raw = TRUE), data = arg_entrenamiento)
sum_b <- summary(mod_b)
print(sum_b)

grid_b <- data.frame(year = seq(min(arg_entrenamiento$year), 
                                max(arg_entrenamiento$year), 
                                length.out = 400))
grid_b$pred_b <- predict(mod_b, newdata = grid_b)
plot_b <- ggplot() +
  geom_point(data = arg_entrenamiento, aes(x = year, y = income_per_person)) +
  geom_line(data = grid_b, aes(x = year, y = pred_b), color = "green") +
  labs(
    title = "Modelo b",
    x = "Año",
    y = "Ingreso por persona"
  )
print(plot_b)

# Modelo c:
mod_c <- lm(income_per_person ~ poly(year, 10, raw = TRUE), data = arg_entrenamiento)
sum_c <- summary(mod_c)
print(sum_c)

grid_c <- data.frame(year = seq(min(arg_entrenamiento$year), 
                                max(arg_entrenamiento$year), 
                                length.out = 400))
grid_c$pred_c <- predict(mod_c, newdata = grid_c)
plot_c <- ggplot() +
  geom_point(data = arg_entrenamiento, aes(x = year, y = income_per_person)) +
  geom_line(data = grid_c, aes(x = year, y = pred_c), color = "black") +
  labs(
    title = "Modelo c",
    x = "Año",
    y = "Ingreso por persona"
  )
print(plot_c)

# Vemos como a medida que auementamos el grado del polinomio, la predicción se va acercando
# a los datos reales y su tendencia.


# 3)

# a)
# Elegimos 5 países de sudamérica para el siguiente inciso:

paises_elegidos <- c("Argentina", "Brazil", "Uruguay", "Chile", "Colombia")

seleccion <- datos %>% 
  filter(country %in% paises_elegidos) %>% 
  select(country, income_per_person, year)

# Necesito los datos reordenados

sel_acomodada <- seleccion %>% 
  pivot_wider(names_from = country, values_from= income_per_person)

matriz_ingresos <- cor(sel_acomodada, use = "complete.obs")
print(matriz_ingresos)

# b) 
# Agrego los datos de crecimiento con la formula de logaritmos

crec_sel <- seleccion %>% 
  group_by(country) %>% 
  mutate(crecimiento = (income_per_person - lag(income_per_person))/ lag(income_per_person)) %>% 
  ungroup()

# De nuevo necesito reordenar

crec_acomodado <- crec_sel %>%
  select(year, country, crecimiento) %>%
  pivot_wider(names_from = country, values_from = crecimiento)

matriz_crecimiento <- cor(crec_acomodado, use ="complete.obs")
print(matriz_crecimiento)


# 5) 

# Debemos elegir un año en concreto, y elegimos el 2005

datos2005 <- datos %>% 
  filter(!is.na(life_expectancy), !is.na(life_expectancy_female)) %>% 
  filter(year==2005) %>% 
  mutate(
    life_expectancy = as.numeric(life_expectancy),
    life_expectancy_female= as.numeric(life_expectancy_female)
  )

# Tuvimos que poner as.numeric porque la regresión nos daba muchos coeficientes, 
# e intuimos que tomaba los datos como texto, por suerte se solucionó.

ggplot(datos2005, aes(x = life_expectancy_female, y = life_expectancy)) +
  geom_point(color = "black", size = 2, shape = 16) +
  labs(title = "Esperanza de vida vs Esperanza de vida en Mujeres",
       x = "Esperanza de vida en mujeres",
       y = "Esperanza de vida")

# Observamos (lógicamente) una fuerte correlación entre las variables.


# 6)

mod_6 <- lm(life_expectancy ~ life_expectancy_female, data = datos2005)
sum_6 <- summary(mod_6)
print(sum_6)

# Observamos el R2 que es casi 1, lo que nos prueba la correlación y el peso explicativo 
# de la esperanza femenina en la esperanza global.


# 7)

beta1_hat <- coef(mod_6)["life_expectancy_female"]
se_beta1  <- coef(sum_6)[2, "Std. Error"]
n7 <- nrow(datos2005)
df7 <- n7-2
t_stat7  <- beta1_hat / se_beta1
p_value7 <- pt(-abs(t_stat7), df = df7)
print(p_value7)

# Vemos que el p-value es practicamente 0, lo que da fuerza a la significatividad de la variable.


# 8)

mod_8 <- lm(life_expectancy ~ life_expectancy_female + income_per_person, data= datos2005)
sum_8 <- summary(mod_8)
print(sum_8)

# Notemos que el efecto B1 es mucho mayor que B2. Además, este nuevo modelo tiene un R2
# Casi igual al anterior (0,95 contra 0,93), lo que indica que income_per_person tiene en si
# poco peso exlpicativo, y no es necesario agregarlo en este estudio.


# 9)

datos2005 <- datos %>% 
  filter(!is.na(life_expectancy), !is.na(life_expectancy_female), !is.na(children_per_woman), !is.na(child_mortality)) %>% 
  filter(year==2005) %>% 
  mutate(
    life_expectancy = as.numeric(life_expectancy),
    life_expectancy_female= as.numeric(life_expectancy_female),
    income_per_child = income_per_person/ children_per_woman
    
  )

# Creamos la variable income_per_children, que nos dice el bienestar económico de cada
# chico. Vamos a hacer un modelo con esa variable, child_mortality y children_per_woman,
# osea que queremos estimar la esperanza de vida observando las condiciones de los infantes,
# en salud, economía y cantidad en sí.

mod_9 <- lm(life_expectancy ~ children_per_woman + child_mortality + income_per_child, data= datos2005)
sum_9 = summary(mod_9)
print(sum_9)

datos2005 <- datos2005 %>%
  mutate(predict9 = predict(mod_9))
ggplot(datos2005, aes(x = predict9, y = life_expectancy)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "red")+
  labs(
    x = "Esperanza de vida (modelo)",
    y = "Esperanza de vida(real)"
  )

# Observando los datos y nuestras predicciones, en la columna predict9, concluímos en que el modelo
# es bastante acertado, pues casi es una relación 1 a 1 perfecta.

