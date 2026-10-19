
###############################################
# TRABAJO PRÁCTICO R
# ALUMNOS: Martínez, Matías; Videla, Santiago
###############################################

########################
# EJERCICIO SIMULACIÓN
########################

# Vamos a hacer el ejercicio del ataque en el T.E.G
#Funciones definidad y usadas en este ejercicio
##Inciso 1
#Escribo la función llamada resultado ataque que recibe dos inputs: la cantidad de dados que utiliza el atacante y los que utiliza el defensor. 
resultado_ataque <- function(n_atacante, n_defensor) {
  #tiro los dados de manera aleatoria, una cantidad n veces (entr 1 y 3), permitiendo que se puedan repetir los números. 
  #Utilizo el comando sample, el cual selecciona números (en este caso del 1 al 6) aleatoriamente. 
  dados_atacante <- sample(1:6, size = n_atacante, replace = TRUE)
  dados_defensor <- sample(1:6, size = n_defensor, replace = TRUE)
  #Ordeno los numeros obtenidos de mayor a menor. 
  dados_atacante <- sort(dados_atacante, decreasing = TRUE)
  dados_defensor <- sort(dados_defensor, decreasing = TRUE)
  #Creamos dos variables y le asigno un valor inicial de cero
  perdidas_atacante <- 0
  perdidas_defensor <- 0
  #Con el for busco repetir desde 1 hasta el mínimo de dados que tengan (es decir, la cantidad de duelos que va a haber).
  for (i in 1:min(n_atacante, n_defensor)) {
    if (dados_atacante[i] > dados_defensor[i]) {
      perdidas_defensor <- perdidas_defensor + 1
    } else {
      perdidas_atacante <- perdidas_atacante + 1
    }
  }
  #A partir del anterior if, si el primer dado del atacanta es mayor al primer dado del defensor, le asigno una perdida más al defensor; sino, al atacante.
  return(c(atacante = perdidas_atacante, defensor = perdidas_defensor))
}
#Obtengo como retorno un vector en donde tenemos el resultado de cada una de las perdidas, tanto del atacante como del defensor.

#probamos la funcion 
resultado_ataque(3,3)

##Inciso 2 
#Creo la función llamada simular_batalla en donde recibo como input cada una de las fichas de los jugadores.
simular_batalla <- function(fichas_atacante, fichas_defensor) {
  while (fichas_atacante > 1 && fichas_defensor > 0) {
    #Utilizamos el while para imponer que la función se desarrolle siempre que las fichas del atacante sean mayores a uno y las del defensor a cero.
    dados_a <- min(3, fichas_atacante - 1)
    dados_d <- min(3, fichas_defensor)
    #calculamos cuantos dados puede tirar cada uno en esa ronda.Utilizo el min (.) porque nunca ningún jugador puede tirar más de tres dados en una batalla.
    perdidas <- resultado_ataque(dados_a, dados_d)
    #Utilizo la función resultado ataque creada en el inciso previo.
    fichas_atacante <- fichas_atacante - perdidas[1]
    fichas_defensor <- fichas_defensor - perdidas[2]
    #guardo las fichas de cada uno de los jugadores (le resto a las fichas que tenían la cantidad perdida)
  }
  # Una vez que se dejen de cumplir las condicones (que el atacante tenga más de una ficha y el defensor al menos una), se detiene el while y se llega a un resultado. En caso de que sea TRUE, ganó el atacante; FALSE este no logró conquistar.
  return(fichas_defensor == 0)
}

#probamos la funcion
simular_batalla(5,5)

##inciso 3
#creo la función probabilidad ataque
probabilidad_ataque <- function() {
  #defino una variable llamada resultados que simula 1000 veces un ataque de 5 fichas contra 5 fichas.
  resultados <- replicate(1000, simular_batalla(5, 5))
  #luego defino probabilidad, la cual saca un promedio de los 1000 resultados obtenidos anteiormente. Si es true es 1 y si es false es 0.
  probabilidad <- mean(resultados)
  #la función probabilidad_ataque devuelve el promedio de los 1000 resultados, obteniendo así la probabilidad de ganar del atacante (para una muestra de n=100).
  return(probabilidad)
}

# Vemos que, con 5 contra 5 fichas, la probabilidad converge hacia 18% aproximadamente, 
# lo cual tiene sentido por las reglas del juego.
