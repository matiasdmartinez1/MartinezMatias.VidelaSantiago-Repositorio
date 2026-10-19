###############################################
# TRABAJO PRÁCTICO R
# ALUMNOS: Martínez, Matías; Videla, Santiago
###############################################

# Para que los resultados sean reproducibles:
set.seed(155)

########################
# EJERCICIO SIMULACIÓN
########################

# Cargo las funciones definidas en el archivo Funciones.R.
source("Funciones.R")

# Inciso 1: simulo una tirada con tres dados por jugador.El input son la cantidad de dados que utiliza cada uno de los jugadores. El output es la cantidad de perdidas, tanto del atacante como del defensor.
resultado_ataque(3, 3)

# Inciso 2: simulo una batalla con cinco fichas por jugador.El input son las cinco fichas de cada uno y el output es el resultado de la batalla, el cual es TRUE si gano el atacante y FALSE si gano el defensor.
simular_batalla(5, 5)

# Inciso 3: repito 1000 batallas para estimar la probabilidad de que el atacante gane con cinco fichas contra cinco.
probabilidad_ataque()
#La probabilidad de ataque converge al 18%, lo cual es lógico ya que, dadas las reglas del juego, en caso de empate el defensor consigue la victoria. 