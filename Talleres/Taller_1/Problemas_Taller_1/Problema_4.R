#--------------------------------
# TALLER #1
# PROBLEMA #4
#--------------------------------
# Integrantes
# - Andres Camilo Giraldo Gil
# - Erika Alejandra Gonzalez
# - Leonel Steven Londono
#-------------------------------
# Analisis Numerico
#--------------------------------

error = function(velocidad, tiempo, evelocidad, eTiempo){
  errorAbsoluto <- (velocidad*evelocidad) + (tiempo*eTiempo)
  distancia <- velocidad*tiempo
  errorRelativo <- (evelocidad/velocidad)+(eTiempo/tiempo)
  cat("Distancia recorrida: ", distancia,"m/s", " con un rango de error de : ",
      distancia-errorAbsoluto, "m/s  <<- ", distancia, "m/s ->> ", distancia+errorAbsoluto,"m/s",
      " \nEl error relativo es de : ", errorRelativo*100, "%")
}

error(4,5,0.1,0.1)

#--------------------------------