#--------------------------------
# TALLER #1
# PROBLEMA #5
#--------------------------------
# Integrantes
# - Andres Camilo Giraldo Gil
# - Erika Alejandra Gonzalez
# - Leonel Steven Londono
#--------------------------------
# Analisis Numerico
#--------------------------------

#Funcion que contiene el algoritmo de horner
Horner <-function(coef, valorX ){
  
  resultado <- coef[1]
  iteraciones <- 0
  
  for (i in coef[2:length(coef)]) {
    resultado = resultado * valorX + i
    iteraciones <- iteraciones + 1
  }
  
  return(cat("El resultado es: ", resultado,
             "\nSe realizo en ", iteraciones*2, " operaciones minimas",
             "con", iteraciones, " multiplicaciones y ", iteraciones, " sumas."))
  
}

valorX <- -2 

coeficientes <- c(2, 0, -3 , 3, -4)
Horner (coeficientes, valorX)

#--------------------------------