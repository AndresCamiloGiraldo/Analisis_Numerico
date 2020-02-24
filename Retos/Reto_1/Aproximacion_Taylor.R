#--------------------------------
# RETO #1
# APROXIMACION DE TAYLOR
#--------------------------------
# Integrantes
# - Andres Camilo Giraldo Gil
# - Erika Alejandra Gonzalez
# - Leonel Steven Londono
#--------------------------------
# Analisis Numerico
#--------------------------------

library(numbers)
operaciones = 0
errorRelativo = 0
errorAbsoluto = 0
vectorErrores = c()

aproxTaylor <- function(x, polinomio, gradoDelPolinomio)
{
  seno = 0
  for (i in polinomio) 
  {
    seno = seno + i * (x^gradoDelPolinomio) 
    gradoDelPolinomio = gradoDelPolinomio - 1
  }
  return (seno)
}

error <- function(x, aproximacion)
{
  if(sin(x) == 0){
    return(0)
  } else {
    errorAbsoluto = sin(x) - aproximacion
    errorRelativo = (errorAbsoluto/sin(x))*100
    return (errorRelativo)
  }
}

operacionesNecesarias <- function(poliniomio)
{
  for(i in poliniomio)
  {
    operaciones = operaciones + 1
  }
  return (operaciones)
}

seno <- function(x)
{
  return(sin(x))
}

pruebas <- function(vector, i)
{
  errorTotal = 0
  for(x in vector)
  {
    gradoDelPolinomio = length(polinomioDeTaylor)-1
    aproximacion = aproxTaylor(x,polinomioDeTaylor, gradoDelPolinomio)
    
    cat("Valor en Taylor: ", aproximacion)
    cat("\t      Valor de Seno: ",sin(x), "\n")
    
    errorTotal = errorTotal + error(x, aproximacion) 
    operaciones = operaciones + operacionesNecesarias(polinomioDeTaylor)
  }
  errorTotal = errorTotal/i
  cat("Numero de operaciones necesarias: ", operaciones, "\n")
  cat("Error relativo: ", errorTotal, "\n")
  
  return(errorTotal)
}

polinomioDeTaylor = pracma::taylor(seno,0,4)
cat("Polinomio de Taylor: ", polinomioDeTaylor)

for(i in 1:10)
{
  cat("Pruebas con un intervalo de [-pi/64, pi/64] y una partición de (pi/64+pi/64)/",i, "\n")
  vector <- seq(from = -pi/64, to = pi/64, by = (pi/64+pi/64)/i)
  vectorErrores = c(vectorErrores, pruebas(vector, i+1))
  cat("_________________________________________________________________________________________\n")
}

iteraciones = seq(from = 1, to = 10)
plot(iteraciones, vectorErrores, xlab = "No. Iteraciones", ylab = "Error relativo", main = "Error por No. Iteraciones")
lines(iteraciones, vectorErrores, col = "blue")

#--------------------------------