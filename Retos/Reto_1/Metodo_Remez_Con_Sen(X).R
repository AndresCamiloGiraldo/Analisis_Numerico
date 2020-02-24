#--------------------------------
# RETO #1
# METODO DE REMEZ
#--------------------------------
# Integrantes
# - Andres Camilo Giraldo Gil
# - Erika Alejandra Gonzalez
# - Leonel Steven Londono
#--------------------------------
# Analisis Numerico
#--------------------------------

#Particiones en X
ParticionesX <- function(gradosPoli, suma){
  coeficientesX <- c()
  i <- 1
  while(length(coeficientesX) < gradosPoli){
    if (length(coeficientesX) == 0 )
    {
      coeficientesX[i] <- -pi/64;
      i <- i +1
    }
    coeficientesX[i] <- (coeficientesX[i-1]+ (suma))
    i <- i+1
  }

  return (coeficientesX)
}

#Particiones en Y
ParticionesY <- function(gradospoli, funcion,derivada, coeficientesX){
  coeficientesY <- c()
  j <- 1
  while (j < gradospoli+1 )
  {
    coeficientesY[j] <- funcion(coeficientesX[j])
    j <- j+1
  }

  coeficientesY[j] <- derivada(pi/128)
  return(coeficientesY)
}

#Metodo de Remez
Remez_P3 <- function(funcion, derivada, gradosPoli, suma){
  #Coeficientes de las particiones en X y Y
  coeficientesX <- ParticionesX(gradospoli,suma )
  coeficientesY <- ParticionesY(gradospoli, funcion, derivada, coeficientesX)

  #Coeficientes del sistema de ecuaciones
  coeficientesSistema = rbind(
    c(1,coeficientesX[1],(coeficientesX[1])^2,(coeficientesX[1])^3),
    c(1,coeficientesX[2],(coeficientesX[2])^2,(coeficientesX[2])^3),
    c(1,coeficientesX[3],(coeficientesX[3])^2,(coeficientesX[3])^3),
    c(0,1,2*(pi/128),3*(pi/128)^2))

  #Se resuelve el sistema de ecuaciones
  coeficientesPolinomio <- solve(coeficientesSistema, coeficientesY)

  cat("\nCoeficientes del polinomio aproximado de grado :", gradospoli, ":\n")
  print(coeficientesPolinomio)
  #Se remplazan los coeficientes en el polinomio de aproximacion


  funcionAproximada <- function(x, gradosPoli){
    coeficientesPolinomio[1] + (coeficientesPolinomio[2]*x) + (coeficientesPolinomio[3]*x^2) + (coeficientesPolinomio[4]*x^3)
  }

  return (funcionAproximada)

}

Remez_P4 <- function(funcion, derivada, gradosPoli, suma){
  #Coeficientes de las particiones en X y Y
  coeficientesX <- ParticionesX(gradospoli,suma )
  coeficientesY <- ParticionesY(gradospoli, funcion, derivada, coeficientesX)

  #Coeficientes del sistema de ecuaciones
  coeficientesSistema = rbind(
    c(1,coeficientesX[1],(coeficientesX[1])^2,(coeficientesX[1])^3,(coeficientesX[1])^4),
    c(1,coeficientesX[2],(coeficientesX[2])^2,(coeficientesX[2])^3,(coeficientesX[2])^4),
    c(1,coeficientesX[3],(coeficientesX[3])^2,(coeficientesX[3])^3,(coeficientesX[3])^4),
    c(1,coeficientesX[4],(coeficientesX[4])^2,(coeficientesX[4])^3,(coeficientesX[4])^4),
    c(0,1,2*(pi/128),3*(pi/128)^2,4*(pi/128)^3))

  #Se resuelve el sistema de ecuaciones
  coeficientesPolinomio <- solve(coeficientesSistema, coeficientesY)

  cat("\nCoeficientes del polinomio aproximado de grado: ", gradospoli, ":\n")
  print(coeficientesPolinomio)
  #Se remplazan los coeficientes en el polinomio de aproximacion


  funcionAproximada <- function(x, gradosPoli){
    coeficientesPolinomio[1] + (coeficientesPolinomio[2]*x) + (coeficientesPolinomio[3]*x^2) + (coeficientesPolinomio[4]*x^3)+(coeficientesPolinomio[5]*x^4)
  }

  return (funcionAproximada)

}

#Funcion a la cual se le va a realizar la aproximacion
funcion <- function(x){
  return (sin(x))
}

#Derivada de la funcion
derivada <- function(x){
  return(cos(x))
}

#Grado del polinomio de aproximacion
gradospoli <- 3

suma <- (2*(pi/64))/(gradospoli-1)

#Polinomio que aproxima la funcion
funcionAproximada <- Remez_P3(funcion, derivada, gradosPoli, suma)

#Grafica de la funcion seno
plot(funcion,xlim = c(-pi,pi), ylim=c(-1,1),ylab = "Y", col = "blue")
#Grafica del polinomio que aproxima la funcion
par(new = TRUE)
plot(funcionAproximada,xlim = c(-pi,pi),ylim=c(-1,1),ylab = "Y", main = "Polinomio de Aproximacion grado 3", col= "red")

#Variables que se usaran para medir los errores
ErrorAbsoluto <- 0
ErrorRelativo <- 0
ValorX <- pi/200

#Calculo del error en un punto dado
ErroAbsoluto = abs((funcion(ValorX)- funcionAproximada(ValorX))*10^-6)
ErrorRelativo  = ((ErroAbsoluto / funcionAproximada(ValorX))*100)*10^-6
#Se muestra los errores en un punto dato
cat("\nDado el punto  ",ValorX)
cat("\nEl Error Relativo es de :" ,ErrorRelativo)
cat("\nEl Error Absoluto es de :" ,ErroAbsoluto, "\n")

#--------------------------------
# Prueba con un polinomio de grado 4
#--------------------------------

#Grado del polinomio de aproximacion
gradospoli <- 4

suma <- (2*(pi/64))/(gradospoli-1)

#Polinomio que aproxima la funcion
funcionAproximada <- Remez_P4(funcion, derivada, gradosPoli, suma)

#Grafica de la funcion seno
plot(funcion,xlim = c(-pi,pi), ylim=c(-1,1),ylab = "Y", col = "blue")
#Grafica del polinomio que aproxima la funcion
par(new = TRUE)
plot(funcionAproximada,xlim = c(-pi,pi),ylim=c(-1,1),ylab = "Y", main = "Polinomio de Aproximacion grado 4", col= "red")

#Variables que se usaran para medir los errores
ErrorAbsoluto <- 0
ErrorRelativo <- 0
ValorX <- pi/200

#Calculo del error en un punto dado
ErroAbsoluto = abs((funcion(ValorX)- funcionAproximada(ValorX))*10^-6)
ErrorRelativo  = ((ErroAbsoluto / funcionAproximada(ValorX))*100)*10^-6
#Se muestra los errores en un punto dato
cat("\nDado el punto  ",ValorX)
cat("\nEl Error Relativo es de :" ,ErrorRelativo)
cat("\nEl Error Absoluto es de :" ,ErroAbsoluto)

#--------------------------------
