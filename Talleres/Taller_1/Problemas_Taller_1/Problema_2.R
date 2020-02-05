#--------------------------------
# TALLER #1
# PROBLEMA #2
#--------------------------------
# Integrantes
# - Andres Camilo Giraldo Gil
# - Erika Alejandra Gonzalez
# - Leonel Steven Londono
#--------------------------------
# Analisis Numerico
#--------------------------------

raizCuadrada <- function(n, e, x){
  eActual <- c()
  eAnterior <- c()
  b <- 0
  y <- (1/2)*(x+(n/x))
  b <- abs(x-y)
  while(b > e)
  {
    x <- y
    y <- (1/2)*(x+(n/x))
    eAnterior <- c(eAnterior, b)
    b <- abs(x-y)
    eActual <- c(eActual, b)
  }
  points(eActual, eAnterior, col = "red")
  lines(eActual, eAnterior, col = "blue")
  return(cat("El valor final es : ", y, " con error de ", e))
}
f = function(x) (x)
plot(f, xlim=c(0,60), ylim=c(0,60), col = "white", xlab="Error actual",ylab="Error anterior ", main= "Error actual vs Error anterior")
abline(h=0,col="black")
raizCuadrada(7, 0.000000001, 100)


#--------------------------------
