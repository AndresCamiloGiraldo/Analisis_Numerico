#--------------------------------
# TALLER #1
# METODO AITKEN
#--------------------------------
# Integrantes
# - Andres Camilo Giraldo Gil
# - Erika Alejandra Gonzalez
# - Leonel Steven Londono
#--------------------------------
# Analisis Numerico
#--------------------------------


Fx <- function(x){
  return (3*sin(x)^3 - 1)
}

Fx <- function(x){
  return (4*sin(x)*cos(x))
}

Biseccion <- function(a,b,i){
  e <- b-a
  error <- 1e-8
  x <- 0
  iteraciones <- 0
  vectorAux <- 0
  while (error < e & iteraciones < i ){
    x <- (a+b)/2
    if(Fx(x)*Fx(a) < 0 )
      b = x
    if(Fx(x)*Fx(b) < 0)
      a = x

    e <- (b-a)/2
    vectorAux <- c(vectorAux,x)
    iteraciones <- iteraciones+1
  }
  vectorAux <<- vectorAux
  e <<- e
  return(x)
}
Aitken <- function(x,xPos1,xPos2){
  
  resultado = xPos2 - (((xPos2 - xPos1)^2)/(xPos2 -2*xPos1+x)) 
  
  return(resultado)
}
i <-18
iteracion <- 3
Biseccion(0,2,i)
while(iteracion < i ){
  cat("i= ",iteracion," x=", Aitken(vectorAux[iteracion-2],vectorAux[iteracion-1],vectorAux[iteracion]),"\n")
  iteracion <- iteracion +1
}

cat("Resultado sin aceleracion: ", Biseccion(0,1,i), "\n")
cat("Resultado con aceleracion: ", Aitken(vectorAux[i-3],vectorAux[i-2],vectorAux[i-1]), "\n")
cat("Valor real de la solucion: 0.55382701\n")
cat("Resultado sin aceleracion: ", Biseccion(1,2,i), "\n")
cat("Resultado con aceleracion: ", Aitken(vectorAux[i-3],vectorAux[i-2],vectorAux[i-1]), "\n")
cat("Valor real de la solucion: 1.63853")

#--------------------------------
