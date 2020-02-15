#--------------------------------
# TALLER #1
# METODO DE STEFFENSEN
#--------------------------------
# Integrantes
# - Andres Camilo Giraldo Gil
# - Erika Alejandra Gonzalez
# - Leonel Steven Londono
#--------------------------------
# Analisis Numerico
#--------------------------------

#Funcion
Gx <- function(x)
{
  return (2*x + sin(x))
}

#Metodo de punto fijo donde se aplica el Aitken
PuntoFijo <- function(a,b,i)
{
  if((Gx(a)-a)*(Gx(b)-b) < 0)
  {
    x<-(a+b)/2
    iteraciones <- 0
    vectorAux <- 0
    dx = 0
    tol = 1e-8
    
    while (Gx(x) != x & iteraciones < i) 
    {
      dx<-abs(a-b)/2
      
      if(dx > tol)
      {
        if (Gx(x) < x)
        {
          b <- x
        }
        else {a <- x}
      }
      else 
      {
        break
      }  
      x<-(a+b)/2
      vectorAux <- c(vectorAux,x)
      iteraciones <- iteraciones+1
    }
    vectorAux <<- vectorAux
    return(x)
    
  }
  else
  {
    cat("No tiene raíz la funcion en ese intervalo\n")
  }

}

Aitken <- function(x,xPos1,xPos2)
{
  
  resultado = xPos2 - (((xPos2 - xPos1)^2)/(xPos2 -2*xPos1+x)) 
  
  return(resultado)
}

i <-18
iteracion <- 10

x2 <- 1.63848876953125
x3 <- 1.6384874536322
while(iteracion < i ){
  cat("i= ",iteracion," x=", Aitken(vectorAux[iteracion-2],vectorAux[iteracion-1],vectorAux[iteracion]),"\n")
  iteracion <- iteracion +1
}

valorResultado <- PuntoFijo(-100, 1,i)
valorResultado <- 100+valorResultado
cat("Resultado sin aceleracion: ", valorResultado, "\n")
cat("Resultado con aceleracion: ", Aitken(vectorAux[i-3],vectorAux[i-2],vectorAux[i-1]), "\n")
cat("Valor real de la solucion: 0.55382701 \n")
cat("Resultado sin aceleracion: ", x2 , "\n")
cat("Resultado con aceleracion: ", x3, "\n")
cat("Valor real de la solucion: 1.63853")

#--------------------------------