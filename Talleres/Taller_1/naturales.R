
#-------------------
#autores:
#Andres Gil 
#Erika Gonzalez 
#Leonel Londono
#Analisis numerico

#------------------
sumaNaturales <- function(b)
{
  vecto <- c()
  for( i in b)
  {
    suma = 0
    
    for(d in 0:i^2)
    {
      suma = suma + d^2
    }
    cat(suma, "\n")
    vecto = c(vecto,suma)
  }
  plot(b,vecto,xlab="n",ylab="Suma de cuadradados",main = " n numeros vs Suma de cuadrados ")
  lines(b, vecto, col = "blue")
  
  
  
}
b<-c(10,20,30,35,40,50,60,70,80,90,100,110,120,130,140,150)

sumaNaturales(b)