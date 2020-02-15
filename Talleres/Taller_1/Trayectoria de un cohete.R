#-------------------
#autores:
#Andres Gil 
#Erika Gonzalez 
#Leonel Londono
#Analisis numerico

#------------------
cohete = function()
{
  x = c()
  y= c()
  t = 0
  value = 6+2.13*(t^2)-0.0013*(t^4)
  final = 6+2.13*((t+1)^2)-0.0013*((t+1)^4)
  while(value<final)
  {
    x = c(x,t)
    t =t+1
    value = 6+2.13*(t^2)-0.0013*(t^4)
    final = 6+2.13*((t+1)^2)-0.0013*((t+1)^4)
    y = c(y,value)
    
  }
  x = c(x,t+1)
  y= c(y,final)
  plot(x,y,xlab="Tiempo(s)",ylab="Distancia(m)",main ="Tiempo vs Distancia")
  lines(x, y, col = "blue")
  
  cat("La mayor altura alcanzada es :",value)
  
  
}

cohete()