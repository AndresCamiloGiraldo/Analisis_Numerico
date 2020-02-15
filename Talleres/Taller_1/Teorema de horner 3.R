#-------------------
#autores:
#Andres Gil 
#Erika Gonzalez 
#Leonel Londono
#Analisis numerico

#------------------
evaluar = function(b)
{
   x = b
   z = b
   resultI = 0
   resultF = 0
  #Se evalua la x en P(x)
  for(i in 0:50)
  {
  resultI = round(x^i + resultI,10)  
  }
   #Se evalua x en la expresión Q(x)
  resultF = ((z^51)-1)/(z-1)
   cat("resultf", resultF)
   cat("resulti",resultI)
}
options(digits=12)
evaluar(1.0001)