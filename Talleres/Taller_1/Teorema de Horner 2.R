
#-------------------
#autores:
#Andres Gil 
#Erika Gonzalez 
#Leonel Londono
#Analisis numerico

#------------------

horner <- function(coef, x)
  {
    valorf <- coef[1]
    itera <-0
  
    for(k in coef[2:length(coef)])
      {
        valorf = (x*valorf) + k
        itera = itera + 2
      }
  return(cat("Resultado: ", valorf, "\n con un numero minimo de operaciones de: ", itera,"\n con: ",itera/2," multiplicaciones y ",itera/2, " sumas"))
}
xi <- -2
coef <- c(2,0,-3,3,-4)
horner(coef,xi)