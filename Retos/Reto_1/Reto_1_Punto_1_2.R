#--------------------------------
# RETO #1
# METODO DE HORNER PUNTO 1-2
#--------------------------------
# Integrantes
# - Andres Camilo Giraldo Gil
# - Erika Alejandra Gonzalez
# - Leonel Steven Londono
#--------------------------------
# Analisis Numerico
#--------------------------------

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
z = as.complex(-3)
sqrt(z)
xi <- z
coef <- c(2,0,-3,3,-4)
horner(coef,xi)

#--------------------------------