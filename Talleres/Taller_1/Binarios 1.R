library(R.utils)
#-------------------
#autores:
#Andres Gil 
#Erika Gonzalez 
#Leonel Londono
#Analisis numerico

#--------------------

binary<-function(Dato) 
  {
  bsum<-0
  bexp<-1
  while (Dato > 0) 
    {
    digito<-Dato %% 2
    Dato<-floor(Dato / 2)
    bsum<-bsum + digito * bexp
    bexp<-bexp * 10
  }
  return(bsum)
}
aux = pi-trunc(pi)
aux=aux*10^7

Dato<-as.numeric(aux)
bsum<-binary(Dato)
cat("Binary:1", bsum)
