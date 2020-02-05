#--------------------------------
# TALLER #1
# PROBLEMA #1
#--------------------------------
# Integrantes
# - Andres Camilo Giraldo Gil
# - Erika Alejandra Gonzalez
# - Leonel Steven Londono
#--------------------------------
# Analisis Numerico
#--------------------------------

errorTruncamiento = function(maxDigitos, numero){
  contador = 0
  aux = numero
  while(aux>1)
  {
    aux = aux/10
    
    contador = contador+1
  }
  auxTruncado = trunc(aux*10^maxDigitos)/10^maxDigitos
  solucion = (aux - auxTruncado)*10^(contador-1)
  print(solucion)
  
}
errorTruncamiento(4, 536.78)

#--------------------------------
