
#-------------------
#autores:
#Andres Gil 
#Erika Gonzalez 
#Leonel Londono
#Analisis numerico

#------------------
parseToInt = function(number)
{
  dato = 0
  cont=0
  pot = 0
  while(number > 0)
  {
    dato = number %% 10
    cont = cont + (dato*(2^pot))
    pot = pot + 1
    number = number %/% 10
  }
  
  return(cont)
}
cat("representacion decimal de  101010101 es :",parseToInt(101010101),"\n")

cat("representacion decimal de 1011.101 es: ",parseToInt(1011),".", parseToInt(101),"\n")

cat("representacion decimal de 10111.010101010101 es: ",parseToInt(10111),".", parseToInt(010101010101))

cat("representacion decimal de 111.11111111 es : ",parseToInt(111),".", parseToInt(11111111))
