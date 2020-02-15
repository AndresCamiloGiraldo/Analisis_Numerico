

#-------------------
#autores:
#Andres Gil 
#Erika Gonzalez 
#Leonel Londono
#Analisis numerico

#------------------
parseInt = function(aux)
{
  total=0
  n=1
  while(aux>0)
  {
    valEnt = aux%%2
    total =  total+n*valEnt
    n=n*10
    
    aux = aux%/%2
    
  }
  return (total)
}

convertDeci = function(aux,c)
{
  val<-" "
  for (i in 1:c)
  {
    aux = aux*2;
    if(aux >= 1)
    {
      val = paste(val,"1",saux="")
      aux = aux - 1;
    }
    else
    {
      val = paste(val,"0",saux="")
    }
  }
  return(val)
}
cat("Decimal 11.25 ", parseInt(11),".",convertDeci(0.25,11), "\n")
cat("Decimal 0.66: ","0",".",convertDeci(0.6666666666,11), "\n")
cat("Decimal 30.6: ", parseInt(30),".",convertDeci(0.6,11), "\n")
cat("decimal 99.9: ", parseInt(99),".",convertDeci(0.9,11), "\n")