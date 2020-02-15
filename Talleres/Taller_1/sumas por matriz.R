#-------------------
#autores:
#Andres Gil 
#Erika Gonzalez 
#Leonel Londono
#Analisis numerico

#------------------

matrizTriang<-function(b)
{
  sumas <-c()
  for(z in b)
  {
    
    cont = 0  
    matriz<-matrix(0,ncol=z,nrow = z)
    for(i in 1:z )
    {
      for (j in 1:i)
      {
      if(j==z)
      {
        matriz[i,j]=1
      }
        else
        {
            
        matriz[i,j]=1
        }
        
        }
      cont=cont+1
    }
sumaf=apply(matriz,1,sum)
value=sum(sumaf)
cat(value,"\n")
sumas = c(sumas,value)
  }
  
plot(b,sumas,xlab="Tamaño matriz",ylab="Sumas por matriz",main = "Tamaño matriz vs Sumas por matriz")
lines(b,sumas, col = "blue")
}
b<-c(10,20,30,35,40,50,60,70,80,90)
matrizTriang(b)

