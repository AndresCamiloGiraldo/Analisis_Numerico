#--------------------------------
# TALLER #1
# PROBLEMA #3
#--------------------------------
# Integrantes
# - Andres Camilo Giraldo Gil
# - Erika Alejandra Gonzalez
# - Leonel Steven Londono
#--------------------------------
# Analisis Numerico
#--------------------------------

taylor = function(f, punto, a, n)
{
  val = parse(text=f)
  
  fx = function(x){eval(val[[1]])}
  
  summe = rep(NA, lenvalth=n+1)
  for(k in 1:n)
  {
    val. = Metodo(val,"x", k)
    fp = function(x) eval(val.)
    summe[k]=1/factorial(k)*(punto-a)^k *fp(a)
  }
  summe[n+1] = fx(a)
  sum(summe)
}

Metodo <- function(Termin, nombre, order = 1) 
{
  if(order < 1) stop("'order' must be >= 1")
  if(order == 1) D(Termin, nombre)
  else Metodo(D(Termin, nombre), nombre, order - 1)
}
cat("El resultado es ", round(taylor("exp(x)", 0.5, 1, 6),4))

#--------------------------------