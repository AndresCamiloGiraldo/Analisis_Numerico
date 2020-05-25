

# Analisis Numerico SIMULADOR COVID-19

# Integrantes

Andres Camilo Giraldo Gil

Erika Alejandra Gonzalez

Leonel Steven Londono

# Instalar estas librerias para poder ejecutar el proyecto


install.packages("shinydashboard")

install.packages("shinyjs")

install.packages("deSolve")

install.packages("phaseR")

# Ejecucion del proyecto

* En Rstudio, selecione todas las lineas del codigo, presion ctrl + enter para ejecutar el proyecto.
* Seleccione el modelo por el cual quiere hacer la simulacion, bien sea SIS o SIR.
* Presione calcular y observe los resultados.

# Detalles del proyecto

El proyecto se trabajo con datos reales, en base a la información suministrada por la base de datos, se tiene la poblacion total de colombia del ultimo censo realizado, pero en una escala de [1:400] lo que quiere decir que cada individuo dentro del modelo representa a 400 mas.

