#Integrantes:
#Katherine
#Raquel
#Geovanna Gonzalez P. 

#cargar paquetes
library(tidyverse)
library(openxlsx)
library(readxl)


#Importar base de datos
balances_2014 <-read.xlsx("C:/Users/USUARIO/Desktop/Proyecto_R/balances_2014.xlsx")

#Explorar data banco
str(balances_2014)

datos <- datos %>%
  mutate(nueva_variable = variable_existente)
