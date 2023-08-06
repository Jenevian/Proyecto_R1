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

#creación de variables
Empresas<-c(balances_2014$nombre_cia)
Status<-c(balances_2014$situacion)
Tipo_de_empresa<-c(balances_2014$tipo)
País<-c(balances_2014$pais)
Provincia<-c(balances_2014$provincia)
Cantón <-c(balances_2014$canton)
Ciudad <-c(balances_2014$ciudad)
Actividad_económica<-c(balances_2014$ciiu4_nivel1)
Subactividad<-c(balances_2014$ciiu4_nivel6)

#Liquidez corriente
T_Activos_corrientes<-c(balances_2014$v345)
T_pasivos_corrientes<-c(balances_2014$v539)

Liquidez_c<-c(T_Activos_corrientes/T_pasivos_corrientes)

#Endeudamiento del activo = Pasivo / Activo




Endeudamiento patrimonial = Pasivo / Patrimonio
Endeudamiento del Activo Fijo = Patrimonio / Activo No Corriente
Apalancamiento = Activo / Patrimonio

