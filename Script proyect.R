########PROYECTO FINAL R ###########
#Integrantes:
#Katherine Del Valle
#Raquel Sierra
#Geovanna Gonzalez P. 
####################################


#CARGAR LIBRERIAS NECESARIAS
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(tidyverse)


# CARGAR LOS EXCEL DE LA CARPETA DATOS
balances <- read_excel("Datos/balances_2014.xlsx")
cias <- read_excel("Datos/cias_codebook.xlsx")
ciiu <- read_excel("Datos/ciiu.xlsx")
Formulario <- read_excel("Datos/Formulario 102.xls")



#VISUALIZAR LOS DATOS
str(balances)
head(balances)

str(cias)
head(cias)

str(ciiu)
head(ciiu)

#GENERAR TABLA DEL ARCHIVO balance_2014.xlsx
empresas<-tibble::as_tibble(balances) 

#MUTATE A LOS CAMPOS
empresas_1<- empresas %>%  mutate(Liquidez_C= (v345/v539), 
                                  Endeudamiento_A= (v599/v499),
                                  Endeudamiento_P= (v599/v698),
                                  Endeudamiento_Activo_F= (v698/v498),
                                  Apalancamiento= (v499/v698))  %>%
                           select(Empresas=nombre_cia, 
                                  Status=situacion,
                                  TipoEmpresa = tipo, 
                                  Pais=pais, 
                                  Provincia=provincia, 
                                  Canton=canton, 
                                  Ciudad=ciudad,
                                  Actividad_economica=ciiu4_nivel1,
                                  SubActividad=ciiu4_nivel6,
                                  Liquidez_C,
                                  Endeudamiento_A,
                                  Endeudamiento_P,
                                  Endeudamiento_Activo_F,
                                  Apalancamiento,
                                  fecha_const,
                                  tamanio,
                                  trabajadores_directo=trab_direc,
                                  trabajadores_administrativos=trab_admin)
                         




#ANÁLISISDE LOS DATOS, PARA TRANFORMARLOS A FACTOR 
glimpse(empresas_1)
empresas_1$Status<- as.factor(empresas_1$Status) 
empresas_1$TipoEmpresa<- as.factor(empresas_1$TipoEmpresa)
empresas_1$Actividad_economica<- as.factor(empresas_1$Actividad_economica)
empresas_1$SubActividad<- as.factor(empresas_1$SubActividad)


#MODIFICAR REGISTROS 
modificar_reg <- function(dataframe, col_n) {
  # Iterar sobre las columnas numéricas y reemplazar los -Inf y +Inf con NA
  for (col in col_n) {
    dataframe[[col]][dataframe[[col]] == -Inf | dataframe[[col]] == Inf] <- NA
  }
  
  return(dataframe)
}

col_mod <- c("Liquidez_C", "Endeudamiento_A","Endeudamiento_P", "Endeudamiento_Activo_F", "Apalancamiento")
empresas_2 <- modificar_reg(empresas_1, col_mod)


#QUITAR NA DE LOS REGISTROS
empresas_3 <- empresas_2 %>% drop_na()


# Función para imputar outliers
impute_outliers <- function(dataframe, variables) {
  for (var in variables) {
    dataframe[[var]][dataframe[[var]] < quantile(dataframe[[var]], 0.05, na.rm = TRUE)] <- mean(dataframe[[var]], na.rm = TRUE)
    dataframe[[var]][dataframe[[var]] > quantile(dataframe[[var]], 0.95, na.rm = TRUE)] <- median(dataframe[[var]], na.rm = TRUE)
  }
  return(dataframe)
}
columnas_a_imputar <- c("Liquidez_C", "Endeudamiento_A","Endeudamiento_P", "Endeudamiento_Activo_F", "Apalancamiento")




#PARTE 2
#¿El endeudamiento del activo fue mayor en empresas micro + pequeñas vs. grandes?
micro_peq_grande <- empresas_4 %>%
                    group_by(tamanio) %>%
                    summarise(Promedio_Endeudamiento = mean(Endeudamiento_A, na.rm = FALSE)) 
micro_peq_grande


graf_endeudamiento <- ggplot(micro_peq_grande, aes(x = tamanio, y = Promedio_Endeudamiento, fill = tamanio)) +
                      geom_bar(stat = "identity") +
                      scale_fill_brewer(palette = "Set3") +
                      labs(title = "Promedio del Endeudamiento Activo por Tamaño de Empresa",
                      x = "Tamaño de Empresa",
                      y = "Promedio del Endeudamiento Activo") +
                      theme_minimal()
graf_endeudamiento


#¿La liquidez por tipo de compañía es diferente entre aquellas empresas que tienen más de
#60 trabajadores directos y que cuenta con 100 a 800 trabajadores administrativos?
empresas_filtradas <- empresas_4 %>%
                       filter(trabajadores_administrativos >= 100, trabajadores_administrativos <= 800,trabajadores_directo > 60) 
empresas_filtradas

pregunta2 <- empresas_filtradas %>%
             group_by(TipoEmpresa) %>%
             summarise(Promedio_Liquidez = mean(Liquidez_C, na.rm = FALSE))
pregunta2

graf_trabajadores <- ggplot(pregunta2, aes(x = TipoEmpresa, y = Promedio_Liquidez, fill = TipoEmpresa)) +
                    geom_bar(stat = "identity") +
                    scale_fill_brewer(palette = "Set3") +
                    labs(title = "Promedio de Liquidez por Tipo de Empresa",
                    x = "Tipo de Empresa",
                    y = "Promedio de Liquidez") +
                    theme_minimal()

graf_trabajadores


#Describe el top 10 de empresas con mayor apalancamiento.
top_10_empresas <- empresas_4 %>%
                              arrange(desc(Apalancamiento)) %>%head(10) 
top_10_empresas

graf_apalancamiento <- ggplot(top_10_empresas, aes(x = Empresas, y = Apalancamiento, fill = Empresas)) +
                       geom_bar(stat = "identity") +
                       labs(title = "Top 10 Empresas con Mayor Apalancamiento",
                       x = "Empresas",
                       y = "Apalancamiento") +
                       theme_minimal() +
                       theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                       scale_fill_discrete(name = "Empresas")
graf_apalancamiento



#PARTE 3 

empresas_4 <- impute_outliers(empresas_3, columnas_a_imputar)
plot(empresas_4$Endeudamiento_A) 


#TOTAL DE EMPRESAS POR ACTIVIDAD ECONÓMICA Y EL CANTÓN RESUMIENDO EL NÚMERO DE EMPRESAS
resumen_actividad <- empresas_4 %>%
  group_by(Actividad_economica) %>%
  summarise(num_Empresas = n()) %>% view("resumen_actividad")


# MOSTRAR EL TOTAL DE EMPRESAS POR ACT ECONÓMICA Y CANTÓN
resumen_canton <- empresas_4 %>%
  group_by(Actividad_economica, Canton) %>%
  summarise(num_Empresas = n()) %>% view("resumen_canton")

#GRÁFICA DE LIQUIDEZPOR STATUS Y PROVINCIA CUANDO ESTAN ACTIVAS
#LIQUIDEZ  DE LA EMPRESA
emp_activa<- empresas_4 %>% filter(Status=="ACTIVA")
ggplot(emp_activa, aes(x=fecha_const,  y = Liquidez_C, color= Provincia)) +
  geom_line(color="red") +
  labs(title = "Gráfica comparativo de la Liquidez Corriente por provincia")+
  facet_wrap(~Provincia)+
  theme_grey()+
  theme(legend.position = "none")


#SOLVENCIA DE LA EMPRESA
ggplot(emp_activa, aes(x=fecha_const,  y = Apalancamiento, color= Provincia)) +
  geom_line(color="blue") +
  labs(title = "Gráfico comparativo entre Solvencia y provincia")+
  facet_wrap(~Provincia)+
  theme_grey()+
  theme(legend.position = "none")


#GRAFICO DE LIQUIDEZ POR STATUS Y TIPO DE EMPRESA
#LIQUIDEZ DE LA EMPRESA
ggplot(emp_activa, aes(x=fecha_const,  y = Liquidez_C, color= TipoEmpresa, group=TipoEmpresa)) +
  geom_line(color="red") +
  labs(title = "Gráfico comparativo entre Liquidez Corriente y el Tipo de Empresa")+
  facet_wrap(~TipoEmpresa)+
  theme_grey()+
  theme(legend.position = "none")    

#SOLVENCIA DE LA EMPRESA
ggplot(emp_activa, aes(x=fecha_const,  y = Apalancamiento, color= TipoEmpresa, group=TipoEmpresa)) +
  geom_line(color="green") +
  labs(title = "Gráfico comparativo entre Liquidez Corriente y Tipo de Empresa")+
  facet_wrap(~TipoEmpresa)+
  theme_grey()+
  theme(legend.position = "none")   
