library(readxl)
library(tidyverse)
library(MetBrewer)
datos <- read_xlsx("Trabajo.xlsx")
glimpse(datos)
# Variables ---------------------------------------------------------------

#IPI : Variable Respuesta (Continua)
#Antiguedad : En años (Continua)
#Horas : En horas (Continua)
#Edad : En años (Continua)
#Presupuesto : En dólares (Continua)
#Distancia : En kilómetros (Continua)
#Satisfacción : Puntaje (Discreta) ~ Cuasi-continua
#Nivel : Categórica
#Modalidad : Categórica
#Departamento : Categórica
#Certificación : Categórica

# Antiguedad --------------------------------------------------------------

ggplot(datos, aes(Antiguedad)) + 
  geom_histogram(fill = met.brewer("Cross", type = "discrete")[1], color = "white")
ggplot(datos,aes(Antiguedad,IPI))+
  geom_point()+
  geom_smooth(method = lm)

# Horas -------------------------------------------------------------------

ggplot(datos, aes(Horas))+
  geom_histogram(fill = met.brewer("Cross", type = "discrete")[2], color = "white")
ggplot(datos, aes(Horas,IPI))+
  geom_point()+
  geom_smooth(method = lm)

# Edad --------------------------------------------------------------------

ggplot(datos, aes(Edad))+
  geom_histogram(fill = met.brewer("Cross", type = "discrete")[3], color = "white")
ggplot(datos, aes(Edad,IPI))+
  geom_point()+
  geom_smooth(method = lm)

# Presupuesto -------------------------------------------------------------

ggplot(datos, aes(Presupuesto))+
  geom_histogram(fill = met.brewer("Cross", type = "discrete")[4], color = "white")
ggplot(datos, aes(Presupuesto,IPI))+
  geom_point()+
  geom_smooth(method = lm) #Valor atipico!

# Distancia ---------------------------------------------------------------

ggplot(datos, aes(Distancia))+
  geom_histogram(fill = met.brewer("Cross", type = "discrete")[7], color = "white")
ggplot(datos, aes(Distancia,IPI))+
  geom_point()+
  geom_smooth(method = lm)

# Satisfacción ------------------------------------------------------------
ggplot(datos, aes(Satisfaccion_Clima))+
  geom_histogram(fill = met.brewer("Cross", type = "discrete")[8], color = "white")
ggplot(datos, aes(Satisfaccion_Clima,IPI))+
  geom_point()+
  geom_smooth(method = lm)

# Nivel -------------------------------------------------------------------
ggplot(datos, aes(Nivel,fill=Nivel))+
  geom_bar(color = "white")+
  scale_fill_met_d("Cross")
ggplot(datos, aes(Nivel,IPI))+
  geom_boxplot()
ggplot(datos,aes(IPI,colour = Nivel,fill = Nivel))+
  geom_density(linewidth = 0.75,alpha = 0.4) +
  scale_color_met_d("Cross") +
  scale_fill_met_d("Cross")
# Modalidad ---------------------------------------------------------------
ggplot(datos, aes(Modalidad,fill=Modalidad))+
  geom_bar(color = "white")+
  scale_fill_met_d("Juarez")
ggplot(datos, aes(Modalidad,IPI))+
  geom_boxplot()
ggplot(datos,aes(IPI,colour = Modalidad,fill = Modalidad))+
  geom_density(linewidth = 0.75,alpha = 0.4) +
  scale_color_met_d("Juarez") +
  scale_fill_met_d("Juarez")

# Departamento ------------------------------------------------------------

ggplot(datos, aes(Departamento,fill=Departamento))+
  geom_bar(color = "white")+
  scale_fill_met_d("Archambault")
ggplot(datos, aes(Departamento,IPI))+
  geom_boxplot()
ggplot(datos,aes(IPI,colour = Departamento,fill = Departamento))+
  geom_density(linewidth = 0.75,alpha = 0.2) +
  scale_color_met_d("Archambault") +
  scale_fill_met_d("Archambault")

# Certificación -----------------------------------------------------------

ggplot(datos, aes(Certificacion,fill=Certificacion))+
  geom_bar(color = "white")+
  scale_fill_met_d("Hokusai3")
ggplot(datos, aes(Certificacion,IPI))+
  geom_boxplot()
ggplot(datos,aes(IPI,colour = Certificacion,fill = Certificacion))+
  geom_density(linewidth = 0.75,alpha = 0.2) +
  scale_color_met_d("Hokusai3") +
  scale_fill_met_d("Hokusai3")

