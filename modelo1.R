library(readxl)
library(tidyverse)
datos <- read_xlsx("Trabajo.xlsx")
#Auditoria de datos ------------

glimpse(datos)

datos$Nivel <- factor(datos$Nivel, levels = c("Postgrado","Grado","Tecnico"))
datos$Departamento <- 
summary(lm(IPI ~ . , data = datos))
