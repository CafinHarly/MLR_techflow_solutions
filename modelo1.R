library(readxl)
library(tidyverse)
datos <- read_xlsx("Trabajo.xlsx")
datos$Nivel <- factor(datos$Nivel, levels = c("Postgrado","Grado","Tecnico"))
datos$Departamento <- 
glimpse(datos)
#Auditoria de datos ------------



summary(lm(IPI ~ . , data = datos))
