library(readxl)
library(tidyverse)
library(GGally)
datos <- read_xlsx("Trabajo.xlsx")
datos$Nivel <- factor(datos$Nivel, levels = c("Postgrado","Grado","Tecnico"))
datos$Departamento <- factor(datos$Departamento)
datos$Certificacion <- factor(datos$Certificacion)
datos$Modalidad <- factor(datos$Modalidad)
glimpse(datos)
#Auditoria de datos ------------


summary(lm(IPI ~ . , data = datos))
