library(readxl)
library(tidyverse)
library(nortest)
library(GGally)
library(olsrr)
library(car)
library(lmtest)
library(PerformanceAnalytics)
datos <- read_xlsx("Trabajo.xlsx")
datos$Nivel <- factor(datos$Nivel, levels = c("Postgrado","Grado","Tecnico"))
datos$Departamento <- factor(datos$Departamento)
datos$Certificacion <- factor(datos$Certificacion)
datos$Modalidad <- factor(datos$Modalidad)
chart.Correlation(datos,histogram = T)
glimpse(datos)

# Variables significativas ------------------------------------------------
modelo_og <- lm(IPI ~ ., data=datos)
modelo1 <- lm(IPI ~ . - Edad - Distancia - Departamento, data = datos)
summary(modelo_og) #p-value: 2.2e-16
summary(modelo1)
vif(modelo_og) #No hay multicolinealidad


# Análisis de residuales --------------------------------------------------
ols_pred_rsq(modelo1)
ols_plot_resid_stand(modelo1)
