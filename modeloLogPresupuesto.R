
# Libraries ---------------------------------------------------------------
library(readxl);library(tidyverse);library(nortest);library(GGally);library(olsrr);library(car);library(lmtest);library(PerformanceAnalytics); library(MASS)
source("funciones/bestSubset.R")
source("funciones/atipicos.R")
datos <- read_xlsx("Trabajo.xlsx")

# Preparacion de datos ----------------------------------------------------
datos$Nivel <- factor(datos$Nivel, levels = c("Tecnico", "Grado", "Postgrado"))
datos$Departamento <- factor(datos$Departamento)
datos$Certificacion <- factor(datos$Certificacion)
datos$Modalidad <- factor(datos$Modalidad)
datos$Presupuesto <- log(datos$Presupuesto)

modelo_og <- lm(IPI ~ ., data=datos)
modelo0 <- lm(IPI ~ 1, data=datos)
summary(modelo_og)

# Selección de variables --------------------------------------------------

bestSubset(modelo_og)

step(modelo_og, direction = "backward", trace = F)
step(modelo0, direction = "forward", scope = formula(modelo_og), trace = F)
step(modelo0, direction = "both", scope = formula(modelo_og), trace = F)
modeloSeleccion <- lm(formula = IPI ~ Presupuesto + Antiguedad + Nivel + Horas + Modalidad + Certificacion + Satisfaccion_Clima, data = datos)
modeloSeleccion$model[150,]

# Valores atípicos --------------------------------------------------------
p <- sum(hatvalues(modeloSeleccion))
which(abs(hatvalues(modeloSeleccion)) > 2*sum(hatvalues(modeloSeleccion))/nrow(modeloSeleccion$model))
which(abs(rstandard(modeloSeleccion)) > 2)
which(abs(rstudent(modeloSeleccion)) > 2)
which(cooks.distance(modeloSeleccion) > qf(0.5,p,nrow(modeloSeleccion$model)-p))
which(abs(dffits(modeloSeleccion))> 2*sqrt(p/nrow(modeloSeleccion$model)))
which(apply(abs(dfbetas(modeloSeleccion)[,-1]) > 2/sqrt(nrow(modeloSeleccion$model)), 1, sum) > 2)
which(covratio(modeloSeleccion) > 1 + 3*p/nrow(modeloSeleccion$model) |
        covratio(modeloSeleccion) < 1 - 3*p/nrow(modeloSeleccion$model))
atipicos(modeloSeleccion)

# Supuestos ---------------------------------------------------------------
n <- nrow(modeloSeleccion$model)
p <- sum(hatvalues(modeloSeleccion))
which(cooks.distance(modeloSeleccion)>qf(0.5,p,n-p))
plot(modeloPrueba, which = 4)
cooks.distance((modeloSeleccion))
plot(cooks.distance((modeloSeleccion)))

#Normalidad
plot(modeloSeleccion, 2)
modeloPrueba <- update(modeloSeleccion,log(IPI) ~ Presupuesto + Antiguedad + Nivel + Horas + Modalidad + Certificacion + Satisfaccion_Clima,data = modeloSeleccion$model[-c(80,150,15),])
ad.test(modeloPrueba$residuals)
plot(modeloPrueba)


bptest(modeloPrueba)
dwtest(modeloPrueba,alternative = "t")

plot(modeloSeleccion, which = 4)
# Umbral clásico: Cook > 4/n

