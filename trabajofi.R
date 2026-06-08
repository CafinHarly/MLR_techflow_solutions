# 1. conversion a valores factores (Grado),(Administracion) ----------
Trabajo$Modalidad = factor(Trabajo$Modalidad)
Trabajo$Certificacion = factor(Trabajo$Certificacion)
Trabajo$Nivel = factor(Trabajo$Nivel, levels = c("Grado","Tecnico","Postgrado"))
Trabajo$Departamento = factor(Trabajo$Departamento, levels = c("Admin","Soporte","TI","Ventas"))
str(Trabajo)
library(olsrr)
#1.1 grafico de correlacion 
# library(GGally)
# ggpairs(Trabajo)

# modelo antes de la limpieza -----
modelosin = lm(IPI~.,data=Trabajo)
anova(modelosin)

library(olsrr)
ols_plot_resid_stand(modelosin)
ols_plot_cooksd_bar(modelosin)
ols_plot_cooksd_chart(modelosin)
# anova
anova(modelosin)
summary(Trabajo$Presupuesto)

#outliers
#balanceo
hii1<-hatvalues(modelosin)
p<-sum(hii1)
n<-nrow(Trabajo)
2*p/n >1
which(abs(hii1)>2*p/n)

#leverege
hii1<-hatvalues(modelosin)
p<-sum(hii1)
n<-nrow(Trabajo)
2*p/n
which(abs(hii1)>2*p/n)
c1<-which(abs(hii1)>2*p/n)
# Residuos Estandarizados
re<-rstandard(modelosin)
which(abs(re)>2)
c2<-which(abs(re)>2)
# residuos student
res<-rstudent(modelosin)
which(abs(res)>2)
c3<-which(abs(res)>2)

# distancia cook
di<-cooks.distance(modelosin)
p<-sum(hii1)
n<-nrow(Trabajo)
ref1<-qf(0.5,p,n-p)
which(di>ref1)
c5<-which(di>ref1)

#DFFITS 
dfit<-dffits(modelosin)
ref2<-2*sqrt(p/n)
which(abs(dfit)>ref2)
c6<-which(abs(dfit)>ref2)

#DFBETAS
dfbe<-dfbetas(modelosin)
ref3<-2/sqrt(n)
res2<-abs(dfbe[,-1])>ref3
which(apply(res2,1,sum)>2) # matriz
c7<-which(apply(res2,1,sum)>2)

#COVRATIO 
covr<-covratio(modelosin)
which(covr>1+3*p/n | covr<1-3*p/n)
c8<-which(covr>1+3*p/n | covr<1-3*p/n)


final<-c(c1,c2,c3,c6,c7,c8)
tabla_basica <- table(final)
tabla_basica
library(MASS)
horizonte = formula(IPI~Antiguedad+Horas+Edad+Presupuesto+Distancia+Satisfaccion_Clima+Nivel+Modalidad+Departamento+Certificacion)
modelosin0 = lm(IPI~1,data=Trabajo)
step(modelosin0, scope = horizonte, direction = "both", trace=0)



# modelo con la limpieza ------

arreglando_data = Trabajo
arreglando_data$Presupuesto[arreglando_data$Presupuesto == 10000000] <- NA
library(VIM)
colSums(is.na(arreglando_data)) # 2 valores NA

# aplicacion del KNN
arreglando_data <- kNN(arreglando_data, 
                       variable = "Presupuesto", 
                       dist_var = c("Antiguedad", "Edad", "Nivel", "Departamento", "Horas"), 
                       k = 5,
                       imp_var = FALSE) # imp_var = FALSE evita que cree columnas extra molestas

colSums(is.na(arreglando_data)) 

# nuevos valores de presupuesto
#90.17  142841 6.1  55 Grado
#95.00 143748 5.6 71 Grado Presencial

#modelo con la data arreglada
modeloconarreglos = lm(IPI~.,data=arreglando_data)

ols_plot_resid_stand(modeloconarreglos)
ols_plot_cooksd_bar(modeloconarreglos)
ols_plot_cooksd_chart(modeloconarreglos)
ols_plot_dfbetas(modeloconarreglos)
ols_plot_dffits(modeloconarreglos)


anova(modeloconarreglos)
summary(arreglando_data$Presupuesto)
horizonte = formula(IPI~Antiguedad+Horas+Edad+Presupuesto+Distancia+Satisfaccion_Clima+Nivel+Modalidad+Departamento+Certificacion)
modelosconarreglo0 = lm(IPI~1,data=arreglando_data)
step(modelosconarreglo0, scope = horizonte, direction = "both", trace=0)
prueba2
# modelo con la limpieza

colSums(is.na(Trabajo))

#Ctrl + Alt + Shift + M
# Quantiles de IPI y otras numéricas 
quantile(Trabajo$IPI, probs = seq(0, 1, 0.25), na.rm = TRUE) # good Y

quantile(Trabajo$Antiguedad, probs = seq(0, 1, 0.25), na.rm = TRUE) #good cuantitativa discreta??
quantile(Trabajo$Horas, probs = seq(0, 1, 0.25), na.rm = TRUE) # good cuantitativa continua
quantile(Trabajo$Edad, probs = seq(0, 1, 0.25), na.rm = TRUE) # good cuantitativa discreta
quantile(Trabajo$Presupuesto, probs = seq(0, 1, 0.25), na.rm = TRUE) # valor atipico10000000.0 - 498.0 
quantile(Trabajo$Distancia, probs = seq(0, 1, 0.25), na.rm = TRUE) # para mi esta bueno
quantile(Trabajo$Satisfaccion_Clima, probs = seq(0, 1, 0.25), na.rm = TRUE) # good 



#outlieerrs
#balanceo
hii11<-hatvalues(modeloconarreglos)
p1<-sum(hii11)
n1<-nrow(arreglando_data)
2*p1/n1 >1
which(abs(hii11)>2*p1/n1)

#leverege
hii11<-hatvalues(modeloconarreglos)
p1<-sum(hii11)
n1<-nrow(arreglando_data)
2*p1/n1
which(abs(hii11)>2*p1/n1)
c11<-which(abs(hii11)>2*p1/n1)
# Residuos Estandarizados
re1<-rstandard(modeloconarreglos)
which(abs(re1)>2)
c21<-which(abs(re1)>2)
# residuos student
res1<-rstudent(modeloconarreglos)
which(abs(res1)>2)
c31<-which(abs(res1)>2)

# distancia cook
di1<-cooks.distance(modeloconarreglos)
p1<-sum(hii11)
n1<-nrow(arreglando_data)
ref11<-qf(0.5,p1,n1-p1)
which(di1>ref11)
c51<-which(di1>ref11)

#DFFITS 
dfit11<-dffits(modeloconarreglos)
ref21<-2*sqrt(p1/n1)
which(abs(dfit11)>ref21)
c61<-which(abs(dfit11)>ref21)

#DFBETAS
dfbe1<-dfbetas(modeloconarreglos)
ref31<-2/sqrt(n1)
res21<-abs(dfbe1[,-1])>ref31
which(apply(res21,1,sum)>2) # matriz
c71<-which(apply(res21,1,sum)>2)

#COVRATIO 
covr1<-covratio(modeloconarreglos)
which(covr1>1+3*p1/n1 | covr1<1-3*p1/n1)
c81<-which(covr1>1+3*p1/n1 | covr1<1-3*p1/n1)


final1<-c(c11,c21,c31,c61,c71,c81)
tabla_basica1 <- table(final1)
tabla_basica1




# data nueva eliminado filas  6 filas-------

# 50, 15 ,80,125,142,150

datasin6filas = arreglando_data[-c(50,15,80,125,142,150),]

#modelo 
modelosin6 =lm(IPI~.,data=datasin6filas)
anova(modelosin6)
library(olsrr)
ols_plot_resid_stand(modelosin6)
ols_plot_cooksd_bar(modeloconarreglos)
ols_plot_cooksd_chart(modeloconarreglos)
ols_plot_dfbetas(modelosin6)
ols_plot_dffits(modeloconarreglos)


horizonte = formula(IPI~Antiguedad+Horas+Edad+Presupuesto+Distancia+Satisfaccion_Clima+Nivel+Modalidad+Departamento+Certificacion)
modelosconarreglo2 = lm(IPI~1,data=datasin6filas)
step(modelosconarreglo2, scope = horizonte, direction = "both", trace=0)

subconjuntos6 = ols_step_all_possible(modelosin6)

resultado6filas = subconjuntos6$result
  
# outlieerrs
# balanceo
hii22 <- hatvalues(modelosin6)
p2 <- sum(hii22)
n2 <- nrow(datasin6filas)
2*p2/n2 > 1
which(abs(hii22) > 2*p2/n2)

# leverege
hii22 <- hatvalues(modelosin6)
p2 <- sum(hii22)
n2 <- nrow(datasin6filas)
2*p2/n2
which(abs(hii22) > 2*p2/n2)
c12 <- which(abs(hii22) > 2*p2/n2)

# Residuos Estandarizados
re2 <- rstandard(modelosin6)
which(abs(re2) > 2)
c22 <- which(abs(re2) > 2)

# residuos student
res_stu2 <- rstudent(modelosin6) 
which(abs(res_stu2) > 2)
c32 <- which(abs(res_stu2) > 2)

# distancia cook
di2 <- cooks.distance(modelosin6)
p2 <- sum(hii22)
n2 <- nrow(datasin6filas)
ref12 <- qf(0.5, p2, n2-p2)
which(di2 > ref12)
c52 <- which(di2 > ref12)

# DFFITS 
dfit22 <- dffits(modelosin6)
ref22 <- 2 * sqrt(p2/n2)
which(abs(dfit22) > ref22)
c62 <- which(abs(dfit22) > ref22)

# DFBETAS
dfbe2 <- dfbetas(modelosin6)
ref32 <- 2 / sqrt(n2)
res22 <- abs(dfbe2[, -1]) > ref32
which(apply(res22, 1, sum) > 2) # matriz
c72 <- which(apply(res22, 1, sum) > 2)

# COVRATIO 
covr2 <- covratio(modelosin6)
which(covr2 > 1 + 3*p2/n2 | covr2 < 1 - 3*p2/n2)
c82 <- which(covr2 > 1 + 3*p2/n2 | covr2 < 1 - 3*p2/n2)

# Tabla final
final2 <- c(c12, c22, c32, c62, c72, c82)
tabla_basica2 <- table(final2)
tabla_basica2


# nueva eliminacion de filas ahora  15 filas-------
# c(13, 21, 64, 65, 93, 96, 113, 133, 176)
datossin15filas <- datasin6filas[-c(13, 21, 64, 65, 93, 96, 113, 133, 176), ]

modelosin15 =lm(IPI~.,data=datossin15filas)
horizonte = formula(IPI~Antiguedad+Horas+Edad+Presupuesto+Distancia+Satisfaccion_Clima+Nivel+Modalidad+Departamento+Certificacion)
modelosconarreglo3 = lm(IPI~1,data=datossin15filas)
step(modelosconarreglo3, scope = horizonte, direction = "both", trace=0)

anova(modelosin15)
ols_plot_dffits(modelosin15)
