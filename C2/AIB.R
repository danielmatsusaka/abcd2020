# Instalación de los paquetes necesarios
install.packages("car")
install.packages("gplots")
install.packages("agricolae")
install.packages("fBasics")

#activación de las librerias
library(gplots)
library(car)
library(agricolae)
library(fBasics)
# Explorando los resultados----
AIB <- read.csv("AIB.csv")
names(AIB)#"dosis"    "longitud"
boxplot(AIB$longitud~AIB$dosis,xlab="dosis",ylab="longitud(cm)", col="red"  )
plotmeans(longitud ~ dosis, AIB, ccol="red", pch=20, ci.label=TRUE,digits=2, ylim=c(0,7), cex=2)

# Ajustando el modelo----
modelo1<-lm(longitud~dosis, AIB)
summary(modelo1)
anova(modelo1)

# Analisis de supuestos ----
(e<-residuals(modelo1))
(pred<-predict(modelo1))
hist(e)
boxplot(e)
qqnormPlot(e) 
shapiro.test(e)
RE<-rstudent(modelo1)
PRED<-fitted(modelo1)
plot(factor(PRED),RE)

par(mfrow=c(2,2))

plot(modelo1)
par(mfrow=c(1,1))

leveneTest(AIB$longitud,AIB$dosis)
################################################


