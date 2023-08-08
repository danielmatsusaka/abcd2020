install.packages("nlme")
install.packages("multcomp")
install.packages("ggplot2")

library(gplots)
library(multcomp)
library(nlme)
library(lattice)
library(car)
library(fBasics)
### Ejemplo 1####################################################################################
#windows()
data.bloques <- read.delim("bloques.txt")
summary(data.bloques)
names(data.bloques)#[1] "rend"   "var"    "bloque"
plotmeans(rend ~ var, data.bloques, ccol="red", pch=20, ci.label=TRUE,digits=2, ylim=c(60,90), cex=2)
par(mfrow=c(1,2))
boxplot(rend~bloque, data.bloques, col="red", ylab="rendimiento", xlab="bloques")
boxplot(rend~var, data.bloques, col="red", ylab="rendimiento", xlab="variedades")
par(mfrow=c(1,1))

modelo.01<-lme(rend ~ 1+var ,random= ~1|bloque , data.bloques)
summary(modelo.01)
anova(modelo.01)
###########SUPUESTOS#####################################
# Analisis de supuestos ----
#Homogeneidad de varianzas
par(mfrow=c(1,2))
boxplot(residuals(modelo.01)~data.bloques$var, col="wheat2")
plot(residuals(modelo.01)~predict(modelo.01), col="blue", pch=20, cex=2)
par(mfrow=c(1,1))
leveneTest(data.bloques$rend,data.bloques$var)

# Normalidad
h<-graph.freq(residuals(modelo.01))
normal.freq(h, col="red", cex=2)
qqnormPlot(residuals(modelo.01))
shapiro.test(residuals(modelo.01))

### Analisis de varianza

anova(modelo.01)
(IC<-confint( glht (modelo.01, mcp(var="Tukey") )))
plot(IC)
#########################################################################################################

