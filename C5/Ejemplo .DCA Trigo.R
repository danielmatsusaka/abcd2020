### Paquetes necesarios
install.packages("car")
install.packages("broom")
install.packages(gplots)
install.packages("fBasics")
install.packages("agricolae")
install.packages("nlme")
install.packages("lsmeans")
install.packages("ggplot2")
install.packages("rlang")

#### Activación de paquetes
library(broom)
library(car)
library(gplots)
library(fBasics)
library(agricolae)
library(nlme)
library(lsmeans) 
library(ggplot2) 
# Explorando los resultados----
TRIGO <- read.csv("Ejemplo.1.trigo.DCA.csv")
names(TRIGO)#"id"   "trat" "rend"
par(mfrow=c(1,2))
boxplot(TRIGO$rend~TRIGO$trat,xlab="trat",ylab="rendimiento", col="red"  )
plotmeans(rend ~ trat, TRIGO, ccol="red", pch=20, ci.label=TRUE,digits=2, ylim=c(60,180), cex=2)
par(mfrow=c(1,1))
# Ajustando el modelo----
modelo1<-lm(rend~trat, TRIGO)
summary(modelo1)
anova(modelo1)
resultados<-tidy(anova(modelo1))

# Analisis de supuestos ----
#Homogeneidad de varianzas
par(mfrow=c(1,2))
boxplot(residuals(modelo1)~TRIGO$trat, col="wheat2")
plot(residuals(modelo1)~predict(modelo1), col="blue", pch=20, cex=2)
par(mfrow=c(1,1))
leveneTest(TRIGO$rend,TRIGO$trat)

# Normalidad
h<-graph.freq(residuals(modelo1))
normal.freq(h, col="red", cex=2)
qqnormPlot(residuals(modelo1))
shapiro.test(residuals(modelo1))
################################################
# Varianza heterogeneas
modelo2<-gls(rend~trat,weights=varComb(varIdent(form=~1|trat)), TRIGO)
anova(modelo2)
summary(modelo2)

(medias.trat<-aggregate(rend~trat,TRIGO,mean))
(desvio.trat<-aggregate(rend~trat,TRIGO,sd))
(lower = medias.trat$rend - desvio.trat$rend)
(upper=medias.trat$rend + desvio.trat$rend)
dodge <- position_dodge(width=0.8)
p <- ggplot(medias.trat, aes(trat, rend))
p +geom_bar(position = dodge, stat = "identity", fill="tan3") +geom_errorbar(aes(ymin = lower, ymax = upper),size=1,col="blue", position = "dodge", width = 0.8)

# Prueba de comparaciones multiples para gls
(prueba<-lsmeans(modelo2, pairwise ~ trat))
plot(prueba$contrasts)

####################################################
modelo3<-gls(rend~trat, TRIGO)
anova(modelo3, modelo2)

