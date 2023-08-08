
library(lattice)
RAMAS <- read.csv("ramae2.csv", sep=";", dec=",")
names(RAMAS)

#################################################################################################
#Objetivo: ajustar un modelo de biomasa de ramas (ramas) en función del diametro (d), 
#bajos distintos tratamientos 
##################################################################################################

# Grafico los datos en crudo
xyplot(ramas ~ d|trat, data=RAMAS, cex=2, cex.lab=2, type=c("p","r"))
xyplot(log(ramas) ~ d|trat, data=RAMAS, cex=2, cex.lab=2, type=c("p","r"))
xyplot(sqrt(ramas) ~ d|trat, data=RAMAS, cex=2, cex.lab=2, type=c("p","r"))


# Uso la transformación X^2 para linealizar los datos
RAMAS$rt<-sqrt(RAMAS$ramas)

# ajusto un modelos lineal simple con una unica pendiente y ordenada al origen
tmp <- lm(rt ~ d, data=RAMAS)
plot(RAMAS$d, RAMAS$rt, xlab = "Diametro", ylab = "Biomasa de ramas", col=RAMAS$trat)
abline(tmp, col="blue")
summary(tmp)
anova(tmp)


# ajusto un modelos lineal simple teniendo en cuenta el tratamiento

tmp2<- lm(rt ~ d*trat, data=RAMAS)
summary(tmp2)
anova(tmp2)

(predicho_poda= (-5.0599 -0) + (8.62446)*RAMAS$d)
(predichoT_sin.poda= (-5.0599 - 0.3031) + (8.66292 + 0.9128)*RAMAS$d )

plot(RAMAS$d, RAMAS$rt, col=RAMAS$trat, ylab= "sqrt(Biomasa de ramas)", xlab="diámetro a la altura del pecho")
abline(-5.0599, 8.62446, col= "red", lwd=6)
abline(-5.363, 9.57572, col="blue", lwd=6)




