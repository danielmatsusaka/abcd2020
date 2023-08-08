#  Regresion 

# Ejemplo alfalfa

dosisP<-c(0,20,40,60,80,100)
MS<-c(10,10.6,14.4,15.8,15.3,20.2)


plot(dosisP,MS,xlab="dosis de P (kg/ha)",ylab="MS (ton/ha)", col="red", pch=16, main="Relación materia seca fertilización fosforada") 

modelo<- lm(MS~dosisP)
summary(modelo)

plot(dosisP,MS,xlab="dosis de P (kg/ha)",ylab="MS (ton/ha)", col="red", pch=16, main="Relación materia seca fertilización fosforada")
abline(modelo)

round(confint(modelo),2)

predict(modelo)
round((pred.w.clim <- predict(modelo, interval="confidence")),2)
new <- data.frame(x =c(0,20,40,60,80,100))
round((pred.w.plim <- predict(modelo, new, interval="prediction")),2)

matplot(new$x,cbind(pred.w.clim, pred.w.plim[,-1]), lty=c(1,2,2,3,3), col=1, type="l", ylab="predichos",xlab="dosis P")
matpoints(new$x, MS, pch=20)

anova(modelo)

summary(modelo)$r.squared 

influence(modelo)# hat, coeficcient, sigma .wt.res
mean(influence(modelo)$hat)
cooks.distance(modelo)

plot(modelo )
    