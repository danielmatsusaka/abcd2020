library(nlme)
library(MASS)
library(lattice)
# Ejemplo de parcelas divididas


data("oats")
names(oats)
?oats
summary(oats)
bwplot(Y~N|V, data=oats,layout=c(3,1),fill="cyan")

# Ajustemos el modelo
Modelo1Oats <- lme( Y ~ V*N, data = oats,  random = ~ 1 | B/V )
anova(Modelo1Oats)
summary(Modelo1Oats)

