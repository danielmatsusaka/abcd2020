install.packages("emmeans")
install.packages("Rmisc")

pinus <- read.csv("pinus.csv", sep=";")
View(pinus)

install.packages("psych")

#Anidado
names(pinus) #"fert"    "descomp" "parcela" "plantin" "altura" 
pinus$parcela<-as.factor(pinus$parcela)
pinus$fert<-as.factor(pinus$fert)

#opcion 1. Ignorando parcelas
library(ggplot2)
p <- ggplot(pinus, aes(fert, altura))
p + geom_boxplot(aes(colour=descomp)) + labs(y="Altura total") 

#Graficos de perfiles
(medias.pinus<-aggregate(altura~fert+descomp, pinus,mean))
gp <- ggplot(medias.pinus, aes(x=fert, y=altura, colour=descomp, group=descomp))
gp + geom_line(aes(linetype=descomp), size=.6) +geom_point(aes(shape=descomp), size=3) 

library(psych)
describeBy(pinus$altura,pinus$fert) #estadiaticA descrptiva por nivel del factor fert
tapply(pinus$altura,pinus$fert, mean) #muestra las medias porlos niveles del factor fert de la descripcion anterior

install.packages("plyr")
library(plyr)
estimadores<- ddply(pinus,.(fert,descomp),summarise, rend.medio = mean(altura), desvio=sd(altura), mediana=median(altura), cuatil_25= quantile(altura,.25))
estimadores # con esta funcion pido los estadisticos decriptivos por factor y por variable

m1<-aov(altura~fert*descomp, pinus)#*
summary(m1)  
m1b<-lm(altura~fert*descomp, pinus)#* son dos maneras de pedir lo mismo
anova(m1b)  
#La interaccion da significativa o sea que hay interaccion. 
#pero el análisis es incorrecto, hay seudoreplicación, 
#ya que considera 300 observaciones independientes
#(300 aplicaciones independientes de los tratamientos)


#opcion 2. Considerando efecto aleatorio parcelas en el modelo. Modelo condicional
library(nlme)
m3 <- lme(altura ~ fert*descomp, random= ~1 | parcela, data = pinus)
names(m3)
  
# Supuestos
e<-rstand(m3) # residuos 
re<-residuals(m3, type="pearson")
re<-rstandard(m3) #residuos estandarizados
pre<-predict(m3) #predichos
alfai<-ranef(m3)$'(Intercept)' #efectos aleatorios
par(mfrow = c(1, 3))
plot(pre, re, xlab="Predichos", ylab="Residuos de pearson",main="Gráfico de dispersión de RE vs PRED",cex.main=.8 )
abline(0,0)
abline(2,0)
abline(-2,0)
qqnorm(e, cex.main=.8) #normalidad de los residuos
qqline(e)
qqnorm(alfai, cex.main=.8) #normalidad de los efectos aleatorios
qqline(alfai)
shapiro.test(e)  #normalidad de los residuos
shapiro.test(alfai)  #normalidad de los efectos aleatorios

#Ok supuestos
summary(m3)
anova(m3)
# No hay interaccion! Puedo analizar los efectos de los tratamientos por separado.
# Ambos tratamientos tienen un efecto independiente sobre la altura. Osea que puedo analizar cada factor por tukey por separado
# HAcer estas comparaciones!
# Si hubiera interaccion, no podemos mirar los efectos principales, los efectos de un factor depende de los niveles del otro factor.
#tendría que comparar todas las comparaciones posibles. Serian 15 comparaciones. Tambien podria hacer efectos simples serian 6 comparaciones
#tambien podria hacer efectos simples en el otro sentido, serian 3 contrastes nada mas. DEpende de las preguntas que quiero responder. Tambien se hace por tuckey.

#Comparacion entre dosis de fert:
library(multcomp)
comp<-glht(m3, linfct=mcp(fert ="Tukey")) #chilla prque el modelo contiene interaccion
summary(comp)
cld(comp) #con letras
confint(comp)

#usando emmeans
library(emmeans)
comp1<-emmeans(m3, pairwise ~ fert) #Tukey por default
comp1
confint(comp1) #da los IC para las diferencias de medias
plot(comp1, comparisons = TRUE)
CLD(comp1)

#hay diferencias en los resultados...

#Comparacion entre descompactado 
#(ya se vio por anova que hay dif, 
#pero sirve para obtener los IC para las diferencias de medias
comp2<-emmeans(m3, pairwise ~ descomp) #Tukey por default
comp2
confint(comp2)
plot(comp2, comparisons = TRUE)
CLD(comp2)

#Graficos
library(Rmisc)
resumen <- summarySE(pinus, measurevar="altura", groupvars="fert")
#no sirve porque no respeta la estructura de anidamiento, los EE estan subestimados

#entonces sacamos los EE de las comparaciones del modelo anidado:
resumen_anidado<-as.data.frame(comp1$emmeans)

ggplot(resumen_anidado, aes(x=fert, y=emmean)) + 
  geom_bar(position=position_dodge(), stat="identity", fill="blue") + labs(y="altura de pinus (cm)") +
  labs(x="Dosis de fertilizante (g/cm2)") +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE),                
                width=.2)   +
annotate("text", x = c("0","100","200"), y=100, label = c("A", "B", "AB"))

#comparar con este gráfico, con seudoreplicacion
resumen2 <- summarySE(pinus, measurevar="altura", groupvars="descomp")
ggplot(resumen2, aes(x=descomp, y=altura)) + 
  geom_bar(position=position_dodge(), stat="identity", fill="blue") + labs(y="altura de pinus (cm)") +
  labs(x="Dosis de fertilizante (g/cm2)") +
  geom_errorbar(aes(ymin=altura-se, ymax=altura+se),                
                width=.2)  

#idem para suelo
#entonces sacamos los EE de las comparaciones del modelo anidado:
resumen_anidado2<-as.data.frame(comp2$emmeans)

ggplot(resumen_anidado2, aes(x=descomp, y=emmean)) + 
  geom_bar(position=position_dodge(), stat="identity", fill="blue") + labs(y="altura de pinus (cm)") +
  labs(x="Descompactado de suelo") +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE),                
                width=.2)   +
  annotate("text", x = c("20 cm","50 cm"), y=100, label = c("A", "B"))
