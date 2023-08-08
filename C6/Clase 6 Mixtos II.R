#Anidado
semillas <- read.csv("semillas")
names(semillas)
semillas$potrero<-as.factor(semillas$potrero)

#opcion 1. Ignorando potreros
library(ggplot2)
p <- ggplot(semillas, aes(tratamiento, biomasa))
p + geom_boxplot() + labs(y="Biomasa de semillas (g/m2)") 

library(psych)
describeBy(semillas$biomasa,semillas$tratamiento)
tapply(semillas$biomasa,semillas$tratamiento, mean)

m1<-aov(biomasa~tratamiento, semillas)
summary(m1)  
m1b<-lm(biomasa~tratamiento, semillas)
anova(m1b)  

#opcion 2. promediando VR por potrero
ggplot(semillas, aes(tratamiento, biomasa, fill=interaction(tratamiento, potrero), dodge=potrero)) +  geom_boxplot() + labs(y="Biomasa de semillas (g/m2)") 
#promediando biomasa por potrero
(medias.potrero<-aggregate(biomasa~tratamiento+potrero, semillas,mean))
describeBy(medias.potrero$biomasa,medias.potrero$tratamiento)

m2<-aov(biomasa~tratamiento, medias.potrero)
summary(m2)
m2b<-lm(biomasa~tratamiento, medias.potrero)
anova(m2b)

#opcion 3. Considerando potreros en el modelo. Modelo condicional
library(nlme)
m3 <- lme(biomasa ~ tratamiento, random= ~1 | potrero, data = semillas)
summary(m3)
anova(m3)

#opcion 3b. idem anterior, pero usando nlme
library(lme4)
m3b<- lmer(biomasa ~ tratamiento + (1|potrero), semillas)
summary(m3b)
anova(m3b)

#Nos quedamos con m3. Supuestos
e<-resid(m3) # residuos de pearson
pre<-predict(m3) #predichos
alfai<-ranef(m3)$'(Intercept)' #efectos aleatorios
par(mfrow = c(1, 3))
plot(pre, e, xlab="Predichos", ylab="Residuos de pearson",main="Gráfico de dispersión de RE vs PRED",cex.main=.8 )
abline(0,0)
qqnorm(e, cex.main=.8)
qqline(e)
qqnorm(alfai, cex.main=.8)
qqline(alfai)
shapiro.test(e)
shapiro.test(alfai)

#Parte fija
fitted(m3)  #predicciones parte fija + aleatoria
fixef(m3) #estimacion parametros efectos fijos
X<-model.matrix(m3)
pred_fija<-X %*% fixef(m3)#predicciones parte fija 

library(multcomp)
comp<-glht(m3, linfct=mcp(tratamiento ="Tukey"))
summary(comp)
cld(comp) #con letras

#usando emmeans
library(emmeans)
emmeans(m3, pairwise ~ tratamiento) #Tukey por default

medias.potrero  #un dato por potrero
library(Rmisc)
resumen <- summarySE(medias.potrero, measurevar="biomasa", groupvars="tratamiento")

ggplot(resumen, aes(x=tratamiento, y=biomasa)) + 
  geom_bar(position=position_dodge(), stat="identity", fill="blue") + labs(y="Biomasa de semillas (g/m2)") +
  geom_errorbar(aes(ymin=biomasa-se, ymax=biomasa+se),                
                  width=.2)    # ancho de la barra de error

#Parte aleatoria
library(lattice)
dotplot(ranef(m3b, condVar = TRUE))

##############################
#Anidado con dos efectos aleatorios
cedro <- read.csv2("cedro.csv")
BD<-cedro

names (BD)
BD<-BD[,1:3] #nos quedamos solo con largo
head(BD)
str(BD)
summary(BD)

#medias
(medias.poblacion<-aggregate(largo~poblacion, BD,mean))
(medias.familia<-aggregate(largo~poblacion+familia, BD,mean))

#modelo anidado
m4<- lmer(largo ~ 1 + (1|poblacion/familia), BD)
m4b<-lme(largo ~ 1, random = ~ 1|poblacion/familia, BD)

summary(m4)
library(lattice)
dotplot(ranef(m4, condVar = TRUE))

#Significacion de efectos aeatorios
m5<- lmer(largo ~ 1 + (1|poblacion), BD)
anova(m5,m4)
AIC(m5, m4)

#Predichos
F0 <- fitted(m5, level = 0) #poblacional
F1 <- fitted(m5, level = 1) #condicional a cada familia
a<-cbind(F0,F1)
a


