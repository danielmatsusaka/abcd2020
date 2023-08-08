
library(ggplot2)
library(lsmeans)
library(agricolae)
library(nlme)
#### n de 120 , tratamiento tienes dos niveles sexo dos y estacion dos (inv y ver), es decir 4  (2x2)
# nos interessa ver promedios de cada uno de los 4 y ver si existe una diferencia de cada unos de los 4 en proteinas
# varaible respuesta tproteina es la Y ,pt=u +xi( el sexo 1,2)+ bj(estacion i 1,2+xbj ( interacion )+ Aij+Eij~N(0,R2) [decimos que la varianza es constante] INDEPENDENCIA)
# gls modela las varianzas 

# estacion y sexo son fijos

 # Explorando los resultados----
datos <- read.csv2("atriplex v2.csv")
datos <- read.csv2("atriplex.csv")
 names(datos)# "sexo"                   "estacion"               "Concentración_proteica"
 
 p <- ggplot(datos, aes (sexo,proteina))
 p + geom_point() + xlab("variedades") + ylab("Concentración_proteica")  + theme_bw() + facet_grid(. ~estacion)
 p + geom_boxplot() + geom_jitter(colour="red")+ theme_bw() + facet_grid(. ~estacion)
 
 library(psych)
 describeBy(datos$proteina,list(datos$sexo,datos$estacion))
 
 # Prueba de comparaciones multiples para gls
 
 # Ajustando el modelo----
 modelo1<-lm(proteina~sexo*estacion, datos)
 summary(modelo1)
 anova(modelo1)
plot(modelo1)
 
 # Analisis de supuestos ----
 #Homogeneidad de varianzas
 datos$residuales<- residuals(modelo1)
 p1 <- ggplot(datos, aes (sexo,residuales))
 p1 + geom_point() + xlab("variedades") + ylab("residuales")  + theme_bw() + facet_grid(. ~estacion)
 p1 + geom_boxplot() + geom_jitter(colour="green")+ theme_bw() + facet_grid(. ~estacion)
 
 
 # Normalidad
 h<-graph.freq(residuals(modelo1))
 normal.freq(h, col="red", cex=2)
  shapiro.test(residuals(modelo1))
 ################################################
 # Varianza heterogeneas
 modelo2<-gls(proteina~sexo*estacion,weights=varComb(varIdent(form=~1|estacion)), datos)
 plot(modelo2)
  anova(modelo2)
 summary(modelo2)
plot(modelo2)
shapiro.test(residuals(modelo2))
 modelo3<-gls(proteina~sexo*estacion,weights=varExp(), datos)
 AIC(modelo2, modelo3)
 
 names(modelo1)
 # Prueba de comparaciones multiples para gls
 interaccion <- with(datos, interaction(sexo:estacion))
 modelo4<- modelo2<-gls(proteina~interaccion,weights=varComb(varIdent(form=~1|estacion)), datos)
 (prueba<-lsmeans(modelo4, pairwise ~ interaccion))
 plot(prueba$contrasts)
 # te quedas con el modelo 2 es mas facil de interpretar , ve a variance funcion  0.45  ,te dice la mitad de la vairanza , la mitad de la varianza del invierno es igual a la del verano .
 
 # el varexp es una varianza ~ e ala 0.058 
 # el varcomb es para combinar fucniones 
 
 (medias.trat<-aggregate(proteina~interaccion,datos,mean))
 (desvio.trat<-aggregate(proteina~interaccion,datos,sd))
 (lower = medias.trat$proteina - desvio.trat$proteina)
 (upper=medias.trat$proteina + desvio.trat$proteina)
 dodge <- position_dodge(width=0.8)
 p <- ggplot(medias.trat, aes(interaccion, proteina))
 p +geom_bar(position = dodge, stat = "identity", fill="tan3") +geom_errorbar(aes(ymin = lower, ymax = upper),size=1,col="blue", position = "dodge", width = 0.8)
 
  ####################################################
 modelo1<-gls(proteina~sexo*estacion, datos)
 modelo2<-gls(proteina~sexo*estacion,weights=varComb(varIdent(form=~1|estacion)), datos)
 
 anova(modelo1, modelo2)
AIC(modelo2,modelo3,modelo4)
anova(modelo2,modelo3, test ="F" )

modelo5<-gls(proteina~sexo*estacion,weights=varComb(varIdent(form=~1|sexo)), datos)
plot(modelo5)
anova(modelo5)
summary(modelo5)
## winnre model 2 
e<-shapiro.test(residuals(modelo2))
re_1<-shapiro.test(residuals(modelo2,type ="pearson" ))
e<-residuals(modelo2)
re<-residuals(modelo2,type = "pearson")
shapiro.test(re)
residuos<-cbind(e,re)
names(modelo2)
anova(modelo2)

interaccion <- with(datos, interaction(sexo:estacion))
modelo7<- modelo2<-gls(proteina~interaccion,weights=varComb(varIdent(form=~1|estacion)), datos)
(prueba<-lsmeans(modelo4, pairwise ~ interaccion))
plot(prueba$contrasts)

library(emmeans)
library(multcompView)
comp1<-emmeans(modelo2,pairwise~sexo*estacion)
plot(comp1,comparisons=TRUE)
#SE ANALIZO POR UN ANOVA DE DOS FACTORES ,se analizo el completmento de supuesto por shapirto y leeneven par a la varianza
# cuando se detecto heetereticidad se aplicao (varexp ,var power...)la funcion varided para el modela de varianza, se compararon disitntas estruccturas de varianzas y se escogio el mejor valor 3 aic 
# para comparar entre medias se efectuaron mediante tukey,se utilizo un nivel de siginifacion del 5%
# SE DECTECTO INTERACCION ENTRE SEXO Y ESTACIONES mediante el anova te das cuenta
# tods los analises se efecturon usando R 
# TODOS CONTRA TODOS 
# EFE SIMPLES /ESTACION
#EFECTOS SIMPLE/SEXO

# clase 7

# asociacion entre el tamalo de la celda de panales de abeja y la prevalencia del ectoracido varoa destructor
# vr:presencia o ausecia  en cada celdad   bernoulli
# ve  ancho de lacelda cuantitativa 
#glm(paracitos~celdas(bo+b1ancho)),family= binomial)
#table(abejas$paaracitos)
# variable paracitos esperanza (vr)= pi  ,56% de probabilidad
# logit(pi)=es el logarit la probabilidad de exitos entre la porbailidad de fracaso es coeficiente es Inodds
# el nacho de la celda y la probablidad logit es siginifactiva ancho es 2.217
#p es oodds # e a la logaritdmo es el odds retio e ala b es igual a e ala 2.2175 es igual a 9.2 
#esto me dice q  por cada mm de celda me dice cuanto aumenta el odds esta paracitado en este caso 9.2
#aic y devianza mira siempre .
#bolker para consultas  modelo mixtos 