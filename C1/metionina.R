# Instalación paquetes adicionales ----
install.packages("fBasics")
library(fBasics)
install.packages("gridExtra")
library(gridExtra)
install.packages("grid")
library(grid)
install.packages("ggplot2")
library(ggplot2)
#lectura de datos ----
metionina <- read.csv("metionina.csv")
names(metionina)# "METIONINA" "CAROTENO"  "PESO"     
str(metionina)
summary(metionina)

#Estadisticos desriptivos ----
mean(metionina$PESO)
median(metionina$PESO)
min(metionina$PESO)
max(metionina$PESO)
quantile(metionina$PESO) # calculates cualquier cuantil(default 0%, 25%, 50%, 75%, 100%)
var(metionina$PESO) # varianza
sd(metionina$PESO) # desvio estandar
summary(metionina$PESO) #(minimo, primer cuartil, mediana, media, tercer cuartil, maximo).

# Tablas----
(tabla1<-basicStats(metionina$PESO))
(tabla1<-round(tabla1,2))
colnames(tabla1)<-c( "peso")
grid.table(tabla1)

# Estadisticos por factores
(tabla2<-aggregate(PESO ~ CAROTENO, data = metionina, mean))
colnames(tabla2)<-c( "nivel de caroteno","peso promedio")
grid.table(tabla2)

(tabla3<-(aggregate(PESO ~ CAROTENO+METIONINA, data = metionina, mean)))
colnames(tabla3)<-c( "nivel de caroteno","nivel metionina","peso promedio")

grid.table(tabla3)

# Graficos
# Graficos de puntos

p1 <- ggplot(metionina, aes(CAROTENO, PESO))
p1 + geom_point(aes(colour = METIONINA), size = 4 )+theme_bw()
p1 +geom_point(colour="blue", size = 4 )+facet_grid(.~METIONINA)+ theme(legend.position="bottom")+theme_bw()

# boxplot
p2 <- ggplot(metionina, aes( , PESO))
p2 + geom_boxplot(aes(PESO))
p3 <- ggplot(metionina, aes(CAROTENO,PESO))
p3 + geom_boxplot( )+theme_bw()
p3+ geom_boxplot(fill= "thistle2")+ facet_grid(.~METIONINA)+theme_bw()

# graficos de barras
(medias.trat<-aggregate(PESO~CAROTENO+METIONINA,metionina,mean))
(desvio.trat<-aggregate(PESO~CAROTENO+METIONINA,metionina,sd))
(lower = medias.trat$PESO - desvio.trat$PESO)
(upper=medias.trat$PESO + desvio.trat$PESO)
dodge <- position_dodge(width=0.8)
p <- ggplot(medias.trat, aes(CAROTENO, PESO, fill = METIONINA))
p +geom_bar(position = dodge, stat = "identity") +geom_errorbar(aes(ymin = lower, ymax = upper),size=1,col="blue", position = "dodge", width = 0.8)

guanacos$campo<-as.factor(guanacos$campo)


#lectura de datos ----
guanacos <- read.csv2("guanacos.csv")
names(guanacos)# "CAMPO" "DIAMETRO"  "MANEJO"     
str(guanacos)
summary(guanacos)

#Estadisticos desriptivos ----
mean(guanacos$diametro)
median(guanacos$diametro)
min(guanacos$diametro)
max(guanacos$diametro)
quantile(guanacos$diametro) # calculates cualquier cuantil(default 0%, 25%, 50%, 75%, 100%)
var(guanacos$diametro) # varianza
sd(guanacos$diametro) # desvio estandar
summary(guanacos$diametro) #(minimo, primer cuartil, mediana, media, tercer cuartil, maximo).

# Tablas----
(tabla1<-basicStats(guanacos$diametro))
(tabla1<-round(tabla1,2))
colnames(tabla1)<-c( "diametro")
grid.table(tabla1)

# Estadisticos por factores
(tabla2<-aggregate(diametro ~ manejo, data = guanacos, mean))
colnames(tabla2)<-c( "manejo","diametro")
grid.table(tabla2)

(tabla3<-(aggregate(diametro ~ manejo+campo, data = guanacos, mean)))
colnames(tabla3)<-c( "manejo","campo","diametro")

grid.table(tabla3)


grid.table(tabla3)

# Graficos
# Graficos de puntos

p1 <- ggplot(guanacos, aes(manejo, diametro))
p1 + geom_point(aes(colour = campo), size = 4 )+theme_bw()
p1 +geom_point(colour="blue", size = 4 )+facet_grid(.~campo)+ theme(legend.position="bottom")+theme_bw()

# boxplot
p2 <- ggplot(guanacos, aes( , diametro))
p2 + geom_boxplot(aes(diametro))
p3 <- ggplot(guanacos, aes(manejo,diametro))
p3 + geom_boxplot( )+theme_bw()
p3+ geom_boxplot(fill= "thistle2")+ facet_grid(.~campo)+theme_bw()

# graficos de barras
(medias.trat<-aggregate(diametro~manejo+campo,guanacos,mean))
(desvio.trat<-aggregate(diametro~manejo+campo,guanacos,sd))
(lower = medias.trat$diametro - desvio.trat$diametro)
(upper=medias.trat$diametro + desvio.trat$diametro)
dodge <- position_dodge(width=0.8)
p <- ggplot(medias.trat, aes(manejo, diametro, fill = campo))
p +geom_bar(position = dodge, stat = "identity") +geom_errorbar(aes(ymin = lower, ymax = upper),size=1,col="blue", position = "dodge", width = 0.8)





