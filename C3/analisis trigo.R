# paquetes a utilizar (instalar y habilitar)
install.packages("readxl")
install.packages("car")
install.packages("ggplot2")
install.packages("plyr")
install.packages("agricolae")


library(readxl)
library(car)
library(ggplot2)
library(plyr)
library(agricolae)


######## trigo #################### ----
# Realice graficos descriptivos de los resultados encontrados
trigo <- read_excel("trigo.xlsx")
names(trigo)
#"inoculante" "bloque"  "fertilizacion" "rendimiento" 
p <- ggplot(trigo, aes (fertilizacion,rendimiento))
p + geom_point(aes(colour = inoculante)) + theme_bw()  + xlab("fertilizante") + ylab("rendimiento") 
p + geom_point() + theme_bw()  + xlab("fertilizante") + ylab("rendimiento") + facet_grid(. ~inoculante)


medias<- ddply(trigo,.(inoculante,fertilizacion),summarise, rend.medio = mean(rendimiento))

p1<-ggplot(medias, aes(x =inoculante, y = rend.medio, colour = fertilizacion)) 
p1+  geom_line(data = medias, aes(y = rend.medio, group = fertilizacion)) + theme_bw()

p2<-ggplot(medias, aes(x =fertilizacion, y = rend.medio, colour = inoculante)) 
p2+  geom_line(data = medias, aes(y = rend.medio, group = inoculante)) + theme_bw()

# Obtenga estimadores puntuales (media, mediana, desvió estándar y cuantiles) para cada tratamiento. ¿Que le indican los resultados obtenidos? 

estimadores<- ddply(trigo,.(inoculante,fertilizacion),summarise, rend.medio = mean(rendimiento), desvio=sd(rendimiento), mediana=median(rendimiento), cuatil_25= quantile(rendimiento,.25))
estimadores

# ¿Qué ventaja les aporta a los investigadores haber realizado un ensayo con los tratamientos en arreglo factorial?
# ¿Se cumplen los supuestos del modelo? Fundamente.

# Efectúe el análisis de varianza y las comparaciones que considere pertinentes.
modelo.trigo<-lm(rendimiento ~ bloque +inoculante + fertilizacion + inoculante:fertilizacion, data=trigo)
anova(modelo.trigo)


leveneTest(rendimiento~inoculante*fertilizacion, data=trigo)
shapiro.test(residuals(modelo.trigo))
plot(modelo.trigo)

interaccion <- with(trigo, interaction(inoculante,fertilizacion))
modelo.3 <- lm(rendimiento ~ inoculante+fertilizacion+interaccion, data=trigo)

comparaciones<-HSD.test(modelo.3, "interaccion", group=TRUE)
comparaciones
plot(comparaciones)


# Concluya. Comente todos los resultados. Efectúe recomendaciones en función de los objetivos planteados. Incluya magnitud del efecto. 

(media_general<-mean(trigo$rendimiento))
(media_tratamientos<- ddply(trigo,.(inoculante,fertilizacion),summarise, rend.medio = mean(rendimiento)))
(media_inoculantes<-ddply(trigo,.(fertilizacion),summarise, rend.medio = mean(rendimiento)))
(media_fertilizantes<-ddply(trigo,.(inoculante),summarise, rend.medio = mean(rendimiento)))
(media_bloque<-ddply(trigo,.(bloque),summarise, rend.medio = mean(rendimiento)))

