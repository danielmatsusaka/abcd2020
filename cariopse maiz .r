# Ejemplo maiz
cariopse=c(12,15,12,15,9,10,11,10,11,15,12,11,14,10,12,9,14,15,10)
# Estadisticas descriptivas
boxplot(cariopse, main="Box plot para diámetro de cariopse ", col="blue")
summary(cariopse)
sd(cariopse)

# Pruba de hipótesis
prueba=t.test(cariopse, mu=10, alternative = "greater" )
prueba

# Intervalo de confianza
Prueba_interval=t.test(cariopse, mu=10)
Prueba_interval

df<-read.csv(file.choose(),header = T,sep=",",na.strings = c("NA"))
df
boxplot(df$hojasnv,main="HOJAS NV",col ="red" )
summary(df$hojasnv)
sd(df$hojasnv)
summary(df)


(tabla<-basicStats(df$hojasnv))
(tabla1<-round(tabla1,2))
colnames(tabla)<-c( "hojas nv")
grid.table(tabla)


(tabla2<-aggregate(PESO ~ CAROTENO, data = metionina, mean))
colnames(tabla2)<-c( "nivel de caroteno","peso promedio")
grid.table(tabla2)
