# librerias a utilizar
library(readxl)
######################################################################
# 1.Leer los datos----
lluvia<- read_excel("lluvia_pampa.xlsx")
names(lluvia)
# [1] "localidad" "año"       "ene"       "feb"       "mar"       "abr"       "may"      
# [8] "jun"       "jul"       "ago"       "sep"       "oct"       "nov"       "dic"      
# [15] "lat"       "long"      "altitud" 
summary(lluvia)

# seleccionar datos ----

# ver el registro que corresponde a la primer localidad en la base de datos
lluvia[1,1]
# ver la columna de las localidades
lluvia[1,]
# ver los valores de la primer fila de la base de datos
lluvia[,1]

#Si se quiere extraer de ahí solamente los elementos de los meses de verano y otoño
verano<-lluvia[,1:6]
otoño<-lluvia[,c(1:2, 7:10)]

# ¿Cuantas estaciones tiene la base de datos?
unique(lluvia$localidad)

# Si solo quiero la localidad MIRAMAR ?
miramar <- lluvia[lluvia$localidad=="MIRAMAR", ]
######################################################################

# la familia apply----
#apply, sapply, lapply, tapply, and mapply.

# Calculos multiples: Simplifica el uso del ciclo "for".

#apply :se usa cuando desea aplicar una función sobre una matriz o data.frame.
# Sumar valores por fila o columna
#¿Cual fue la lluvia maxima que se presento en enero para todas las localidades?
apply(lluvia[3], 2, max) #  2 indica columnas

#¿Cual fue la lluvia maxima que se presento en enero para Miramar?
apply(lluvia[lluvia$localidad=="MIRAMAR",3 ],2,max)

#¿Cual fue la lluvia maxima que se presento en enero para Miramar y "TRES ARROYOS-AERO"?
apply(lluvia[lluvia$localidad=="MIRAMAR" | lluvia$localidad=="TRES ARROYOS-AERO",3 ],2,max)


# Aplique una función a cada celda designada,en función
# de los niveles de ciertos factores.

#¿Cual es la suma de las pp en el mes de enero en cada localidad?
(tabla.1<-tapply(lluvia$ene, lluvia$localidad, sum))


#¿Cual es la suma de las pp en el mes de enero en cada localidad antes de 1970?
(tabla.2<-tapply(lluvia$ene[lluvia$año<1970], lluvia$localidad[lluvia$año<1970], sum))

#¿Cual es la suma de las pp en el mes de enero en cada localidad antes de 1970 localidad:AZUL-AERO?
(tabla.3<-tapply(lluvia$ene[lluvia$año<1970 & lluvia$localidad=="AZUL-AERO"], 
                 lluvia$localidad[lluvia$año<1970 & lluvia$localidad=="AZUL-AERO"], sum))

# usando funciones propias
# Cuentos años tiene cada localidad ?

(años.localidad <- tapply(lluvia$año,lluvia$localidad,  function(x) length(unique(x))))

#¿Cuantas localidades tuvieron más del 40 años de registros
años.localidad[años.localidad >40]

###############uso de for
lluvia<- read_excel("lluvia_pampa.xlsx")
#seleciono el mes
i=3
datos<-cbind(lluvia[1], lluvia[2], lluvia[,i])
# me interesa conocer la tendencia a traves del tiempo para cada localidad
#defino la localidad
#¿que localidad hay?
unique(datos$localidad)
# selecciono una localidad
lugar<-"MIRAMAR"
# defino las variables
precipitacion<-datos[datos$localidad==lugar,3 ]
tiempo<-datos[datos$localidad==lugar,2]
#ajusto el modelo
modelo<-lm(precipitacion~tiempo)
#guardo los resultados
resultados<-summary(modelo)
str(resultados)
resultados$coefficients
#extraigo el valor p
valor.p<-round(resultados$coefficients[2,4],2)
# armo un vector con lo que quiero saber
(tendencia<-c(lugar,colnames(datos)[3],valor.p ))
##########################################################################
# funciones propias
(x<-1:10) # Generamos datos
sd(x)     # Utilizamos la función de R
# Definimos la función
desv = function(x){
  sqrt(var(x))
}    

desv(x)     
       



# armar una función propia
# ¿que es variable?: localidad mes 

tendencia.lluvia=function(lugar, mes){
  datos<-cbind(lluvia[1], lluvia[2], lluvia[,mes])
  precipitacion<-datos[datos$localidad==lugar,3 ]
  tiempo<-datos[datos$localidad==lugar,2]
  #ajusto el modelo
  modelo<-lm(precipitacion~tiempo)
  #guardo los resultados
  resultados<-summary(modelo)
  resultados$coefficients
  #extraigo el valor p
  valor.p<-round(resultados$coefficients[2,4],2)
  # armo un vector con lo que quiero saber
  (tendencia<-data.frame(lugar,colnames(datos)[3],valor.p ))
}

tendencia.lluvia("MIRAMAR",3)

#########################################################################
# Uso del for

lugares<-unique(lluvia$localidad)
meses<-c(3:14)
salida.tendencia<-data.frame()

for (i in lugares){
  for(j in meses){
    calculos<-(tendencia.lluvia(i,j))
    salida.tendencia<-(rbind(salida.tendencia, calculos))
    }
}
salida.tendencia 
summary(salida.tendencia)

significativas<-subset(salida.tendencia, valor.p<0.05)

################################################################################
# Armar una base de datos solo con los registros de los meses de primavera
verano<-lluvia[,11:14]

# ¿Qué largo tiene la serie de datos (¿Cuál es el menor año y cual es el mayor?)
unique(lluvia$año)
# 
 
# ¿Cuántos años de registro tiene la localidad de: "PERGAMINO-INTA" 

pergamino <-(lluvia[lluvia$localidad=="PERGAMINO-INTA" , ])
unique(pergamino$año)
