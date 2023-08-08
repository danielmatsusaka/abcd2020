# librerias a utilizar
library(readxl)
library(fBasics)
######################################################################
# simulaciones
set.seed(100)
data<-rnorm(10000000, mean = 0, sd = 1)
mean(data)
sd(data)dnorm(1, mean = 0, sd = 1)
plot(density(data), main="función de densidad Normal (0,1)")

#####################################################################
# Modifique los valores de N y p

  n=50 # numero de simulaciones
  N=25 # tamaño de muestra
  p=0.05 # probabilidad de exito
  
  (x <- rbinom(n,N,p))
  
  qqnormPlot(x,title = F, main=paste("QQ-plot,N=",N, "p=",p)) 
  shapiro.test(x)


