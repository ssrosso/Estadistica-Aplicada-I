set.seed(5)

samples <- 50                         #Cantidad de muestras
obs <- 100
a <- 1                                #alfa
b <- (33+71)/2                        #beta
maximos <- rep(NA,samples)

X = replicate(samples,rgamma(n = obs, shape = a, scale = b), simplify = FALSE)
for(i in 1:samples){
  maximos[i] = max(X[[i]])
}

#hist(maximos, main = "Histograma", 
#     xlab = "Máximos Observados", ylab = "Frecuencia",
#     col = "cyan", breaks = 10)
#abline(v=mean(maximos),col="red")

hist(maximos, main = "Histograma de densidades", 
     xlab = "Máximos Observados", ylab = "Densidad",
     col = "cyan", breaks = 10, freq=FALSE)
abline(v=mean(maximos),col="red")
lines(density(maximos))

boxplot(maximos, main = "Diagrama de Cajas y Bigotes",
       xlab = "Máximos Observados",
       col = "orange", horizontal = TRUE)
abline(v=mean(maximos),col="red")  

#Algunos calculos con los maximos hallados que seran utilizados en el Excel
#mean(maximos)
media <- sum(maximos)/50
#var(maximos)
varianza <- (1/49)*sum((maximos-mean(maximos))^2)
#sd(maximos)
desvio_muestral <- varianza^(1/2)
