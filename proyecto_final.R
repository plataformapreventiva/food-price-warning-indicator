library("tsbugs")
library("curl")
library(tidyverse)
library(lubridate)
library(stringr)
library(ggplot2)
library(readr)
#library(XML)
#library(xml2)
library(R2OpenBUGS)
library(R2jags)

##### Herramientas ####
prob<-function(x){
  out<-min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

#setwd("/home/baco/workspace/food-price-warning-indicator")

##################################################
#### 1. PRECIOS INTERNACIONALES####
##################################################
# tipo de cambio
tipo_cambio <-read_csv("./data/tipo_de_cambio.csv") %>%
  mutate(fecha = dmy(fecha)) %>%
  mutate(año = year(fecha))  %>%
  mutate(mes = month(fecha)) %>%
  select(año,mes,tipo_cambio)

internacional <- read_csv("./data/precio_internacional_dolares.csv") %>% 
  separate(Month, c("mes", "año"), " ")  %>%
  mutate(año = as.numeric(año))  %>%
  filter(año>2000) %>%
  mutate(mes = match(mes,month.abb))  %>%
  mutate(fecha = make_datetime(year=año, month=mes, day=1)) %>%
  mutate(fecha= ymd(fecha)) %>%
  rename(int_price = Price) %>% 
  left_join(tipo_cambio) %>%
  # Obtenemos el precio por kilogramo (De tonelada) por el tipo de cambio
  mutate(int_price = (int_price*tipo_cambio)/1000) %>% 
  select(fecha,año,mes,int_price) 


summary(internacional)
# La base de datos está suficientemente limpia
internacional %>% ggplot() + geom_line(aes(make_date(year=año,month=mes,day=1),int_price)) 



##################################################
#### 2. PRECIOS NACIONALES####
##################################################
#Análisis descriptivo y limpieza de base
nacional <- read_csv("./data/precios_granos_semanales.csv")
problems(nacional)
glimpse(nacional)
summary(nacional)
# EL análisis se hará con precios máxico, que es el precio del kilogramo vendido por tonelada.
nacional <-select(nacional,producto,precio_min,fecha,edo_destino,obs) %>% 
  #definimos variable de fecha
  mutate(fecha=dmy(fecha)) 
  # Observemos más a fondo el precio por kilogramo vendido en bulto
  # identificamos un outlier
quantile(nacional$precio_min,c(.7,.8, .9,.98,.9999,1) ,na.rm = TRUE)
ggplot(nacional) + geom_point(aes(fecha,precio_min))
filter(nacional,precio_min > 15)
# Vemos que ese outlier no tiene observaciones en la columna obs. 
# quitamos esa observación para que no perjudique el promedio nacional

maiz_nacional <- read_csv("./data/precios_granos_semanales.csv") %>%
  select(producto,fecha,edo_destino,precio_min,obs) %>% 
  #definimos variable de fecha
  mutate(fecha=dmy(fecha)) %>%
  #filtramos el outlier
  filter(precio_min < 15) %>%  
  arrange(fecha) %>%
  mutate(mes = month(fecha)) %>% 
  mutate(año = year(fecha)) %>% 
  mutate(fecha = make_datetime(year=año,month=mes,1)) %>% 
  mutate(fecha = ymd (fecha))

ggplot(maiz_nacional) + geom_line(aes(fecha,precio_min,color=edo_destino))




##################################################
#### 3. Construcción Base de datos Semántica####
##################################################
nacional <- maiz_nacional %>%
  group_by(fecha,año,mes) %>% 
  summarise(precio_promedio = mean(precio_min, na.rm = TRUE)) %>% 
  left_join(internacional,by = c("fecha", "año", "mes")) 

nacional %>% ggplot() + geom_line(aes(fecha,precio_promedio),color="green") 
  #+ geom_line(aes(fecha,int_price),colour="blue") +  
  
# Obtener una base tipo panel para cada estado del precio por mes del maiz blanco
estatal <- maiz_nacional %>%
  group_by(edo_destino,fecha,año,mes) %>%
  summarise(precio_promedio = mean(precio_min,na.rm=TRUE)) %>%
  spread(key = edo_destino, value = precio_promedio)


semantic <- left_join(nacional,estatal) 
#semantic %>% View()




##################################################
#### Análisis econométrico [Este no cuenta]
##################################################
#
mts <- ts(semantic$precio_promedio, start=c(2001, 1), end=c(2016, 11), frequency=12) 
start(mts)
end(mts)
plot(mts)
abline(reg=lm(mts~time(mts)))
cycle(mts)
#year on year trend
plot(aggregate(mts,FUN=mean))
boxplot(mts~cycle(mts))
acf(log(mts))
acf(diff(log(mts)))
pacf(diff(log(mts)))
(fit <- arima(log(mts), c(0, 1, 1),seasonal = list(order = c(1, 3, 2), period = 12)))
pred <- predict(fit, n.ahead = 5*12)
ts.plot(mts,2.718^pred$pred, log = "y", lty = c(1,3))
abline(h = 0, v =2017, col = "red", lty = 100)
abline(h = 0, v =2019, col = "red", lty = 100)




##################################################
#### 4. Modelo Serie de tiempo Nacional ####
##################################################
#Leer data
semantic
n<-nrow(semantic)

#puedes hacer algunas gráficas exploratorias de la variable de interés
plot(semantic$precio_promedio,type="l")
hist(semantic$precio_promedio,freq=FALSE)

#-Definir la estuctura de los datos-
data<-list("n"=n,"y"=c(semantic$precio_promedio[1:(n-6)],rep(NA,6)))
inits<-function(){list(mu=0,tau=1)}
parameters<-c("mu","yf1")

ejA.sim<-bugs(data,inits,parameters,model.file="A.txt",
              n.iter=5000,n.chains=1,n.burnin=500)

#Traza de la cadena
#traceplot(ejA.sim)
out<-ejA.sim$sims.list


#sacas las predicciones
out.sum<-ejA.sim$summary
#obten subset todas las rows con yf
out.yf<-out.sum[grep("yf",rownames(out.sum)),]

#establece rango del eje y [mean,2.5 y 97.5]
ymin<-min(semantic$precio_promedio,out.yf[,c(1,3,7)])
ymax<-max(semantic$precio_promedio,out.yf[,c(1,3,7)])
xmin<-min(semantic$fecha)
xmax<-max(semantic$fecha)
par(mfrow=c(1,1))

#t vs y
plot(tail(semantic$fecha,-1),tail(semantic$precio_promedio,-1),type="b",col="grey80",ylim=c(ymin,ymax),xlim=c(xmin,xmax))
lines(tail(semantic$fecha,-1),out.yf[,3],col=2,lty=2)
lines(tail(semantic$fecha,-1),out.yf[,7],col=2,lty=2)
lines(tail(semantic$fecha,-1),out.yf[,1],col=2,lty=2)


##################################################
####¿EXTRA? 5. Modelo Serie de tiempo por Estado ####
##################################################
# No tenemos todos los estados pero podemos hacer el análisis para los que tengamos.
# Podemos correr los mismos modelos de 4. pero separado para cada estado
# ALgunos estados tienen muchos missing values, podemos hacer el modelo con los estados completos y 
# con matching hacer algún argumento sobre por qué tendrían un precio parecido. 

##################################################
####¿EXTRA? 6. Modelo Serie de tiempo por Estado ####
##################################################
# No tenemos todos los estados pero podemos hacer el análisis para los que tengamos.
# ¿Dependencia espacial?


