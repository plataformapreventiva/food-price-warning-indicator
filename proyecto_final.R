library("tsbugs")
library("curl")
library("tidyverse")
library("lubridate")
library("stringr")
library("ggplot2")
library("readr")
#library("XML")
#library("xml2")
library("R2OpenBUGS")
library("R2jags")

##### Herramientas ####
prob<-function(x){
  out<-min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

#setwd("./")

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
estados_dic <- read_csv("./data/estados_dic.csv") %>% 
  mutate(NOM_ENT = str_to_lower(NOM_ENT)) %>%
  mutate(CVE_ENT = sprintf("%02d", CVE_ENT)) %>%
  rename(edo_destino=NOM_ENT)


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
  mutate(fecha = ymd (fecha)) %>%
  left_join(estados_dic,by = "edo_destino") %>%
  mutate(CVE_ENT = ifelse(edo_destino== "michoacán", "16", 
                    ifelse(edo_destino== "veracruz","30",
                      ifelse(edo_destino=="df","09",
                             ifelse(edo_destino=="coahuila","05",CVE_ENT)))))


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
#### Análisis econométrico (Seasonal-Trend Decomposition)
##################################################
#
mts <- ts(semantic$precio_promedio, start=c(2001, 1), end=c(2016, 11), frequency=12) 
start(mts)
end(mts)
plot(mts)

#Seasonal Trend Decomposition 
fit = stl(mts, s.window="periodic",t.window=6)
fit = stl(mts, s.window="periodic")
plot(fit)

#The four graphs are the original data, seasonal component, trend component and the remainder 
#and this shows the periodic seasonal pattern extracted out from the original data and the trend 
#that moves around between 47 and 51 degrees Fahrenheit. There is a bar at the right hand side of 
#each graph to allow a relative comparison of the magnitudes of each component. For this data the 
#achange in trend is less than the variation doing to the monthly variation.

#correlation function
acf(mts)
#partial autocorrelations
pacf(mts)


#abline(reg=lm(mts~time(mts)))
#cycle(mts)
#year on year trend
#plot(aggregate(mts,FUN=mean))
#boxplot(mts~cycle(mts))
#acf(log(mts))
#acf(diff(log(mts)))
#pacf(diff(log(mts)))
#(fit <- arima(log(mts), c(0, 1, 1),seasonal = list(order = c(1, 3, 2), period = 12)))
#pred <- predict(fit, n.ahead = 5*12)
#ts.plot(mts,2.718^pred$pred, log = "y", lty = c(1,3))
#abline(h = 0, v =2017, col = "red", lty = 100)
#abline(h = 0, v =2019, col = "red", lty = 100)




##################################################
#### 4. Modelo Serie de tiempo Nacional ####
##################################################
#Leer data
semantic_nacional <- semantic %>% filter(complete.cases(int_price))

n<-nrow(semantic_nacional)

#puedes hacer algunas gráficas exploratorias de la variable de interés
plot(semantic_nacional$precio_promedio,type="l")
hist(semantic_nacional$precio_promedio,freq=FALSE)


#-Definir la estuctura de los datos depediendo del modelo
# Modelo A
data<-list("n"=n,"y"=c(semantic_nacional$precio_promedio[1:(n-6)],rep(NA,6)))
inits<-function(){list(mu=0,tau=1)}
parameters<-c("mu","yf1")
model="A.txt"

#Modelo B 
# AR(1) Model
data<-list("n"=n,"y"=c(semantic_nacional$precio_promedio[1:(n-6)],rep(NA,6)))
inits<-function(){list(mu=0,tau=1)}
parameters<-c("mu","yf1")
model="B.txt"

#Modelo C 
# AR(4) Model
# [Estos Modelos solo autorregresivos son muy malos porque como vimos en el acf
# la variable depende mucho de el primer lag y poco de los demás, para que funcione
# tendría que tener un ponderador en las betas - más importancia en la primera, etc
data<-list("n"=n,"y"=c(semantic_nacional$precio_promedio[1:(n-6)],rep(NA,6)))
inits<-function(){list(mu=0,tau=1,beta=rep(0,4),eta=rep(0,n))}
parameters<-c("beta","mu","yf1")
model = "C.txt"

# Modelo D  [CON COVARIABLES]
#[Modelo dinámico covariable Futuro de precios]
data<-list("n"=n,"y"=c(semantic_nacional$precio_promedio[1:(n-3)],rep(NA,3)),"x"=semantic_nacional$int_price)
#inits<-function(){list(beta=rep(0,n),tau.y=1,tau.b=1,yf1=rep(0,n))}
inits<-function(){list(beta=rep(0,n),tau.y=1,tau.b=1,yf1=rep(0,n),g=0)}
#parameters<-c("beta","tau.y","tau.b","yf1")
parameters<-c("beta","tau.y","tau.b","yf1","g")
model = "D.txt"






if (Sys.info()[['sysname']] == "Darwin") {
  mod.sim <- jags(data,inits,parameters,model.file=model,
                n.iter=5000,n.chains=1,n.burnin=500)
} else {
  mod.sim <- bugs(data,inits,parameters,model.file=model,
                n.iter=5000,n.chains=1,n.burnin=500)
}


#Traza de la cadena
#traceplot(ejA.sim)
out<-mod.sim$sims.list

#sacas las predicciones
out.sum<-mod.sim$summary
#obten subset todas las rows con yf
out.yf<-out.sum[grep("yf",rownames(out.sum)),]

#establece rango del eje y [mean,2.5 y 97.5]
ymin<-min(semantic_nacional$precio_promedio,out.yf[,c(1,3,7)])
ymax<-max(semantic_nacional$precio_promedio,out.yf[,c(1,3,7)])
xmin<-min(semantic_nacional$fecha)
xmax<-max(semantic_nacional$fecha)
par(mfrow=c(1,1))

#t vs y 
#Si estás haciendo un autorregresivo tienes que quitarle observaciones al semantic
plot(semantic_nacional$fecha,semantic_nacional$precio_promedio,type="b",col="grey80",ylim=c(ymin,ymax),xlim=c(xmin,xmax))
lines(semantic_nacional$fecha,out.yf[,3],col=2,lty=2)
lines(semantic_nacional$fecha,out.yf[,7],col=2,lty=2)
lines(semantic_nacional$fecha,out.yf[,1],col=2,lty=2)

#t vs y (usar si es autorregresivo - pierdes observaciones)
#define el orden del autorregresivo
i = nrow(semantic) - nrow(out.yf) -1
plot(tail(semantic_nacional$fecha,-i),tail(semantic_nacional$precio_promedio,-i),type="b",col="grey80",ylim=c(ymin,ymax),xlim=c(xmin,xmax))
lines(tail(semantic_nacional$fecha,-i),out.yf[,3],col=2,lty=2)
lines(tail(semantic_nacional$fecha,-i),out.yf[,7],col=2,lty=2)
lines(tail(semantic_nacional$fecha,-i),out.yf[,1],col=2,lty=2)




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


