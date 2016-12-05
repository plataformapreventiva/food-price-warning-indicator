library(rstan)
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
calculateMASE <- function(f,y) { # f = vector with forecasts, y = vector with actuals
#FORECAST ACCURACY:  as described by Hyndman et al in "Another look at measures of forecast accuracy" (2006).
  if(length(f)!=length(y)){ stop("Vector length is not equal") }
  n <- length(f)
  return(mean(abs((y - f) / ((1/(n-1)) * sum(abs(y[2:n]-y[1:n-1]))))))
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

estados_dic<- read_csv("./data/estados_dic.csv") %>%
  rename(edo_destino=NOM_ENT) %>% 
  mutate(edo_destino = str_to_lower(edo_destino)) %>%
  mutate(CVE_ENT = str_pad(CVE_ENT, 2, pad = "0"))
  

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
  
nacional %>% ggplot() + geom_line(aes(fecha,precio_promedio),color="green") + geom_line(aes(fecha,int_price),colour="blue") 
  

# Obtener una base tipo panel para cada estado del precio por mes del maiz blanco
estatal <- maiz_nacional %>%
  #group_by(CVE_ENT,fecha,año,mes) %>%
  group_by(edo_destino,fecha,año,mes) %>%
  summarise(precio_promedio = mean(precio_min,na.rm=TRUE)) %>%
  #spread(key = CVE_ENT, value = precio_promedio)
  spread(key = edo_destino, value = precio_promedio)


semantic <- left_join(nacional,estatal) 

# X_IPA
semantic$lag_3 = lag(semantic$aguascalientes,3)
semantic$lag_12 = lag(semantic$aguascalientes,12)
semantic<-mutate(semantic, CQGR=(aguascalientes/lag_3)^(1/3)-1)
semantic<-mutate(semantic, CAGR=(aguascalientes/lag_12)^(1/12)-1)

temp_Q <- semantic %>% group_by(mes) %>% summarise(CQGR_month_mean = mean(CQGR,na.rm=TRUE),
                                                   CQGR_month_std = sd(CQGR,na.rm=TRUE))
temp_A <- semantic %>% group_by(año) %>% summarise(CAGR_year_mean = mean(CAGR,na.rm=TRUE),
                                                   CAGR_year_std = sd(CAGR,na.rm=TRUE))

IPA_Q <- semantic %>% left_join(temp_Q) %>% mutate(IPA_Q = (CQGR -CQGR_month_mean)/CQGR_month_std) 
IPA_A <- semantic %>% left_join(temp_A) %>% mutate(IPA_A = (CAGR -CAGR_year_mean)/CAGR_year_std) 
IPA_Q %>% write.csv("IPA_Q.csv")
IPA_Q <- read_csv("IPA_Q.csv")
IPA_Q %>% ggplot() + geom_bar(aes(fecha,abs(IPA_Q)),stat="identity") + 
  geom_line(aes(fecha,precio_promedio),color="green") + 
  geom_hline(yintercept = .5,col="yellow") +
  geom_hline(yintercept = 1,col="red")


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
semantic_nacional$int_price_lag <- lag(semantic_nacional$int_price)
semantic_nacional <- semantic_nacional %>% filter(complete.cases(int_price_lag))

n<-nrow(semantic_nacional)

#puedes hacer algunas gráficas exploratorias de la variable de interés
plot(semantic_nacional$precio_promedio,type="l")
#hist(semantic_nacional$precio_promedio,freq=FALSE)




##################################################
#### STAN (Seasonal-Trend Decomposition)
##################################################


dat <- semantic_nacional 
semantic_nacional<- mutate(semantic_nacional,l_int_price=dplyr::lag(int_price,n=1))

# Modelo STAN  [Time series with seasonality]
# La temporada dura 12 meses
dat<-list(N=n,y=semantic_nacional$precio_promedio,x1=semantic_nacional$int_price)
fit2<-stan(file='hb_ts2.stan',data=dat,iter=1000,chains=1)


#stan code
fit2.smp <- rstan::extract(fit2, permuted = TRUE)
dens2_a<-density(fit2.smp$a)
dens2_d<-density(fit2.smp$d)
a_est2<-dens2_a$x[dens2_a$y==max(dens2_a$y)]
d_est2<-dens2_d$x[dens2_d$y==max(dens2_d$y)]

trend_est2<-rep(0,n)

for (i in 1:n) {
  tmp<-density(fit2.smp$trend[,i])
  trend_est2[i]<-tmp$x[tmp$y==max(tmp$y)]
}

week_est2<-rep(0,n)

for (i in 1:n) {
  tmp<-density(fit2.smp$season[,i])
  week_est2[i]<-tmp$x[tmp$y==max(tmp$y)] 
}

pred2<-a_est2*semantic_nacional$int_price+d_est2+cumsum(trend_est2)+week_est2

matplot(cbind(semantic_nacional$precio_promedio,pred2),type='l',lty=1,lwd=c(2,3),col=c(1,2))

legend('topleft',c('Data','Predicted'),col=c(1,2),lty=1,lwd=c(2,3),cex=1.5,ncol=2)
cor(semantic_nacional$precio_promedio,pred2)
plot(week_est2,type='l')
plot(trend_est2,type='l')

matplot(
  cbind(semantic_nacional$precio_promedio,
        pred2,
        cumsum(trend_est2),
        week_est2+cumsum(trend_est2)),
  xlab="Fecha",ylab="Precio por kilo",
  type='l',lty=1,lwd=c(2,3,2,2),col=c('black','red','blue','green'))
  legend('topleft',c('Data','Predicted','Seasonality + Trend','Trend'),col=c('black','red','blue','green'),lty=1,lwd=c(2,3,2,2),cex=.8,ncol=2)



#-Definir la estuctura de los datos depediendo del modelo

  # Modelo Autorregresivo
  # AR(1) Model Autoregressive AR(1) time series models
  # data<-list("n"=n,"y"=c(semantic_nacional$precio_promedio[1:(n-3)],rep(NA,3)))
  # inits<-function(){list(tau=1)}
  # parameters<-c("yf1")
  # model="A.txt"

cor(semantic_nacional$precio_promedio,semantic_nacional$int_price)


accuracy(mts, pred2)


# Modelo A  [Modelo estático]
semantic_nacional$t <- 1:n
data<-list("n"=n,"y"=c(semantic_nacional$precio_promedio[1:(n-3)],rep(NA,3)),"x"=semantic_nacional$int_price,"t"=semantic_nacional$t/max(semantic_nacional$t))
inits<-function(){list(beta=rep(0,5),tau=1,yf1=rep(1,n))}
parameters<-c("beta","tau","yf1")
model = "A.txt"


# Modelo B  [Modelo Dinámico sin covariable]
#Le hicimos un suavizamiento al modelo metiendo una lambda en la varianza de las betas
#tau.b<- lam * tau.y suvizamos más
data<-list("n"=n,"y"=c(semantic_nacional$precio_promedio[1:(n-6)],rep(NA,6)))
#inits<-function(){list(beta=rep(0,n),tau.y=1,tau.b=1,yf1=rep(0,n))}
inits<-function(){list(beta=0,tau.y=1,yf1=rep(0,n))}
#parameters<-c("beta","tau.y","tau.b","yf1")
parameters<-c("beta","tau.y","tau.b","yf1")
model = "B.txt"


# Modelo C  [Modelo Dinámico con covariables]
data<-list("n"=n,"y"=c(semantic_nacional$precio_promedio[1:(n-3)],rep(NA,3)),"x1"=semantic_nacional$int_price)
inits<-function(){list(alpha=0,beta=rep(0,n),tau=1,yf1=rep(1,n))}
parameters<-c("alpha","beta","tau","yf1")
model = "C.txt"




if (Sys.info()[['sysname']] == "Darwin") {
  mod.sim <- jags(data,inits,parameters,model.file=model,
                  n.iter=5000,n.chains=1,n.burnin=500)
  out<-mod.sim$BUGSoutput$sims.list
  out.sum<-mod.sim$BUGSoutput$summary
} else {
  mod.sim <- bugs(data,inits,parameters,model.file=model,
                  n.iter=5000,n.chains=1,n.burnin=500)
  out<-mod.sim$sims.list
  out.sum<-mod.sim$summary
}



#Traza de la cadena
#traceplot(mod.sim)

  #Graficas el comportamiento de la variable 
  #z<-out$mu #Variable a Analizar
  #Define el espacio de la gráfica
#par(mfrow=c(2,2))
  #plotea la cadena
# plot(z,type="l")
  #ve si en el tiempo converge
# plot(cumsum(z)/(1:length(z)),type="l")
  #histograma - ver media y varianza
# hist(z,freq=FALSE)
  #correlación con los lags
  #Auto- and Cross- Covariance and -Correlation Function Estimation
  #acf(z)


#out<-mod.sim$sims.list

#sacas las predicciones
#out.sum<-mod.sim$summary
#obten subset todas las rows con yf
out.yf<-out.sum[grep("yf",rownames(out.sum)),]
out.mu<-out.sum[grep("mu.b",rownames(out.sum)),]
out.beta<-out.sum[grep("beta",rownames(out.sum)),]

#establece rango del eje y [mean,2.5 y 97.5]
ymin<-min(semantic_nacional$precio_promedio,out.yf[,c(1,3,7)])
ymax<-max(semantic_nacional$precio_promedio,out.yf[,c(1,3,7)])
xmin<-min(semantic_nacional$fecha)
xmax<-max(semantic_nacional$fecha)
par(mfrow=c(1,1))

#t vs y 
#Si estás haciendo un autorregresivo tienes que quitarle observaciones al semantic
plot(semantic_nacional$fecha,semantic_nacional$precio_promedio,type="b",col="grey80",xlab="Fecha",ylab="Precio por kilo",ylim=c(ymin,ymax),xlim=c(xmin,xmax))
lines(semantic_nacional$fecha,out.yf[,3],col=2,lty=2)
lines(semantic_nacional$fecha,out.yf[,7],col=2,lty=2)
lines(semantic_nacional$fecha,out.yf[,1],col=2,lty=2)

#t vs y (usar si es autorregresivo - pierdes observaciones)
#define el orden del autorregresivo
i = nrow(semantic_nacional) - nrow(out.yf) 
plot(tail(semantic_nacional$fecha,-i),tail(semantic_nacional$precio_promedio,-i),type="b",col="grey80",ylim=c(ymin,ymax),xlim=c(xmin,xmax))
lines(tail(semantic_nacional$fecha,-i),out.yf[,3],col=2,lty=2)
lines(tail(semantic_nacional$fecha,-i),out.yf[,7],col=2,lty=2)
lines(tail(semantic_nacional$fecha,-i),out.yf[,1],col=2,lty=2)


out.dic<-mod.sim$DIC

calculateMASE(tail(semantic_nacional$precio_promedio,6),tail(out.yf[,3],6))

#Another look at measures of forecast accuracy
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






