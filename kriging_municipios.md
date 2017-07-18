<!-- README.md is generated from README.Rmd. Please edit that file -->
    library(tidyverse)
    library(lubridate)
    library(stringr)

    tipo_cambio <-read_csv("data/tipo_de_cambio.csv") %>%
      mutate(fecha = dmy(fecha)) %>%
      mutate(año = year(fecha))  %>%
      mutate(mes = month(fecha)) %>%
      select(año,mes,tipo_cambio)

    internacional <- read_csv("data/precio_internacional_dolares.csv") %>% 
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

Precios nacionales promedio por estado:
=======================================

    nacional <- read_csv("data/precios_granos_semanales.csv")
    nacional <- select(nacional,producto,precio_min,fecha,edo_destino,obs) %>% 
      mutate(fecha=dmy(fecha)) %>% # tipo de fecha
      mutate(precio_min = ifelse(precio_min > 8, NA, precio_min)) # identificamos un outlier

    estados_dic <- read_csv("data/estados_dic.csv") %>%
      rename(edo_destino=NOM_ENT) %>% 
      mutate(edo_destino = str_to_lower(edo_destino)) %>%
      mutate(CVE_ENT = str_pad(CVE_ENT, 2, pad = "0"))

    maiz_nacional <- read_csv("data/precios_granos_semanales.csv") %>%
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

    nacional <- maiz_nacional %>%
      group_by(fecha,año,mes) %>% 
      summarise(precio_promedio = mean(precio_min, na.rm = TRUE)) %>% 
      left_join(internacional,by = c("fecha", "año", "mes")) 
    nacional_2 <- nacional %>%
      ungroup() %>% as_data_frame() %>%
      select(fecha, precio_promedio, int_price) 
    colnames(nacional_2) <- c("Fecha","Promedio nacional", "Precio internacional")
    nacional_2 <- nacional_2 %>% 
      gather(key = Precios, value = Precio, -Fecha) %>%
      filter(!is.na(Precio))
    ggplot(nacional_2) + 
      geom_line(aes(x = Fecha, y = Precio, color = Precios)) + 
      scale_x_date(date_breaks = "6 months", date_labels = "%b %y") + 
      theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust = 1))

![](images/unnamed-chunk-7-1.png) <!-- 


```r
# Obtener una base tipo panel para cada estado del precio por mes del maiz blanco
semantic2 <- maiz_nacional %>%
  ungroup() %>% as.data.frame() %>%
  select(CVE_ENT,fecha,precio_min) %>%
  group_by(fecha,CVE_ENT) %>%
  summarise(precio_promedio = mean(precio_min,na.rm=TRUE)) %>%
  spread(key = CVE_ENT, value = precio_promedio) %>%
  left_join(nacional%>%ungroup()%>%select(fecha,int_price), estatal, by = c("fecha")) %>%
  arrange(fecha) %>%
  filter(!is.na(int_price)) %>%
  filter(fecha >= ymd("2005-05-01")) %>%
  select(fecha,`01`,`03`:`11`,`13`:`26`,`28`:int_price) %>%
  ungroup %>% as.data.frame() %>%
  gather(key = CVE_ENT, value = precio_promedio, -fecha, -int_price) %>%
  mutate(CVE_ENT = as.integer(CVE_ENT))
```



```r
library(R2jags)
library(gridExtra)
```


El objetivo planteado es hacer un modelo dinámico de segundo orden:

Sea $i$ el tiempo en el que se observa la medición del precio del maíz y $j$ el estado correspondiente a la observación. Para $i=1,2,\ldots,n$ y $j=1,2,\ldots,m$ se tiene que

$$
\begin{aligned}
\mu_{ij} &= \alpha_{i(j)} + \beta_j + \gamma_i f_i, \quad p_{ij} \sim N(\mu_{ij},\tau_{p}), \\
\eta_{i(j)} &= \alpha_{i-1(j)}, \quad \alpha_{i(j)} \sim N(\eta_{i(j)},\tau_\alpha).
\end{aligned}
$$

Las distribuciones iniciales son:
$$
\begin{aligned}
\tau_p &= 1/\sigma_p^2, \\
\sigma_p &\sim \mbox{Gamma}(1,0.01), \\
\beta_j,\gamma_i &\sim N(0,0.001), \\
\tau_\alpha &= 1/\sigma_\alpha^2,\\
\sigma_\alpha &\sim \mbox{Gamma}(1,0.01)
\end{aligned}
$$


Parámetros iniciales:

```r
fn <- data.frame(fn=1:length(unique(semantic2$fecha)), fecha=unique(semantic2$fecha))
semantic <- semantic2 %>% left_join(fn, by = "fecha")
m <- length(unique(semantic$CVE_ENT))
entidad_recode <- data.frame(CVE_ENT=unique(semantic$CVE_ENT),rec = 1:m)
semantic <- semantic %>% left_join(entidad_recode, by = "CVE_ENT")
r <- 6
semantic_obs <- semantic %>% filter(fn <= length(unique(semantic2$fecha)) - r)
n <- length(unique(semantic_obs$fecha))
j1 <- semantic_obs$rec
p <- semantic_obs$precio_promedio
f1 <- unique(semantic_obs$int_price)
fn1 <- semantic_obs$fn
semantic_pred <- semantic %>% filter(fn > n)
semantic_pred_2 <- semantic_pred %>%
  select(fn,int_price) %>%
  distinct(fn,int_price) %>%
  arrange(fn)
grd <- expand.grid(rec = unique(semantic_pred$rec), fn = unique(semantic_pred$fn)) %>%
  left_join(semantic_pred_2, by = "fn")
f2 <- unique(grd$int_price)
j2 <- grd$rec
fn2 <- grd$fn - n
obs <- length(p)
pred <- m*r
data <- list("n" = n, "m" = m, "r" = r,
             "j1" = j1,
             "j2" = j2,
             "p" = p,
             "f1" = f1,
             "f2" = f2,
             "obs" = obs, "pred" = pred,
             "fn1" = fn1,
             "fn2" = fn2)
inits <- function(){list(alpha=rep(0,n),
                         alpha2=rep(0,r),
                         tau.p=0.01,
                         tau.a=0.01,
                         yf1=rep(0,obs),
                         beta=rep(0,m),
                         gamma1=rep(0,n),
                         gamma2=rep(0,r),
                         yf2=rep(0,pred)
                    )}
parameters <- c('alpha','alpha2','beta','gamma1','gamma2','tau.p','tau.a','yf1','yf2')
```


```r
modelo.txt <-
'
model
{
  #Likelihood
  # Space eq
  for(i in 1:obs){
    p[i] ~ dnorm(mu[i],tau.p)
    mu[i] <- alpha[fn1[i]] + beta[j1[i]] + gamma1[fn1[i]] * f1[fn1[i]]
  }
  #state eq
  for(i in 2:n){
    alpha[i] ~ dnorm(mu.a[i],tau.a)
    mu.a[i] <- mu.a[j1[i-1]]
  }
  #priors
  alpha[1] ~ dnorm(mu.a[1],0.001)
  mu.a[1] <- 0
  tau.p ~ dgamma(0.001,0.001)
  tau.a ~ dgamma(0.001,0.001)
  for(i in 1:n){
    gamma1[i] ~ dnorm(0,0.001)
  }
  for(k in 1:m){
    beta[k] ~ dnorm(0,0.001)
  }
  # prediccion 1
  for(i in 1:obs){
    yf1[i] ~ dnorm(mu[i],tau.p)
  }
  #prediccion2
  for(i in 1:pred){
    yf2[i] ~ dnorm(mu2[i],tau.p)
    mu2[i] <- alpha2[fn2[i]] + beta[j2[i]] + gamma2[fn2[i]] * f2[fn2[i]]
  }
  for(i in 2:r){
    alpha2[i] ~ dnorm(mu.a2[i],tau.a)
    mu.a2[i] <-  mu.a2[i-1]
  }
  alpha2[1] ~ dnorm(mu.a2[1],tau.a)
  mu.a2[1] <- alpha[n]
  for(i in 1:r){
    gamma2[i] ~ dnorm(0,0.001)
  }
}
'
cat(modelo.txt, file = 'modelo.bugs')
```




```r
jags_fit <- jags(
  model.file = "modelo.bugs",    # modelo de JAGS
  inits = inits,   # valores iniciales
  data = data,    # lista con los datos
  parameters.to.save = parameters,  # parámetros por guardar
  n.chains = 5,   # número de cadenas
  n.iter = 2500,    # número de pasos
  n.burnin = 250,   # calentamiento de la cadena
  n.thin = 7
)
```

```
## Compiling model graph
##    Resolving undeclared variables
##    Allocating nodes
## Graph information:
##    Observed stochastic nodes: 2857
##    Unobserved stochastic nodes: 3925
##    Total graph size: 17329
## 
## Initializing model
```


Ahora vamos a evaluar los resultados. Lo primero que tenemos que hacer es revisar la convergencia de la cadena. Para esto vemos la siguiente gráfica para la devianza:




![](images/unnamed-chunk-14-1.png)


Se puede ver que la devianza es aproximadamente 3450.

El DIC es 

```r
jags_fit$BUGSoutput$DIC
```

```
## [1] 174703.1
```

Analicemos los precios ajustados para la entidad 1:

![](images/unnamed-chunk-16-1.png)


# Análisis espacial

Esto lo queremos hacer con el fin únicamente de interpolar utilizando un método estadístico simple

-->
