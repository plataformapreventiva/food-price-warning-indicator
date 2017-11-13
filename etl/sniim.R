library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)

url_base <- 'http://www.economia-sniim.gob.mx/2010prueba/GranosMes.asp?Cons=M&prod=606&dest=T&dqMesMes=%s&dqAnioMes=%s&Formato=Nor&submit=Ver+Resultados'

get_corn_prices <- function(i){
  anio <- fechas$anio[i]
  mes <- fechas$mes[i]
  url <- sprintf(url_base,mes,anio)
  
  data <- read_html(url) %>%
    html_nodes(xpath = '//*[(@id = "Datos")]') %>%
    html_table(fill = T)
  if(length(data) > 0){
    dat <- data[[1]]
    dat <- dat[-(1:5), ]
    if(sum(is.na(dat$X8)) == length(dat$X8)){
      dat$X8 <- NULL
    }
    dat <- dat[, c(1,2,length(dat))]
    names(dat) <- c("destino", "origen", "precio")
    dat[dat$destino=="", "destino"] <- NA
    dat <- dat %>% fill(destino)
    dat$anio <- anio
    dat$mes <- mes
  }else{
    dat <- data.frame(destino=NULL,origen=NULL,precio=NULL,anio=NULL,mes=NULL)
  }
  dat
}

anios <- 2000:2017
meses <- 1:12
fechas <- expand.grid(anio=anios, mes=meses)
maiz_precios <- map_df(.x = 1:nrow(fechas), .f = get_corn_prices)
write_csv(x = maiz_precios, path = "data/maiz_pozolero_precios.csv")
