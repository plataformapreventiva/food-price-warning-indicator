library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)

url_base <- 'http://www.economia-sniim.gob.mx/Nuevo/Consultas/MercadosNacionales/PreciosDeMercado/Agricolas/ResultadosConsultaFechaGranos.aspx?Semana=%s&Mes=%s&Anio=%s&ProductoId=605&OrigenId=-1&Origen=Todos&DestinoId=-1&Destino=Todos&RegistrosPorPagina=%%20500'

get_corn_prices <- function(i){
  print(i)
  anio <- fechas$anio[i]
  mes <- fechas$mes[i]
  semana <- fechas$semana[i]
  url <- sprintf(url_base,semana,mes,anio)
  empty <- data.frame(fecha=NULL,origen=NULL,destino=NULL,precio_min=NULL,precio_max=NULL,
                      precio_frec=NULL,obs=NULL,anio=NULL,mes=NULL,semana=NULL)
  out <- tryCatch(
    {
      data <- read_html(url) %>%
        html_nodes(xpath = '//*[(@id = "tblResultados")]') %>%
        html_table(fill = T, header = T)
      if(length(data) > 0){
        dat <- data[[1]]
        names(dat) <- c("fecha","origen","destino","precio_min","precio_max","precio_frec","obs")
        dat[dat$destino=="", "destino"] <- NA
        dat$obs <- as.character(dat$obs)
        dat$precio_min <- as.numeric(dat$precio_min)
        dat$precio_max <- as.numeric(dat$precio_max)
        dat$precio_frec <- as.numeric(dat$precio_frec)
        dat <- dat %>% fill(destino)
        dat$anio <- anio
        dat$mes <- mes
        dat$semana <- semana
      }else{
        dat <- empty
      }
      dat
    },
    error=function(cond) {
      message(paste("empty table:", url))
      return(empty)
    },
    finally={
      message(paste("Processed URL:", url))
      message("Data will be appended.")
    }
  )    
  out
}

anios <- 2000:2017
meses <- 1:12
semanas <- 1:4
fechas <- expand.grid(anio=anios, mes=meses, semana=semanas)
maiz_precios <- map_df(.x = 1:nrow(fechas), .f = get_corn_prices)
write_csv(x = maiz_precios, path = "data/maiz_pozolero_precios.csv")
