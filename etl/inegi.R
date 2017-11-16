library(tidyverse)
library(lubridate)
library(ggmap)
library(maptools)
library(rgdal)
library(rgeos)

maiz_inpc <- read_csv('../data/maiz_INPC.csv')

maiz_inpc_1 <- maiz_inpc %>%
  mutate(fecha = ymd(paste0(ano,'-',mes,'-1'))) %>%
  group_by(fecha, claveciudad, nombreciudad) %>%
  summarise(precio_media = mean(preciopromedio),
            precio_q1 = quantile(preciopromedio,0.25),
            precio_q3 = quantile(preciopromedio,0.75))

ggplot(maiz_inpc_1, aes(x=fecha, y=precio_media, group=nombreciudad)) +
  geom_point(alpha = 0.4) +
  geom_segment(aes(x=fecha,y=precio_q1,xend=fecha,yend=precio_q3), alpha = 0.8) +
  facet_wrap(~nombreciudad)

maiz_mexico <- maiz_inpc_1 %>% 
  filter(claveciudad==1) %>%
  ungroup() %>%
  mutate(fecha=as.Date(fecha), year = year(fecha), month=as.integer(month(fecha)))

ggplot(maiz_mexico, aes(x=month, y=precio_media, color=factor(year))) +
  geom_line() +
  #geom_segment(aes(x=month,y=precio_q1,xend=month,yend=precio_q3)) +
  #scale_x_date(date_breaks = '1 month', date_labels = '%b-%m') +
  theme(axis.text.x=element_text(angle=90,hjust=1)) #+
  #facet_wrap(~ year)

ciudades <- maiz_inpc %>%
  select(claveciudad,nombreciudad) %>%
  distinct()

geolocalizacion <- function(i){
  clave <- ciudades$claveciudad[i]
  ciudad <- ciudades$nombreciudad[i]
  x <- geocode(paste(ciudad, 'Mexico'))
  tibble(claveciudad=clave,nombreciudad=ciudad,lon=x$lon, lat=x$lat)
}

ciudades <- map_df(1:26, geolocalizacion)

mex <- get_map(location = c(-123,12,-78,34),
               source = 'google', force = T, crop = T)
edo_shp <- readOGR("estados_ligero", layer = "Mex_Edos")
edo_shp@data$id <- rownames(edo_shp@data)
edo_datos <- edo_shp@data
edo_df <- fortify(edo_shp,region="id")

maiz_inpc_2 <- maiz_inpc_1 %>%
  left_join(ciudades, by = 'claveciudad')

maiz_inpc_3 <- maiz_inpc_2 %>%
  filter(fecha > ymd('2017-1-1'))

ggmap(mex) +
  geom_polygon(data=edo_df, aes(long,lat,group = group),alpha = 0.03,
               color = 'navyblue', fill = 'mediumpurple1', size = 0.2) +
  geom_point(data=maiz_inpc_3,
             aes(x=lon,y=lat,size=precio_media,color=precio_media)) +
  geom_jitter()
