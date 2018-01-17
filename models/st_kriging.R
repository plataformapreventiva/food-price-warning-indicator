library(tidyverse)
library(gstat)
library(sp)
library(spacetime)
library(raster)
library(rgdal)
library(rgeos)
library(stringr)
library(GISTools)
library(ggmap)
library(stringr)
library(lubridate)
library(gridExtra)

municipal <- read_csv('data/maiz_municipal_2004_2015.csv')

municipal_13 <- read_csv("data/Cierre_agricola_mun_2013.csv")
colnames(municipal_13)[1] <- "anio"
municipal_13 <- municipal_13 %>%
  rename(anio=anio, cve_ent=CV_ENT, cve_mun=CV_MUN, nom_cul=NOM_CUL, cve_ciclo=CV_CICLO,
         ciclo=NOM_CICLO, cve_modalidad=CV_MODA, modalidad=NOM_MODA, precio=PRECIO) %>%
  dplyr::select(anio, cve_ent, cve_mun, nom_cul, cve_ciclo, ciclo, cve_modalidad, modalidad, precio) %>%
  filter(nom_cul == "Maíz grano")
municipal_14 <- read_csv("data/Cierre_agricola_mun_2014.csv")
colnames(municipal_14)[1] <- "anio"
municipal_14 <- municipal_14 %>%
  rename(anio=anio, cve_ent=CV_ENT, cve_mun=CV_MUN, nom_cul=NOM_CUL, cve_ciclo=CV_CICLO,
         ciclo=NOM_CICLO, cve_modalidad=CV_MODA, modalidad=NOM_MODA, precio=PRECIO) %>%
  dplyr::select(anio, cve_ent, cve_mun, nom_cul, cve_ciclo, ciclo, cve_modalidad, modalidad, precio) %>%
  filter(nom_cul == "Maíz grano")
municipal_15 <- read_csv("data/Cierre_agricola_mun_2015.csv")
colnames(municipal_15)[1] <- "anio"
municipal_15 <- municipal_15 %>%
  rename(anio=anio, cve_ent=CV_ENT, cve_mun=CV_MUN, nom_cul=NOM_CUL, cve_ciclo=CV_CICLO,
         ciclo=NOM_CICLO, cve_modalidad=CV_MODA, modalidad=NOM_MODA, precio=PRECIO) %>%
  dplyr::select(anio, cve_ent, cve_mun, nom_cul, cve_ciclo, ciclo, cve_modalidad, modalidad, precio) %>%
  filter(nom_cul == "Maíz grano")
municipal_16 <- read_csv("data/Cierre_agricola_mun_2016.csv")
colnames(municipal_16)[1] <- "anio"
municipal_16 <- municipal_16 %>%
  rename(anio=anio, cve_ent=CV_ENT, cve_mun=CV_MUN, nom_cul=NOM_CUL, cve_ciclo=CV_CICLO,
         ciclo=NOM_CICLO, cve_modalidad=CV_MODA, modalidad=NOM_MODA, precio=PRECIO) %>%
  dplyr::select(anio, cve_ent, cve_mun, nom_cul, cve_ciclo, ciclo, cve_modalidad, modalidad, precio) %>%
  filter(nom_cul == "Maíz grano")

municipal_1 <- municipal_13 %>%
  bind_rows(municipal_14) %>% bind_rows(municipal_15) %>% bind_rows(municipal_16) %>%
  mutate(cve_ent = str_pad(cve_ent,width=2,pad='0',side='left'),
         cve_mun = str_pad(cve_mun,width=3,pad='0',side='left'),
         precio_kg = precio/1000)

municipal_2 <- municipal_1 %>%
  dplyr::select(cve_ent, cve_mun, anio, precio_kg) %>%
  arrange(cve_ent, cve_mun) %>%
  mutate(precio_kg = ifelse(precio_kg < 1, NA, precio_kg),
         id = paste0(cve_ent,cve_mun),
         precio_kg_log = log(precio_kg))

#Rcentroids
sids <- readOGR(dsn = "data/municipios_ligero/", layer = "municipios_ligero")
centroides = gCentroid(sids,byid=TRUE)

muni_coords <- cbind(sids@data,centroides@coords) %>%
  mutate(cve_muni = paste0(CVE_ENT,CVE_MUN)) %>%
  as.tibble() %>%
  dplyr::select(cve_muni, lon=x, lat=y)

catalogo <- read_csv('data/catalogo_entidades.csv')

semanales <- read_csv("data/maiz_pozolero_precios.csv") %>%
  separate(destino, c("ent_mercado","nom_mercado"), sep = ':') %>%
  mutate(ent_mercado = ifelse(ent_mercado == 'DF', 'Distrito Federal', ent_mercado),
         ent_mercado = ifelse(str_detect(ent_mercado, "Baja") & !str_detect(nom_mercado, "Sur"), 'Baja California', ent_mercado)) %>%
  left_join(catalogo, by=c("ent_mercado"="nom_ent_corto")) %>%
  mutate(mercado = ifelse(str_detect(nom_mercado, "C.C.Agrop.|C.A.|C.de Abasto|C. A."), "Central de Abastos", 
                          ifelse(str_detect(nom_mercado, "C.May."), "Central",
                                 ifelse(str_detect(nom_mercado, "Mod.A. T. |M.A.|M.|Mod.A."), "Mercado de Abastos",
                                        ifelse(str_detect(nom_mercado, "C. Distr.y A.|C.Distr.|C. Distr."), "Central",
                                               ifelse(str_detect(nom_mercado, "M."), "Mercado",
                                                      ifelse(str_detect(nom_mercado, "U.Com."), "Unidad comercial",nom_mercado)))))),
         busqueda = paste(mercado, ent_mercado))

centrales <- semanales %>%
  dplyr::select(busqueda) %>%
  distinct()

centrales_1 <- centrales %>%
  mutate(cve_central = 1:nrow(centrales)) 

semanales <- semanales %>%
  left_join(centrales_1, by = "busqueda")

centrales_1 <- centrales_1 %>%
  mutate(busqueda = ifelse(str_detect(busqueda, "Unión de Comerciantes de La Paz"), "Calzada General Agustin Olachea Avilés Km 3.5 Baja California Sur",busqueda))

geolocalizacion <- function(i){
  bus <- centrales_1$busqueda[i]
  d <- unlist(strsplit(str_replace_all(bus, ",", ""), split=" "))
  if(sum(duplicated(d)) > 0){
    str <- paste(d[-which(duplicated(d))], collapse = ' ')
  }else{
    str <- bus
  }
  x <- geocode(str, force = T)
  if(is.na(x$lon)){
    x <- geocode(paste(bus, 'Mexico'), force = T)
  }
  tibble(cve_central=centrales_1$cve_central[i],busqueda=bus,lon=x$lon, lat=x$lat)
}
# http://www.conacca.mx/index.php/styles/centrales-de-abasto
# centrales <- map_df(1:nrow(centrales_1), geolocalizacion)
# write_csv(x = centrales, path = "data/centrales_abasto.csv")
centrales <- read_csv("data/centrales_abasto.csv")

semanales_2 <- semanales %>%
  left_join(centrales %>% dplyr::select(cve_central,lon,lat), by = 'cve_central')

####################### ST dataframe 🙈 ################################
# https://www.r-bloggers.com/spatio-temporal-kriging-in-r/

# La fecha de la observación debe estar en formato POSIXlt
# Vamos a suponer que los precios observados anualmente corresponden
# al 1 de diciembre, ya que estos datos se registran a final de año.

municipal_2 <- municipal_2 %>%
  filter(precio_kg > 2 & precio_kg <= 7 & anio >= 2015 & anio <= 2016) %>%
  mutate(fecha = ymd(paste0(anio,'-12-28'))) 

municipal_3 <- municipal_2 %>%
  group_by(fecha,id) %>%
  summarise(precio = mean(precio_kg)) %>%
  left_join(muni_coords, by=c("id"="cve_muni")) %>%
  ungroup() %>%
  dplyr::filter(!is.na(lon)) %>%
  mutate(tipo = "Pie de parcela")

semanales_3 <- semanales_2 %>%
  filter(precio_frec > 2 & precio_frec < 7 & anio >= 2015 & anio <= 2016) %>%
  mutate(fecha = dmy(fecha)) %>%
  dplyr::select(fecha, lon, lat, precio=precio_frec) %>%
  mutate(tipo = "Central de abastos")

panel_temporal <- bind_rows(municipal_3, semanales_3) %>%
  ungroup() %>%
  as_tibble()

aux <- tibble(grupo="A pie de parcela (municipal)", precio=municipal_3$precio) %>%
  bind_rows(tibble(grupo="En central de abastos (semanales)", precio=semanales_3$precio))
ggplot(aux, aes(x=grupo, y=precio)) +
  geom_boxplot() +
  scale_y_log10() + 
  scale_x_discrete(name="") +
  scale_y_continuous(name="Precio por kilogramo")

panel_temporal$lon <- panel_temporal$lon + runif(nrow(panel_temporal), min = 1e-6, max = 9e-6)
panel_temporal$lat <- panel_temporal$lat + runif(nrow(panel_temporal), min = 1e-6, max = 9e-6)
panel_temporal$fecha <- lubridate::floor_date(panel_temporal$fecha,"day")

maizSP <- SpatialPoints(panel_temporal[,c('lon','lat')],crs(sids))
maizTM <- as.POSIXlt(panel_temporal$fecha)
maizDF <- panel_temporal %>% dplyr::select(precio)
timeDF <- STIDF(sp=maizSP, time=maizTM, data=maizDF)

# vv <- variogram(precio~1, timeDF, tunit="weeks", twindow=40, tlags=0:3)
# write_rds(x = vv, path = 'out/semivgm_emp_maiz.rds')
vv <- read_rds(path = 'out/semivgm_emp_maiz.rds')
plot(vv)
plot(vv, map=FALSE)
plot(vv,wireframe=T)

SimplesumMetric <- vgmST("simpleSumMetric",
                         space = vgm(1,"Sph", 200, 0),
                         time = vgm(200,"Sph", 200, 0),
                         joint = vgm(0.1,"Sph", 200, 0), 
                         nugget=0.3, 
                         stAni=200)

pars.l <- c(sill.s = 0, range.s = 10, nugget.s = 0,
            sill.t = 0, range.t = 1, nugget.t = 0,
            sill.st = 0, range.st = 10, nugget.st = 0,
            anis = 0)

vgm <- fit.StVariogram(vv, SimplesumMetric, method = "L-BFGS-B",lower=pars.l)
attr(vgm, "MSE")

# Parámetros ajustados del semivariograma paramétrico
extractPar(vgm)

# Semivariograma ajustado
plot(vv,vgm, map = FALSE)

grid_sp <- spsample(sids, n = 500, type = "regular")
#plot(grid_sp, col = 'dark red', pch = ".")

grid_tm <- seq(ymd('2015-1-1'),ymd('2017-1-1'), by = '3 months')
grid_tm <- as.POSIXlt(grid_tm)
grid_ST <- STF(grid_sp, grid_tm)

pred <- krigeST(precio~1, data=timeDF, modelList=vgm, newdata=grid_ST)

# Ajuste de predicción
z = names(pred@data)[1]
df_pred = data.frame(reshape(as.data.frame(pred)[c(z, "time", "sp.ID")], 
                        timevar = "time", idvar = "sp.ID", direction = "wide"))[, -1, drop=FALSE]
x = addAttrToGeom(geometry(pred@sp), df_pred, match.ID = FALSE)
scales = longlat.scales(pred@sp, scales = list(draw = FALSE), 
                        xlim = bbox(pred@sp)[1,], ylim = bbox(pred@sp)[2,])

trimDates = function(x) {
  if (is(x, "ST"))
    x = index(x@time)
  it = as.character(x)
  if (identical(grep("-01$", it), 1:length(it))) # all: day
    it = sub("-01$", "", it)
  if (identical(grep("-01$", it), 1:length(it))) # all: month
    it = sub("-01$", "", it)
  it
}
args = list(x, names.attr = trimDates(pred), as.table = T, 
            auto.key = list(space = "right"), scales = scales, main = "")
if (!is.factor(pred[[z]])) {
  args$cuts = 15
  args$at = NULL
}
if (is(pred@sp, "SpatialPoints"))
  args$key.space = "right"
do.call(spplot, args)


# El problema con este resultado es la diferencia en las medias para 
# precios semanales en central de abstos y precios mensuales a nivel 
# de parcela

panel_temporal %>%
  group_by(tipo) %>%
  summarise(media=mean(precio), sd=sd(precio), n_obs=n())

# Una primera aproximación será normalizar el precio por separado
# en cada grupo.

# Veamos una gráfica de cuantiles empíricos vs cuantiles normales
# para analizar qué tanto sentido tiene hacer pooling ajustando
# media y desviación estándar
panel_ord <- panel_temporal %>%
  group_by(tipo) %>%
  arrange(fecha)
panel_ajuste <- mutate(panel_ord, 
                        media = mean(precio),
                        sd = sd(precio),
                        residual = (precio - media)/sd
                        )

todos_residual <- sort(panel_ajuste$residual)
panel_total <- panel_ajuste %>%
  arrange(residual) %>%
  mutate(
    n_obs = n(),
    cuantil_total = approx(x = 1:length(todos_residual), y = todos_residual,
                           n = n_obs[1])$y
  )

ggplot(panel_total, aes(x = cuantil_total, y = residual)) +
  geom_point() +
  facet_wrap(~ tipo, nrow = 1) +
  stat_smooth(method = "lm")

# Agrupamos los datos y vemos el ajuste con una distribución normal
n <- nrow(panel_total)
panel_normal <- panel_total %>%
  ungroup() %>%
  arrange(residual) %>%
  mutate(
    valor.f.tot = (seq(1, n) - 0.5) / n, 
    q.norm.tot = qnorm(valor.f.tot)
  )

ggplot(panel_normal, aes(x = q.norm.tot, y = residual)) +
  geom_point() +
  stat_smooth(method = "lm")

# Calculamos nuevamente el semivariograma empírico
panel_ajuste$lon <- panel_ajuste$lon + runif(nrow(panel_ajuste), min = 1e-6, max = 9e-6)
panel_ajuste$lat <- panel_ajuste$lat + runif(nrow(panel_ajuste), min = 1e-6, max = 9e-6)
maizSP2 <- SpatialPoints(panel_ajuste[,c('lon','lat')],crs(sids))
maizTM2 <- as.POSIXlt(panel_ajuste$fecha)
maizDF2 <- panel_ajuste %>% ungroup() %>% dplyr::select(residual)
timeDF2 <- STIDF(sp=maizSP2, time=maizTM2, data=maizDF2)

#vv2 <- variogram(residual~1, timeDF2, cutoff = 1100, tunit="weeks", twindow = 1000, tlags=0:4)
#write_rds(x = vv2, path = 'out/semivgm_emp_maiz2.rds')
vv2 <- read_rds(path = 'out/semivgm_emp_maiz2.rds')
plot(vv2)
plot(vv2, map=FALSE)
plot(vv2,wireframe=T)

# Ajustamos nuevamente el semivariograma empírico
sumMetric <- vgmST("sumMetric", 
                   space = vgm(psill=0.5,"Gau", range=200, nugget=0.2),
                   time = vgm(psill=0.5,"Gau", range=200, nugget=0.2), 
                   joint = vgm(psill=0.5,"Gau", range=200, nugget=0.2),
                   nugget = 0.01,
                   stAni=200)

# Adaptamos parámetros iniciales
pars.l2 <- c(sill.s = 0, range.s = 10, nugget.s = 0,
            sill.t = 0, range.t = 1, nugget.t = 0,
            sill.st = 0, range.st = 10, nugget.st = 0,
            anis = 0)

pars.u2 <- c(sill.s = 2, range.s = 100, nugget.s = 1,
            sill.t = 100, range.t = 100, nugget.t = 1,
            sill.st = 2, range.st = 100, nugget.st = 1,
            anis = 700)

vgm2 <- fit.StVariogram(vv2,sumMetric,method="L-BFGS-B",lower=pars.l2,upper=pars.u2)
# Semivariograma ajustado
plot(vv2,vgm2, map = FALSE)
plot(vv2, map=FALSE)
extractPar(vgm2)

fit <- list(vst=vv2, vstModel=vgm2)
toPlot = data.frame(fit$vst)
toPlot2 <- toPlot %>% mutate(timelag = ordered(timelag, levels=unique(toPlot$timelag)))
ggplot(toPlot2, aes(x=dist, y=gamma, color=timelag, group=timelag)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous(name = "distancia", limits = c(0,1100)) +
  scale_y_continuous(name = expression(gamma), limits = c(0.2,1.42))
# ggplot(toPlot, aes(x=timelag, y=gamma, color=spacelag, group=spacelag)) + 
#   geom_point() +
#   geom_line()


dist_grid <- expand.grid(timelag = unique(toPlot$timelag), 
                         spacelag = seq(min(toPlot$spacelag, na.rm=T),
                                        max(toPlot$spacelag, na.rm=T), 
                                        length.out=500))

model <- fit$vstModel
vs = variogramLine(model$space, dist_vector=dist_grid$spacelag)[,2]
vt = variogramLine(model$time,  dist_vector=dist_grid$timelag)[,2]
h = sqrt(dist_grid$spacelag^2 + (model$stAni * as.numeric(dist_grid$timelag))^2)
vst = variogramLine(model$joint, dist_vector=h)[,2]
aux <- data.frame(spacelag=dist_grid$spacelag, timelag=dist_grid$timelag, model=(vs + vt + vst))
aux$timelag <- ordered(aux$timelag, levels = unique(aux$timelag))
ggplot(aux, aes(x=spacelag, y = model, group = timelag, color = timelag)) + 
  geom_line(position = position_dodge(30)) +
  scale_x_continuous(name = "distancia", limits = c(0,1100)) +
  scale_y_continuous(name = expression(gamma), limits = c(0.2,1.42))


grid_sp <- spsample(sids, n = 700, type = "regular")
grid_sp@coords[,1] + runif(350, min = 1e-6, max = 9e-6)
grid_sp@coords[,2] + runif(350, min = 1e-6, max = 9e-6)
grid_tm <- seq(ymd('2015-1-1'),ymd('2017-1-1'), by = '3 months')
grid_tm <- as.POSIXlt(grid_tm)
grid_ST <- STF(grid_sp, grid_tm)
# Repetimos el ajuste de kriging
pred2 <- krigeST(residual~1, data=timeDF2, modelList=vgm2, newdata=grid_ST)

# Nuevo ajuste de predicción
stplot(pred2, main = "Precio del maíz en el espacio y tiempo")
