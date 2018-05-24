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
library(lubridate)
library(gridExtra)
library(optparse)
library(dbplyr)
library(DBI)

option_list = list(
  make_option(c("--data_date"), type="character", default="2018-2",
              help="data date", metavar="character"),
  make_option(c("--database"), type="character", default="predictivadb",
              help="database name", metavar="character"),
  make_option(c("--user"), type="character", default="maestrosedesol",
              help="database user", metavar="character"),
  make_option(c("--password"), type="character", default="Cd#Q4$bGE#uUyYr",
              help="password for datbase user", metavar="character"),
  make_option(c("--host"), type="character", default="predictivadb.cshyqil2j46y.us-west-2.rds.amazonaws.com",
              help="database host name", metavar="character"),
  make_option(c("--pipeline"), type="character", default="",
              help="pipeline task", metavar="character")
);

opt_parser <- OptionParser(option_list=option_list);

opt <- tryCatch(
  {
    parse_args(opt_parser);
  },
  error=function(cond) {
    message("Error: Provide database connection arguments appropriately.")
    message(cond)
    print_help(opt_parser)
    return(NA)
  },
  warning=function(cond) {
    message("Warning:")
    message(cond)
    return(NULL)
  },
  finally={
    message("Finished attempting to parse arguments.")
  }
)

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

if(length(opt) > 1){
  if(opt$database=="" | opt$user == "" |
     opt$password=="" | opt$host == "" ){
    print_help(opt_parser)
    stop("Database connection arguments are not supplied.n", call.=FALSE)
  }else{
    PGDATABASE <- opt$database
    POSTGRES_PASSWORD <- opt$password
    POSTGRES_USER <- opt$user
    PGHOST <- opt$host
    PGPORT <- "5432"
  }
  
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        host = PGHOST,
                        port = PGPORT,
                        dbname = PGDATABASE,
                        user = POSTGRES_USER,
                        password = POSTGRES_PASSWORD
  )
  
  data_date = opt$data_date
  
  if(str_detect(data_date, pattern = "[1-2]\\d\\d\\d-[0-9]?")){
    fecha_actual <- ymd(paste0(data_date, "-1"))
    fecha_base <- fecha_actual %m-% months(24)
    anio_base <- year(fecha_base)
    mes_base <- month(fecha_base)
  }else{
    stop("Wrong data date.n", call.=FALSE)
  }
  
  query <- "
          select to_date(fecha,'dd/mm/yyyy') as fecha, 
            destino, precio_frec, anio, mes, semana
          from clean.maiz_precios
          where precio_frec > 2 and 
            precio_frec < 7 and
            anio >= %s and 
            mes >= %s
  "
  query_1 <- sprintf(query, anio_base, mes_base)
  
  semanales <- tbl(con, sql(query_1)) %>% collect()
  
  catalogo <- read_csv('data/catalogo_entidades.csv')
  
  semanales_2 <- semanales %>%
    separate(destino, c("ent_mercado","nom_mercado"), sep=':') %>%
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
  
  centrales <- semanales_2 %>%
    dplyr::select(busqueda) %>%
    distinct()
  
  centrales_1 <- centrales %>%
    mutate(cve_central = 1:nrow(centrales)) 
  
  semanales_2 <- semanales_2 %>%
    left_join(centrales_1, by = "busqueda")
  
  centrales_1 <- centrales_1 %>%
    mutate(busqueda = ifelse(str_detect(busqueda, "Unión de Comerciantes de La Paz"), "Calzada General Agustin Olachea Avilés Km 3.5 Baja California Sur",busqueda))
  
  centrales <- map_df(1:nrow(centrales_1), geolocalizacion)
  
  semanales_3 <- semanales_2 %>%
    left_join(centrales %>% dplyr::select(cve_central,lon,lat), by = 'cve_central') %>%
    dplyr::select(fecha, lon, lat, precio=precio_frec) %>%
    filter(!is.na(lon) & !is.na(lat)) %>%
    mutate(tipo = "Central de abastos")
  
  query_2 <- "
  select ano, cve_ent, cve_mun, nom_cul, cv_ciclo, nom_ciclo, cv_moda, nom_moda, precio
  from raw.sagarpa_cierre_2003_2016
  where nom_cul = 'Maíz grano' and
    ano >= %s
  "
  
  query_3 <- sprintf(query_2, anio_base)
  
  municipales <- tbl(con, sql(query_3)) %>% collect()
  
  sids <- readOGR(dsn = "data/municipios_ligero/", layer = "municipios_ligero")
  centroides = gCentroid(sids,byid=TRUE)
  
  muni_coords <- cbind(sids@data,centroides@coords) %>%
    mutate(cve_muni = paste0(CVE_ENT,CVE_MUN)) %>%
    as.tibble() %>%
    dplyr::select(cve_muni, lon=x, lat=y)
  
  municipales_2 <- municipales %>%
    mutate(cve_ent = str_pad(cve_ent,width=2,pad='0',side='left'),
           cve_mun = str_pad(cve_mun,width=3,pad='0',side='left'),
           precio_kg = parse_number(precio)/1000) %>%
    dplyr::select(cve_ent, cve_mun, ano, precio_kg) %>%
    arrange(cve_ent, cve_mun) %>%
    mutate(precio_kg = ifelse(precio_kg < 1, NA, precio_kg),
           id = paste0(cve_ent,cve_mun),
           precio_kg_log = log(precio_kg)) %>%
    filter(!is.na(precio_kg)) %>%
    mutate(fecha = ymd(paste0(ano,'-12-28'))) %>%
    filter(precio_kg > 2 & precio_kg <= 7)
  
  municipales_3 <- municipales_2 %>%
    group_by(fecha,id) %>%
    summarise(precio = mean(precio_kg)) %>%
    left_join(muni_coords, by=c("id"="cve_muni")) %>%
    ungroup() %>%
    dplyr::filter(!is.na(lon)) %>%
    mutate(tipo = "Pie de parcela")
  
  panel_temporal <- bind_rows(municipales_3, semanales_3) %>%
    ungroup() %>%
    as_tibble()
  
  panel_ord <- panel_temporal %>%
    group_by(tipo) %>%
    arrange(fecha)
  panel_ajuste <- mutate(panel_ord, 
                         media = mean(precio),
                         sd = sd(precio),
                         residual = (precio - media)/sd
  )
  
  panel_ajuste$lon <- panel_ajuste$lon + runif(nrow(panel_ajuste), min = 1e-6, max = 9e-6)
  panel_ajuste$lat <- panel_ajuste$lat + runif(nrow(panel_ajuste), min = 1e-6, max = 9e-6)
  maizSP <- SpatialPoints(panel_ajuste[,c('lon','lat')],crs(sids))
  maizTM <- as.POSIXlt(panel_ajuste$fecha)
  maizDF <- panel_ajuste %>% ungroup() %>% dplyr::select(residual)
  timeDF <- STIDF(sp=maizSP, time=maizTM, data=maizDF)
  
  vv <- variogram(residual~1, timeDF, cutoff = 1100, tunit="weeks", twindow = 1000, tlags=0:4)
  
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
  
  vgm <- fit.StVariogram(vv,sumMetric,method="L-BFGS-B",lower=pars.l2,upper=pars.u2)
  
  grid_sp <- spsample(sids, n = 700, type = "regular")
  grid_sp@coords[,1] + runif(350, min = 1e-6, max = 9e-6)
  grid_sp@coords[,2] + runif(350, min = 1e-6, max = 9e-6)
  grid_tm <- seq(ymd('2015-1-1'),ymd('2017-1-1'), by = '3 months')
  grid_tm <- as.POSIXlt(grid_tm)
  grid_ST <- STF(grid_sp, grid_tm)
  # Repetimos el ajuste de kriging
  pred <- krigeST(residual~1, data=timeDF, modelList=vgm, newdata=grid_ST)
  
  
  dbDisconnect(con)
}



















