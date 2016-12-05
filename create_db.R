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
                                        ifelse(edo_destino=="coahuila","05",CVE_ENT))))) %>%
  select(fecha,CVE_ENT,precio_min) %>%
  rename(maiz_blanco=precio_min) %>% write.csv("semantic_serie_tiempo_estados.csv")



