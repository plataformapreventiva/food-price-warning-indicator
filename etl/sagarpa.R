library(tidyverse)
library(lubridate)
library(stringr)

set.wd('../data/')

municipal <- read_csv('../data/maiz_municipal_2004_2015.csv')

municipal_1 <- municipal %>%
  gather(key = anio, value = precio, -Nomestado, -Idmunicipio, -Nommunicipio) %>%
  mutate(cve_mun = str_pad(Idmunicipio,width=3,pad='0',side='left'),
         precio_kg = precio/1000) %>%
  select(nom_ent_corto=Nomestado,cve_mun,nom_mun=Nommunicipio,anio,precio_kg)

catalogo_entidad <- read_csv('../data/catalogo_entidades.csv')

municipal_2 <- municipal_1 %>%
  left_join(catalogo_entidad, by = 'nom_ent_corto') %>%
  select(cve_ent, nom_ent, cve_mun, nom_mun, anio, precio_kg) %>%
  arrange(cve_ent, cve_mun) %>%
  mutate(precio_kg = ifelse(precio_kg < 0.30, NA, precio_kg),
         id = paste0(cve_ent,cve_mun),
         precio_kg_log = log(precio_kg))

write_csv(x = municipal_2, path = '../data/municipal_2.csv')



