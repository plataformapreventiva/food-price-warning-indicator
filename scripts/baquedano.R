library(tidyverse)
library(lubridate)
library(stringr)
library(RcppRoll)
library(broom)
library(gridExtra)

setwd('../data/')

maiz_precios <- read_csv(file = "./maiz_precios.csv")
catalogo_entidad <- read_csv('./catalogo_entidades.csv')

cum_sd <- function(x){
  nas <- which(is.na(x))
  xx <- x[-nas]
  y <- sqrt((1/(length(xx)-1))*(cumsum(xx^2) - (1:length(xx))*cummean(xx)^2))
  z <- rep(NA, length(x))
  z[-nas] <- y
  z[z < 1e-6] <- NA
  z
}

cum_mean <- function(x){
  nas <- which(is.na(x))
  xx <- x[!is.na(x)]
  y <- cummean(xx)
  z <- rep(NA, length(x))
  z[-nas] <- y
  z
}

maiz <- maiz_precios %>%
  separate(destino, c('destino_ent', 'destino_central'), ":") %>%
  mutate(fecha = ymd(paste0(anio,'-',mes,'-1'))) %>%
  filter(fecha >= ymd('2008-1-1') & fecha <= ymd('2016-12-1')) %>%
  left_join(catalogo_entidad, by = c("destino_ent"="abr"))

maiz_baquedano <- maiz %>%
  group_by(cve_ent, fecha, anio, mes) %>%
  summarise(precio = mean(precio)) %>%
  ungroup() %>%
  arrange(cve_ent, fecha) %>%
  group_by(cve_ent) %>%
  mutate(precio_lag3 = lag(precio, n = 3),
         precio_lag12 = lag(precio, n = 12),
         CQGR=(precio/precio_lag3)^(1/3)-1,
         CAGR=(precio/precio_lag12)^(1/12)-1) %>% 
  mutate(CQGR_ma = roll_mean(CQGR, 3, align="right", fill=NA),
         CAGR_ma = roll_mean(CQGR, 12, align="right", fill=NA)) %>%
  ungroup() %>%
  group_by(cve_ent, mes) %>% 
  mutate(CQGR_media = cum_mean(CQGR_ma),
         CAGR_media = cum_mean(CAGR_ma),
         CQGR_sd = cum_sd(CQGR_ma),
         CAGR_sd = cum_sd(CAGR_ma)) %>% 
  mutate(Q_IPA_Zt = (CQGR_ma - CQGR_media)/CQGR_sd,
         A_IPA_Zt = (CAGR_ma - CAGR_media)/CAGR_sd) %>%
  ungroup()

pcas <- maiz_baquedano %>%
  replace_na(list(Q_IPA_Zt = 0, A_IPA_Zt = 0)) %>%
  group_by(cve_ent) %>%
  do(comp = tidy(prcomp(~ Q_IPA_Zt + A_IPA_Zt, .), matrix = 'pcs')) %>%
  mutate(gamma = comp$cumulative[1]) %>%
  replace_na(list(gamma = 1)) %>%
  select(cve_ent, gamma)

maiz_baquedano_2 <- maiz_baquedano %>%
  left_join(pcas, by = 'cve_ent') %>%
  mutate(IPA = gamma * Q_IPA_Zt + (1 - gamma) * A_IPA_Zt) %>%
  mutate(X_IPA = ifelse(IPA < 0.5, "Normal",
               ifelse(IPA < 1, "Watch", "Alert")))

# Ejemplo Chihuahua
chihuahua <- maiz_baquedano_2 %>%
  filter(cve_ent == '08' & fecha <= ymd('2015-8-1')) %>%
  filter(fecha >= ymd('2001-1-1'))
chihuahua$X_IPA <- ordered(chihuahua$X_IPA, 
                           levels = c("Normal", "Watch", "Alert"),
                           labels = c("Normal", "Watch", "Alert"))

