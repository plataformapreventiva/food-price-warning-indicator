# Precios de futuros de Maiz en Estados Unidos
# Unidad:	1 Bushel
# http://mx.investing.com/commodities/us-corn-historical-data
system("curl 'http://mx.investing.com/instruments/HistoricalDataAjax' -H 'Cookie: adBlockerNewUserDomains=1480288488; optimizelyEndUserId=oeu1480288489596r0.9433072115441721; __qca=P0-1325937528-1480288490710; __gads=ID=7e57b2e12d46fa11:T=1480288490:S=ALNI_May61dTmk7uBITn2X3rheruCGy-eQ; PHPSESSID=ugmj0tm93ikeknplmb1l22uai7; geoC=MX; fpros_popup=up; gtmFired=OK; StickySession=id.2010857494.340.mx.investing.com; show_big_billboard49=true; SideBlockUser=a%3A2%3A%7Bs%3A10%3A%22stack_size%22%3Ba%3A1%3A%7Bs%3A11%3A%22last_quotes%22%3Bi%3A8%3B%7Ds%3A6%3A%22stacks%22%3Ba%3A1%3A%7Bs%3A11%3A%22last_quotes%22%3Ba%3A1%3A%7Bi%3A0%3Ba%3A3%3A%7Bs%3A7%3A%22pair_ID%22%3Bs%3A4%3A%228918%22%3Bs%3A10%3A%22pair_title%22%3Bs%3A0%3A%22%22%3Bs%3A9%3A%22pair_link%22%3Bs%3A20%3A%22%2Fcommodities%2Fus-corn%22%3B%7D%7D%7D%7D; hostDebug=web102; optimizelySegments=%7B%224225444387%22%3A%22gc%22%2C%224226973206%22%3A%22search%22%2C%224232593061%22%3A%22false%22%2C%225010352657%22%3A%22none%22%7D; optimizelyBuckets=%7B%7D; nyxDorf=YGRkPzVqNHZmMmBtbjoxLWU2MGtkfWZlPTg3MQ%3D%3D; _ga=GA1.2.2051070637.1480288491' -H 'Origin: http://mx.investing.com' -H 'Accept-Encoding: gzip, deflate' -H 'Accept-Language: es-ES,es;q=0.8,en;q=0.6,gl;q=0.4' -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.59 Safari/537.36' -H 'Content-Type: application/x-www-form-urlencoded' -H 'Accept: text/plain, */*; q=0.01' -H 'Referer: http://mx.investing.com/commodities/us-corn-historical-data' -H 'X-Requested-With: XMLHttpRequest' -H 'Connection: keep-alive' --data 'action=historical_data&curr_id=8918&st_date=01%2F12%2F1999&end_date=27%2F11%2F2016&interval_sec=Monthly' --compressed > ./FINAL/futuros_dump.txt")
xml <- read_file("./FINAL/futuros_dump.txt")
# tienes que borrar el id de la tabla para que se pueda parsear
xml<-sub('<table class="genTbl closedTbl historicalTbl" id="curr_table" tablesorter>','<table>',xml)
xmlfile <- xmlTreeParse(xml)
# the xml file is now saved as an object you can easily work with in R:
class(xmlfile)
topxml <- xmlRoot(xmlfile)
x<-read_xml(xml)
xml_children(x)
header <- xml_find_all(x, "./table/thead/tr/th/text()")
header <- xml_text(header)
header <- matrix(header,ncol = 7,byrow = TRUE)
body <- xml_find_all(x, "./table/tbody/tr/td/text()")
body <- xml_text(body)
futuros <- matrix(body,ncol = 7,byrow = TRUE)

#futuros <- rbind(header,futuros)
futuros <- as_tibble(as.data.frame(futuros))
futuros<-futuros[1:(nrow(futuros)-1),]

futuro <- rename(futuros,fecha=V1,cierre=V2,apertura=V3,maximo=V4,minimo=V5,Vol.=V6,var=V7) %>%
  separate(fecha, c("mes", "año"), " ") %>% 
  mutate(mes = ifelse(mes=="Ene", "Jan", mes)) %>%
  mutate(mes = ifelse(mes=="Abr", "Apr", mes)) %>%
  mutate(mes = ifelse(mes=="Ago", "Aug", mes)) %>%
  mutate(mes = ifelse(mes=="Dic", "Dec", mes)) %>%
  mutate(mes = match(mes,month.abb))  %>%
  mutate(fecha = make_datetime(year=año, month=mes, day=1)) %>%
  select(fecha,año,mes,apertura) %>%
  rename(future_price=apertura) %>%
  mutate(future_price=as.numeric(as.character(future_price))) %>%
  #el prcio es por un bushel
  mutate(future_price = (.021772*future_price)/1000 )
