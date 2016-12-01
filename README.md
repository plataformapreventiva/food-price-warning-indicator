# Food Price Warning Indicator

# Introducción

La ingesta de alimentos nutritivos es el indicador más básico de la calidad de vida y es un factor estratégico fundamental ya que tiene una relación muy importante con el desarrollo de los países a través del desempeño educativo, la productividad y la igualdad de oportunidades, entre otros. Sin embargo, es un fenómeno persistente y a pesar de los acuerdos y acciones llevadas globalmente según la ONU alrededor de 795 millones de personas no disponen de alimentos suficientes para llevar una vida saludable y activa, buena parte de esta población se encuentra en países en desarrollo. Según datos de CONEVAL, en México existen 28 millones de personas que presentan algún nivel de inseguridad alimentaria [1]. Dada la periodicidad de los censos y las encuestas realizadas por el INEGI no podemos hacer un seguimiento constante a esta población. Sin embargo, existen fuentes de datos que nos pueden dar información útil para dar seguimiento a estas poblaciones y poder activar política pública en casos de alerta. 

El mercado alimentario mexicano se debe analizar desde el ámbito de la oferta y de la demanda. El seguimiento de la oferta se realiza mediante predicción de producción agrícola y los movimientos de importación y exportación de alimentos en el territorio nacional. Por otro lado, el análisis de la demanda tiene como objetivo principal medir las fuerzas que afectan los requerimientos del mercado con respecto a un bien o servicio dado un nivel de precios. 

El objetivo de este proyecto es analizar el nivel de precios y estimar con métodos bayesianos [**] el movimiento de los precios del maiz a nivel estatal en periodos de 3 y 6 meses. Con el vector de precios se utiliza la metodología del indicador IPA [2] desarrollado por la fao para para generar un indicador de alerta temprana de crecimiento anómalo en precios de alimentos básicos que pueden impactar en la seguridad alimentaria de la población en situación de pobreza, en esta primera etapa se implementa el análisis para el maíz por la importancia que tiene en el mercado de consumo mexicano especialmente en la población de alta vulnerabilidad.

La primera sección de este documento describe la generación de la base de datos, la segunda la metodología y los modelos utilizados y la tercera resultados y conclusiones.

# Datos

## PRECIOS NACIONALES
- Información scrappeada del Sistema Nacional de Información e Integración de Mercados (SNIIM)
- Fuente:
 http://www.economia-sniim.gob.mx/

## MERCADO INTERNACIONAL [Future Market]
- Maize (corn) Monthly Price - US Dollars per Metric Ton
- Fuente:
 http://www.indexmundi.com/commodities/?commodity=corn&months=240&currency=mxn
 http://www.indexmundi.com/commodities/?commodity=corn&months=240

## TIPO DE CAMBIO 
- Usamos tipo de cambio para hacer comparable el precio de futuros 
 (CF86) - Tipo de cambio promedio del periodo	
- Tipo de cambio Pesos por dólar E.U.A., Para solventar obligaciones denominadas en moneda extranjera, Fecha de liquidación Cotizaciones promedio
- Fuente:
 http://www.banxico.org.mx/SieInternet/consultarDirectorioInternetAction.do?accion=consultarCuadro&idCuadro=CF86

# Bibliografía
- [1] El Universal, En México, 28 millones con con carencia alimentaria. http://www.eluniversal.com.mx/articulo/nacion/sociedad/2016/04/23/en-mexico-28-millones-con-carencia-alimentaria
- [2] Baquedano, Felix G. Developing a price warning indicator as an early warning tool - a compound growth approach. 2015.
http://www.fao.org/giews/food-prices/research/detail/es/c/235685/ 
- [3] CIMMYT. 1994. 1993/94 world maize facts and trends. Mexico, DF.
- http://www.fao.org/giews/food-prices/home/en/
- http://www.fao.org/giews/food-prices/price-warnings/en/
-http://www.fao.org/giews/food-prices/research/detail/es/c/235685/
