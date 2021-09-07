# Trabajo práctico 1

Espacio_verde <- read.csv ("Data/espacio-verde-publico.csv", stringsAsFactors = TRUE)
#Fuente: Buenos Aires Data

library(tidyverse)
library(readr)

summary(Espacio_verde)
class (Espacio_verde)

mean(Espacio_verde$SUP_TOTAL) 
# El promedio de superficie de los espacios públicos de la ciudad es de 456,10 m2.

Espacio_verde %>%
  group_by(clasificac) %>%
  summarise(cantidad=n()) %>%
  arrange(desc(cantidad)) 
# Los espacios verdes que más hay en la ciudad de Bs. As. son plazoletas (650), seguidos de canteros centrales (620) y lyego plazas (343). Hay un sólo Jardín Botánico en toda la ciudad.


Espacio_verde %>%
  group_by(clasificac) %>%
  summarise(M2_TOTALES=sum(SUP_TOTAL)) 
# El espacio verde con más superficie son las plazas, que tienen 633.986 m2, seguido de los parques con 111.855 m2.

Espacio_verde_res <- Espacio_verde %>%
  select(WKT, nombre, nombre_ev, clasificac, BARRIO, COMUNA, area, perimeter, ESTADO, SUP_TOTAL )

Espacio_verde_res %>%
  group_by(COMUNA) %>%
  summarise(cantidad=n(),
            M2_Comuna=sum(SUP_TOTAL))
# La comuna 8 es la que mayor cantidad de espacios verdes tiene (344), seguida de la 2 (256) y la 4 (175). Sin embargo, respecto a la cantidad de m2 de espacios verdes por Comuna, las comunas que más m2 tienen son la 4(120857), la 14 (120744) y la 12 (96327).

# Ahora voy a unir otro dataset con informción sobre tamaño de Comunas.


library(sf)
Comunas <- st_read("Data/partidos_amba.shp")
head(Comunas)

Esp_verde_Comuna <- Espacio_verde_res %>%
  mutate(COMUNA= paste("Comuna", COMUNA, sep=" "))  %>%
  group_by(COMUNA) %>%
  summarise(cantidad=n(),
            M2_Comuna=sum(SUP_TOTAL))

Esp_verde_Comuna %>%
  mutate(nombre=COMUNA) %>%
  select(-COMUNA) %>%
  mutate(M2_Comuna= M2_Comuna/1000000)%>%
  left_join(Comunas, by="nombre") %>%
  mutate(KM_2com=(M2_Comuna/area_km2)*1000000)

# Esto indica que la comuna con mayor cantidad de m2 de espacio verde público por km2 es la Comuna 6 (8874 m2 por cada km2), seguida de la 14 (7417) y la Comuna 12 (6187). Por el contrario, la Comuna con peor relación de m2 verde público por km2 es la Comuna 15 con (1451) seguida de la Comuna 2 (1540) y la Comuna 8 (1548).
# Voy a graficar estas relaciones:

Esp_verde_Comuna <- left_join(Comunas, Esp_verde_Comuna, by=c("nombre"="COMUNA")) %>%
  filter(provincia=="CABA")

m2verdes_barrio <- ggplot()+
  geom_sf(data=Esp_verde_Comuna, aes(fill=M2_Comuna))+
  geom_sf_label(data=Esp_verde_Comuna, aes(label=nombre), size=2)+
  labs(title="¿Cuántos m2 de espacio verde hay por Comuna?",
       subtitle="Comunas CABA",
       fill="M2 cuadrados espacios verde por Comuna",
       caption="Fuente: Datos GCBA")+
  scale_fill_distiller(palette="Greens")+
  theme_minimal()

m2verdes_barrio

# En este gráfico se puede observar espacialmente que cue comunas tienen mayor cantidad de m2 de espacios verdes.
# Ahora voy a incorporar data de Properati para ver la relación del precio de compra/venta de propiedades en relación a los espacios verdes públicos.

data_Properati <- vroom::vroom("https://storage.googleapis.com/properati-data-public/ar_properties.csv.gz")

skimr::skim(data_Properati)

str(data_Properati)

data_Properati  %>% count(l2)

# Tenemos 265125 propiedades en Capital Federal, que son con las que nos vamos a quedar.

data_Properaticaba <- data_Properati %>%
  filter(l2=="Capital Federal" & operation_type=="Venta" & lat!="NA" & currency=="USD") %>%
  st_as_sf(coords=c("lon", "lat"), crs=4326)
# Quedaron 168085 propiedades que están en venta. Ahora con un gráfico la intención es mostrar cuanto afecta la cercanía a un espacio verde al precio de venta de la propiedad.

Espacio_verde_res <- Espacio_verde_res %>%
  st_as_sf(wkt="WKT", crs=4326)

st_crs(Comunas)
st_crs(data_Properaticaba)
st_crs(Espacio_verde_res)

data_Properaticaba <- st_join(data_Properaticaba, Comunas)

data_Properaticaba <- filter(data_Properaticaba, !is.na(nombre))

summary(data_Properaticaba)

ggplot(data_Properaticaba)+
  geom_sf()

options(scipen=999)

relacion_verde_venta <- ggplot()+
  geom_sf(data=filter(Comunas, provincia=="CABA"))+
  geom_sf(data=data_Properaticaba, aes(color=price), alpha=0.1)+
  geom_sf(data=Espacio_verde_res, color="darkolivegreen4")+
  labs(title="Relación entre precio venta inmuebles y cercanía a espacios verdes",
       subtitle="CABA",
       color="Precio por m2 en USD",
       caption="Fuente: Datos GCBA y Properati")+
  theme_minimal()

relacion_verde_venta

data_Properaticaba  %>%
  select(as.character("price")) %>%
  summary()

data_Properaticaba <- data_Properaticaba %>%
  filter(!is.na(surface_total)) %>%
  mutate(precio_m2=price/surface_total) %>% 
  mutate(categoria=cut(precio_m2, breaks=c(0,1000,2000,3000,4000,+Inf), labels=c("0-999","1000-1999","2000-2999","3000-4000","+4000")))


data_Properaticaba  %>%
  select(as.character("precio_m2")) %>%
  summary()
relacion_verde_venta <- ggplot()+
  geom_sf(data=filter(Comunas, provincia=="CABA"))+
  geom_sf(data=data_Properaticaba, aes(color=categoria), alpha=0.3)+
  scale_color_viridis_d()+
  geom_sf(data=Espacio_verde_res, color="darkgreen")+
  labs(title="Relación entre precio venta inmuebles y cercanía a espacios verdes",
       subtitle="CABA",
       color="Precio por m2 en USD",
       caption="Fuente: Datos GCBA y Properati")+
  theme_minimal()

relacion_verde_venta
 

# Para cerrar el TP quise ver la relación del precio de los departamentos en venta con su cercanía a los espacios verdes. No parecería la cercanía a los espacios verdes influir en el precio por m2 de la propiedad, sino más bien una cuestión de ubicación barrial.
# Sin embargo, en los barrios donde los precios por m2 son más económicos, en especial al sur de la ciudad, sí se puede apreciar cierto aumento de precio dentro del barrio en cercanía a los espacios verdes.