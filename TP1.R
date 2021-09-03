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

data_Properati <- data_Properati %>%
  filter(l2=="Capital Federal" & operation_type=="Venta")
# Quedaron 183810 propiedades que están en venta. Ahora con un gráfico la intención es mostrar cuanto afecta la cercanía a un espacio verde al precio de venta de la propiedad.



