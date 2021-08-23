# Trabajo práctico 1

Espacio_verde <- read.csv ("Data/espacio-verde-publico.csv", stringsAsFactors = TRUE)

library(tidyverse)

summary(Espacio_verde)

mean(Espacio_verde$SUP_TOTAL) # El promedio de superficie de los espacios públicos de la ciudad es de 456,10 m2.

Espacio_verde %>%
  group_by(clasificac) %>%
  summarise(cantidad=n()) %>%
  arrange(desc(cantidad)) # Los espacios verdes que más hay en la ciudad de Bs. As. son plazoletas (650), seguidos de canteros centrales (620) y lyego plazas (343). Hay un sólo Jardín Botánico en toda la ciudad.


Espacio_verde %>%
  group_by(clasificac) %>%
  summarise(M2_TOTALES=sum(SUP_TOTAL)) # El espacio verde con más superficie son las plazas, que tienen 633.986 m2, seguido de los parques con 111.855 m2.


  


