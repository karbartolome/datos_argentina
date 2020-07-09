# Librerías
library(dplyr)
library(biscale)
library(ggplot2)
library(cowplot)
library(stringr)
library(sf)

rm(list=ls())

# Mapa barrios CABA

caba <- st_read('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios.geojson') %>% 
  mutate(barrio=str_to_title(barrio)) %>% 
  mutate(barrio=case_when(barrio=='Constitucion' ~ 'Constitución',
                          barrio=='San Nicolas' ~ 'San Nicolás',
                          barrio=='Velez Sarsfield' ~ 'Vélez Sársfield',
                          barrio=='Nuñez' ~ 'Núñez',
                          barrio=='Villa Del Parque' ~ 'Villa del Parque',
                          barrio=='Villa Ortuzar' ~ 'Villa Ortúzar',
                          barrio=='Villa Pueyrredon' ~ 'Villa Pueyrredón', 
                          TRUE ~ barrio))

# Superficie total (m2) de departamentos en alquiler de 1 a 5 ambientes (usados y a estrenar) por barrio
# https://www.estadisticaciudad.gob.ar/eyc/?p=27735

alquileres <- readxl::read_excel('alq_mts_caba.xlsx') %>%
  mutate(barrio=case_when(barrio=='La Paternal' ~ 'Paternal',
                          barrio=='Montserrat' ~ 'Monserrat', 
                          TRUE ~ barrio))

df <- merge(caba, alquileres, by='barrio', all.x=TRUE, all.y=TRUE)

# Data en formato biscale (quantiles)
data_biscale <- bi_class(df, 
                         x='mts_alq_may2020',
                         y='mts_alq_may2019', 
                         style="quantile",dim=3)


mapa <- ggplot() +
  geom_sf(data = data_biscale, 
          mapping = aes(fill = bi_class), 
          color = "white", 
          size = 0.1, 
          show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan", dim = 3) +
  labs(title = "Superficie total de departamentos en alquiler",
       subtitle = "CABA - Mayo 2019 - Mayo 2020", 
       caption = "Fuente: Dirección General de Estadística y Censos, Gobierno de la Ciudad de Buenos Aires") +
  bi_theme()+
  theme(plot.caption = element_text(hjust = 0.2))

# Legend
legend <- bi_legend(pal = "DkCyan",
                    dim = 3,
                    xlab = "Metros en alquiler (2020)",
                    ylab = "Metros en alquiler (2019)",
                    size = 8)

# Mapa + Legend
mapa <- ggdraw() +
  draw_plot(mapa, 0, 0, 1, 1) +
  draw_plot(legend, 0.63, 0.05, 0.2, 0.2)

mapa
