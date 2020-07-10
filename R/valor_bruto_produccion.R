library(ggplot2)
library(dplyr)
library(janitor)
library(ggplot2)
library(gganimate)
library(tidyr)
library(png)
library(magick)
library(ggimage)
library(zoo)

# Datos
actividad <- readxl::read_excel('valor_bruto_actividad.xlsx') %>% 
  mutate(trimestre=as.yearqtr(paste(año,trimestre))) %>% select(-año) 

# Porcentualmente 
actividad<-actividad %>% 
  mutate_at(names(actividad[,2:17]),as.numeric) %>% 
  #adorn_totals(where="col") %>% 
  adorn_percentages("row")

actividad <- gather(data = actividad, key = "actividad", value = "porcentaje", 2:17) %>% 
  group_by(trimestre) %>% 
  arrange(desc(porcentaje)) %>% 
  mutate(rank=row_number()) %>% 
  filter(rank <= 10) %>%
  mutate(
    actividad = case_when(
      actividad == "Pesca" ~ "Pesca",
      actividad == "Industria.manufacturera" ~ "Industria manufacturera",
      actividad == "Agricultura..ganadería..caza.y.silvicultura" ~ "Agricultura, ganadería, caza y silvicultura",
      actividad == "Comercio.mayorista..minorista.y.reparaciones" ~ "Comercio mayorista, minorísta y reparaciones",
      actividad == "Actividades.inmobiliarias..empresariales.y.de.alquiler" ~ "Inmobiliarias, empresariales y alquileres",
      actividad ==  "Transporte.y.comunicaciones" ~ "Transporte y comunicaciones",
      actividad == "Explotación.de.minas.y.canteras" ~ "Explotación de minas y canteras",
      actividad == "Administración.pública.y.defensa..planes.de.seguridad.social.de.afiliación.obligatoria" ~ "Admin. Pública, defensa, seguridad social",
      actividad == "Intermediación.financiera" ~ "Intermediación financiera",
      actividad == "Enseñanza" ~ "Enseñanza",
      actividad == "Construcción" ~ "Construcción",
      actividad == "Servicios.sociales.y.de.salud" ~ "Servicios sociales y de salud", 
      TRUE ~ actividad
    )
  )


barchartrace <- ggplot(actividad, aes(x = -rank, y = porcentaje, fill = actividad)) +
  geom_tile(aes(y = porcentaje / 2, height = porcentaje, fill = actividad), width =
              0.8) +
  coord_flip(clip = "off") +
  geom_text(
    aes(label = actividad),
    colour = "black",
    fontface = "bold",
    hjust = "left",
    nudge_y = 0.005,
    size = 3.4
  ) +
  scale_fill_viridis_d(option = "B") +
  ylim(0, 0.42) +
  transition_states(trimestre,
                    transition_length = 3,
                    state_length = 3) +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 13),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(
    title = 'Composición de la Producción Argentina',
    subtitle = 'Trimestre: {closest_state}',
    # subtitle='Trimestre: {current_frame}',
    caption = "Elaboración propia en base a datos del INDEC, #RstatsES, @karbartolome",
    y = "% sobre el total de la producción"
  )


animate(grafico,duration=13, nframes = 100, fps = 50)
