Viajes en subte
================

# Librerías

Se cargan las librerías a utilizar

``` r
library(tidyverse) # Manipulación de datos
library(lubridate)  # Manipulación de fechas
library(circular) # Datos periódicos
library(gt) # Tablas
library(reshape) # Untable
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")

options(scipen=999)
```

# Datos

Se importan los datos de viajes en subte de la Ciudad Autónoma de Buenos
Aires, en 2020.

``` r
df <-
  read_delim(
    'https://cdn.buenosaires.gob.ar/datosabiertos/datasets/sbase/subte-viajes-molinetes/molinetes_112021.csv',
    delim = ';',
    col_types = cols(FECHA = col_date("%d/%m/%Y"))
  ) %>%
  janitor::clean_names() %>%
  filter(!is.na(fecha)) %>%
  mutate(hora = hour(desde))
```

# Generación de datos para la tabla

Función para obtener la hora promedio o mediana:

``` r
get_hour <- function(.linea, .df, .mean = TRUE) {
  temp <- .df %>%
    filter(linea == .linea) %>%
    select(hora, pax_total)
  
  if (.mean == TRUE) {
    hora <- untable(temp, num = temp$pax_total) %>%
      select(-pax_total) %>%
      mutate(hora_circular = circular(hora, template = "clock24", units = "hours")) %>%
      summarise(hora = mean(hora_circular)) %>%
      pull(hora)
  } else {
    hora <- untable(temp, num = temp$pax_total) %>%
      select(-pax_total) %>%
      mutate(hora_circular = circular(hora, template = "clock24", units = "hours")) %>%
      summarise(hora = median(hora_circular)) %>%
      pull(hora)
  }
  
  as.numeric(hora) %% 24
}
```

Función para generar un gráfico circular de la cantidad de pasajeros por
hora por línea de subte:

``` r
plot_clock <- function(.linea, .df, .color = 'black', .hora_promedio) {
  .df %>%
    filter(linea == .linea) %>%
    group_by(hora) %>%
    summarise(pax_total = sum(pax_total)) %>%
    ungroup() %>%
    ggplot(aes(x = hora, y = pax_total)) +
    geom_col(color = 'white', fill = 'lightgrey') +
    coord_polar(start = 0) +
    scale_x_continuous(
      "",
      limits = c(0, 24),
      breaks = seq(0, 24),
      labels = seq(0, 24)
    ) +
    geom_vline(xintercept = .hora_promedio,
               color = .color,
               size = 2) +
    labs(y = 'Pasajeros') +
    theme_minimal() +
    theme(text = element_text(size = 25))
}
```

Se genera el tibble que contiene los datos para luego generar la tabla:

``` r
datos_tabla <- tibble(linea = sort(unique(df$linea))) %>% 
  
  # Colores
  mutate(recorrido = case_when(linea == 'LineaA'~'Plaza de Mayo - San Pedrito',
                           linea == 'LineaB'~'J.M. Rosas - L.N. Alem',
                           linea == 'LineaC'~'Constitución - Retiro',
                           linea == 'LineaD' ~'Congreso de Tucumán - Catedral',
                           linea == 'LineaE' ~'Retiro - Plaza de los Virreyes',
                           linea == 'LineaH' ~ 'Hospitales - Facultad de Derecho',
                           TRUE ~ 'black')) %>%
  
  
  # Colores
  mutate(color = case_when(linea == 'LineaA'~'#18cccc',
                           linea == 'LineaB'~'#eb0909',
                           linea == 'LineaC'~'#233aa8',
                           linea == 'LineaD' ~'#02db2e',
                           linea == 'LineaE' ~'#c618cc',
                           linea == 'LineaH' ~ '#ffdd00',
                           TRUE ~ 'black')) %>%
  
  # Cantidad de viajes realizados en cada línea
  left_join(df %>% 
    group_by(linea) %>% 
    summarise(pax_total = sum(pax_total))) %>%
  
  # Hora promedio por línea
  mutate(hora_promedio = map(linea, ~get_hour(.linea=.x, .df=df))) %>% 

  mutate(hora_promedio = unlist(hora_promedio)) %>% 
  
  
  # Gráfico de cantidad de pasajeros por hora por línea
  mutate(reloj_plot = pmap(
    list(linea, color, hora_promedio),
    ~ plot_clock(
      .linea = ..1,
      .df = df,
      .color = ..2,
      .hora_promedio = ..3
    )
  )) %>%
  
  # Gráfico de la evolución de cantidad de pasajeros por línea
  mutate(
    evolucion_plot = map2(linea, color,
      ~ df %>% filter(linea == .x) %>%
          group_by(fecha) %>%
          summarise(n = n()) %>%
          ggplot(aes(x = fecha, y = n)) +
          geom_line(color='grey', size=2.5)+
          geom_line(color=.y, size=1.5)+
          theme_minimal()+
          labs(x='Fecha',y='Cantidad')+
          theme(text=element_text(size=30),
                 panel.grid = element_blank())
    )
  ) %>% 
  mutate(linea_imagen = here::here('',paste0('gcba_subte/lineas/',
                                             tolower(linea), 
                                             '.jpg')))
```

# Generación de la tabla

Se utiliza el paquete {gt} para generar la tabla que contiene plots de
{ggplot2}

``` r
tabla<-datos_tabla %>% 
  ungroup() %>% 
  select(linea, linea_imagen, recorrido, pax_total, 
         hora_promedio, reloj_plot, evolucion_plot) %>% 
  
  # Se genera la tabla
  gt() %>% 
  
  
  # Colores de los textos
  tab_style(
    style = cell_text(color = "#18cccc"),
    locations = cells_body(columns = c(recorrido),
                           rows = linea == "LineaA")
  ) %>%
  tab_style(
    style = cell_text(color = "#eb0909"),
    locations = cells_body(columns = c(recorrido),
                           rows = linea == "LineaB")
  ) %>%
  tab_style(
    style = cell_text(color = "#233aa8"),
    locations = cells_body(columns = c(recorrido),
                           rows = linea == "LineaC")
  ) %>%
  tab_style(
    style = cell_text(color = "#02db2e"),
    locations = cells_body(columns = c(recorrido),
                           rows = linea == "LineaD")
  ) %>%
  tab_style(
    style = cell_text(color = "#c618cc"),
    locations = cells_body(columns = c(recorrido),
                           rows = linea == "LineaE")
  ) %>%
  tab_style(
    style = cell_text(color = "#ffdd00"),
    locations = cells_body(columns = c(recorrido),
                           rows = linea == "LineaH")
  ) %>% 
  
  cols_hide(linea) %>% 


  
  text_transform(
    locations = cells_body(columns = c(linea_imagen)),
    fn = function(linea_imagen) {
      lapply(linea_imagen, local_image, height=20)
    }
  ) %>% 
  cols_label(linea_imagen = '') %>% 
  
  # Ggplots en formato gráfico (sino aparecen como texto)
  text_transform(
    locations = cells_body(columns=reloj_plot),
    fn = function(x){
      map(datos_tabla$reloj_plot, gt::ggplot_image, 
          height=px(180), aspect_ratio=2)
    }
  ) %>% 
  
  text_transform(
    locations = cells_body(columns=evolucion_plot),
    fn = function(x){
      map(datos_tabla$evolucion_plot, gt::ggplot_image, 
          height=px(150), aspect_ratio=2)
    }
  ) %>% 
  
  fmt_number(hora_promedio) %>% 
  
  fmt_number(pax_total, suffixing = TRUE) %>% 
  
  cols_label(
    recorrido = md('Recorrido'),
    hora_promedio = md("Hora promedio"),
    reloj_plot = md('Pasajeros por hora'),
    # reloj_plot = gt::html(
    #   "<span style='color:#grey'>Pasajeros por hora</span>"),
    pax_total = md('Total pasajeros'),
    evolucion_plot = 'Pasajeros por día'
  )%>%
  
  # Fondo gris en el recorrido
  tab_style(
    style = list(cell_fill(color = "#f0f0f0")),
    locations = cells_body(columns = c('recorrido'))
  )%>%
  
  
  # Opciones de la tabla
  tab_options(
    data_row.padding = px(0),
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table_body.border.top.style = "solid",
    column_labels.border.bottom.style = "solid"
  ) %>% 
  
  tab_header(title=md('**Uso del subte en la Ciudad Autónoma de Buenos Aires**'), 
             subtitle='Período de análisis: Noviembre 2021') %>% 
  
  opt_align_table_header('left') %>% 
  
  tab_footnote(cells_column_labels(columns = reloj_plot), footnote = 'La línea de color representa el horario promedio considerando la distribución circular de la variable hora.') %>% 
  
  tab_source_note('Elaboración propia en base a datos del Portal de Datos Abiertos de la Ciudad Autónoma de Buenos Aires') %>% 
  
  # Título
  tab_style(
    locations = cells_title(groups = 'title'),
    style = list(
      cell_text(
        font=google_font(name = 'Raleway'), 
        size='xx-large',weight='bold',align='left',
        color='#383838'
  )))%>%
  
  # Subtítulo
  tab_style(
    locations = cells_title(groups = 'subtitle'),
    style = list(
      cell_text(
        font=google_font(name = 'Raleway'), 
        size='small',align='left'
  ))) %>% 
  
  # Alineación 
  cols_align('center',  columns = c('pax_total', 'hora_promedio', 
                                    'reloj_plot', 'evolucion_plot')) 
```

# Guardar la tabla

Se guarda la tabla:

``` r
gt::gtsave(tabla, 'tabla_subtes.png')
```

![](gcba_subte_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->