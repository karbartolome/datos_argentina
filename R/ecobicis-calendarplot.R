library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(xts)

# Datos de Eco Bicis - recorridos 2019 para tomar un año completo ---------------
rm(list=ls())
df=read.csv('recorridos-realizados-2019.csv')

# Procesamiento -----------------------------------------------------------
# Me basé en este ejemplo: https://vietle.info/post/calendarheatmap/

dfPlot  = df %>% 
  separate(duracion_recorrido, into=c('duration_dias','duration_string','duration_horas'), sep=" ") %>% 
  select(-duration_dias, -duration_string) %>% 
  mutate(duracion_minutos = 60*hour(as.POSIXct(duration_horas, format="%H:%M:%S"))+
                            minute(as.POSIXct(duration_horas, format="%H:%M:%S"))) %>% 
  filter(!is.na(fecha_origen_recorrido)) %>% 
  select(fecha=fecha_origen_recorrido, 
         duracion = duracion_minutos, 
         edad = edad_usuario
  ) %>% 
  mutate(fecha = as.Date(fecha, format = "%Y-%m-%d")) %>% 
  group_by(fecha) %>% 
  summarise(n=n(), 
            duracion_prom=mean(duracion, na.rm = TRUE), 
            duracion_median = median(duracion, na.rm = TRUE),
            duracion_sum=sum(duracion, na.rm = TRUE), 
            edad_prom = mean(edad, na.rm = TRUE), 
            edad_mediana = median(edad, na.rm = TRUE)) %>% 
  ungroup() 

# Días faltantes si los hubiera
fechas  = tibble(fecha = seq(
    dmy("01/01/2019"),
    dmy("31/12/2019"),
    "days"
  ))

# Unión
dfPlot = merge(dfPlot, fechas,  by='fecha', all.x=TRUE, all.y=TRUE)

# Fill valores faltantes:
#dfPlot$duracion_prom2=na.locf(dfPlot$duracion_prom, fromLast = FALSE) 
#dfPlot$n2=na.locf(dfPlot$n, fromLast = FALSE)   

# Variables de tiempo:
dfPlot = dfPlot %>% mutate(
  weekday = wday(fecha, label = T, week_start = 7), 
  month = month(fecha, label = T),
  date = yday(fecha),
  week = epiweek(fecha)
  )

# isoweek makes the last week of the year as week 1, so need to change that to week 53 for the plot
dfPlot$week[dfPlot$month=="Dec" & dfPlot$week ==1] = 53 

dfPlot = dfPlot %>% 
  group_by(month) %>% 
  mutate(monthweek = 1 + week - min(week))

# Plots -------------------------------------------------------------------

# Calendar plot

g1 = dfPlot %>%
  filter(!is.na(fecha)) %>% 
  ggplot(aes(weekday,-week, fill = duracion_prom)) +
  geom_tile(colour = "white")  + 
  geom_text(aes(label = day(fecha)), size = 2.5, color = "black") +
  theme(aspect.ratio = 1/2,
        legend.position = "top",
        legend.key.width = unit(3, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.title.align = 0.5,
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 15),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        plot.title = element_text(hjust = 0.5, size = 21, face = "bold",
                                  margin = margin(0,0,0.5,0, unit = "cm"))) +
  scale_fill_gradientn(colours = c("white", "#C2D991", "#8BBF56"),
                       na.value = 'lightgrey',
                       name = "Duración promedio (minutos)",
                       guide = guide_colorbar(title.position = "top", 
                                              direction = "horizontal")) +
  facet_wrap(~month, nrow = 3, ncol = 4, scales = "free") +
  labs(title = "Duración promedio de viajes en Eco Bicis (2019)", 
       caption='@karbartolome')

g1

# Series temporales

num=1000
cols = c('#7EC8D9', '#8BBF56')

g2=ggplot (dfPlot %>% filter(!is.na(fecha) & !is.na(duracion_prom)), aes(x=fecha, y=duracion_prom, group=1))+
  geom_path(color=cols[2])+
  geom_line(aes(y = n/num), color = cols[1]) + 
  scale_y_continuous(name = "Duración promedio (minutos)", 
                     sec.axis = sec_axis(~.*num, name = "Cantidad")) + 
  labs(x='',
       title ='Duración y cantidad de viajes en Eco Bicis (2019)', 
       caption='@karbartolome')+
  theme(aspect.ratio = 1/2,
        legend.position = "none",
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y.right=element_text(colour=cols[1]),
        axis.title.y.right=element_text(colour=cols[1]),
        axis.text.y=element_text(colour=cols[2]),
        axis.title.y=element_text(colour=cols[2]),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 15),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        plot.title = element_text(hjust = 0.5, size = 21, face = "bold",
                                  margin = margin(0,0,0.5,0, unit = "cm"))) 

g2


library(gridExtra)
grid.arrange(g1,g2)

