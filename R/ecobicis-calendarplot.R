library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(biscale)
library(cowplot)
#library(xts)

# ----- Datos -----
rm(list=ls())

# Datos de: https://data.buenosaires.gob.ar/dataset/bicicletas-publicas
# Tomé 2019 para tener un año completo
df= read.csv('recorridos-realizados-2019.csv', stringsAsFactors = FALSE)

# ----- Procesamiento -----

df  = df %>% 
  # Proxy de minutos de duración del recorrido:
  separate(duracion_recorrido, into=c('duration_dias','duration_string','duration_horas'), sep=" ") %>% 
  mutate(duracion_minutos = 60*hour(as.POSIXct(duration_horas, format="%H:%M:%S"))+
                            minute(as.POSIXct(duration_horas, format="%H:%M:%S"))) %>% 
  # Renombro variables:
  select(fecha=fecha_origen_recorrido, 
         duracion = duracion_minutos, 
         edad = edad_usuario) %>% 
  # Formato de fecha:
  mutate(fecha = as.Date(fecha, format = "%Y-%m-%d")) %>% 
  # Cantidad de viajes, duración promedio, mediana y total por fecha
  group_by(fecha) %>% 
  summarise(n=n(), 
            duracion_prom=mean(duracion, na.rm = TRUE), 
            duracion_median = median(duracion, na.rm = TRUE),
            duracion_sum=sum(duracion, na.rm = TRUE)) %>% 
  ungroup() 

# Días faltantes si los hubiera
fechas  = tibble(fecha = seq(
  dmy("01/01/2019"),
  dmy("31/12/2019"),
  "days"
))

df = merge(df, fechas,  by='fecha', all.x=TRUE, all.y=TRUE)

# Variables temporales:
df = df %>% mutate(
  weekday = wday(fecha, label = T, week_start = 7), 
  month = month(fecha, label = T),
  date = yday(fecha),
  week = epiweek(fecha)
)

# La última week de dic queda como 1 entonces la pasamos a 53: 
df$week[df$month=="Dec" & df$week ==1] = 53 

# semana del mes
df = df %>% 
  # Hay una fecha faltante:
  filter(!is.na(fecha)) %>%
  group_by(month) %>% 
  mutate(monthweek = 1 + week - min(week))


# Escala bivariada
data_biscale = bi_class(df %>% filter(!is.na(duracion_prom) & !is.na(n)), 
                         x='n',
                         y='duracion_prom', 
                         style="quantile",dim=3)



# ----- Plots -----

# 1. Calendar plot

g1 = data_biscale %>%
  filter(!is.na(fecha)) %>% 
  ggplot(aes(weekday,-week, fill = bi_class)) +
  geom_tile(colour = "white")  + 
  geom_text(aes(label = day(fecha)), size = 2.5, color = "black") +
  bi_scale_fill(pal = "DkCyan", dim = 3) +
  facet_wrap(~month, nrow = 3, ncol = 4, scales = "free") +
  theme(aspect.ratio = 1/2,
        legend.position = "none",
        legend.key.width = unit(3, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=7),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.title.align = 0.5,
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 15),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        plot.title = element_text(hjust = 0, vjust=5, size = 14, face = "bold",
                                  margin = margin(0,0,0.5,0, unit = "cm")),
        plot.subtitle = element_text(hjust = 0, vjust=5, size = 10,
                                     margin = margin(0,0,0.5,0, unit = "cm"))
        
  )
  #labs(title='Duración y cantidad de viajes en EcoBicis 2019')  

legend <- bi_legend(pal = "DkCyan",
                    dim = 3,
                    xlab = "Cantidad",
                    ylab = "Duración promedio",
                    size = 7)
ggdraw() +
  draw_plot(g1, 0, 0, 1, 1) +
  draw_plot(legend, 0.75, 0.79, 0.2, 0.2)

# 2. Series de tiempo
num=1000
cols = c('#6C83B5', '#73AE80')

g2=ggplot (df %>% filter(!is.na(fecha) & !is.na(duracion_prom)), aes(x=fecha, y=duracion_prom, group=1))+
  geom_path(color=cols[2])+
  geom_line(aes(y = n/num), color = cols[1]) + 
  scale_y_continuous(name = "Duración promedio (minutos)", 
                     sec.axis = sec_axis(~.*num, name = "Cantidad")) + 
  labs(x='')+
  theme(aspect.ratio = 1/2,
        legend.position = "none",
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y.right=element_text(colour=cols[1]),
        axis.title.y.right=element_text(colour=cols[1], size=8),
        axis.text.y=element_text(colour=cols[2]),
        axis.title.y=element_text(colour=cols[2], size=8),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 15),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        plot.title = element_text(hjust = 0.5, size = 21, face = "bold",
                                  margin = margin(0,0,0.5,0, unit = "cm"))) 
  #labs(title='Duración y cantidad de viajes en EcoBicis en 2019')


# Uniendo todo:
grafico=ggdraw() +
  draw_plot(g1, x=0, y=0.29, width=1, height = 0.59, hjust=0) +
  draw_plot(legend, x=0.8, y=0.84, width=0.18, height=0.18)+
  draw_plot(g2, x=0, y=0, width=1, height=0.29, hjust=0)+
  draw_label(x=0.01,y=0.95, hjust=0, color = "black", size = 16,
             ' Cantidad y duración de viajes en EcoBicis (2019)')+
  draw_label(x=0.01, y=0.9, hjust=0, color='grey', size=12,
             ' Los colores representan mayor cantidad/duración en base a la escala \n de la derecha, donde cada variable está dividida en 3 cuantiles')+
  draw_label(x=0.7, y=0.02, color='grey', size=12,
             'Elaboración propia en base a datos del GCBA, @karbartolome')

ggsave("biscale-calendarplot.png", plot=last_plot(), width = 22, height = 22, units = "cm")
