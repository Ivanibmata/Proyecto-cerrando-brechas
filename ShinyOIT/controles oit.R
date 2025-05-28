
library("viridis")
library("wesanderson")
library(ggtext)
library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)
library(readxl)


library(RColorBrewer)
par(mar=c(3,4,2,2))
display.brewer.all()
library(plotly)
library(sf)

excelcovid <- read_excel("Entidades_covid.xlsx")

base <- clean_names(excelcovid)

base <- base %>%
  filter(clasifiacion_oit %in% c("Ajuste presupuestario", "Nuevo programa o beneficio", "Programa ajustado", "NA"))



base$nument <-  factor(base$entidad_federativa,
                       labels = c(1:32))

###grafica 5----

gen_grafica <- function(ent_seleccionada){
  bd_i <- base %>%
    filter(nument == ent_seleccionada & poblacion_objetivo != "NA") %>%
    count(poblacion_objetivo)

plt <- bd_i %>%
    ggplot()+
    geom_col(aes(poblacion_objetivo, y = n, fill = poblacion_objetivo,
                 text = str_c(
 "<b>Número de programas: </b>", n )))+
    guides(fill=guide_legend(title="Población objetivo"))+
    scale_fill_manual(values = wes_palette("FantasticFox1", 22, type = "continuous"))+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
    coord_flip()+
    labs(title = "Número de programas aplicados por población objetivo", face= "bold",
         subtitle = "",
         x= "",
         y = "",
         caption = "Fuente: Coneval 2021a y Cejudo et al. 2020")+
    theme(axis.text.y = element_text(angle = 0, size = 8, family="Times New Roman", colour = "black"))+
    theme(axis.title = element_text(family = "Times New Roman", colour = "black"))+
    theme(plot.title = element_text(hjust = 0, face = "bold", family="Times New Roman", colour = "black"),
          plot.subtitle = element_text(hjust = 0.01, face = "bold", family="Times New Roman", colour = "black"),
          plot.caption = element_text(hjust = 1, family="Times New Roman", colour = "black"))+
    theme(legend.position = "none")+
    theme(plot.title = element_markdown(size = 12,
                                        color = "black",
                                        face = "bold"),
          plot.subtitle = element_markdown(size = 8,
                                           color = "black",
                                           face = "bold"),
          axis.text.x = element_blank(),
          legend.title=element_text(size=9, family="Times New Roman", colour = "black"),
          legend.text=element_text(size=8, family="Times New Roman", colour = "black"),
          plot.caption = element_markdown(color = "black"),
          plot.background = element_rect(fill = "gray82"),
          panel.background = element_rect(fill = "gray85"),
          legend.background = element_rect(fill = "gray81"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y  = element_blank(),
          panel.grid.major.y = element_line(color = "gray70"))

plt_2 <- ggplotly(plt, tooltip = "text")

return(plt_2)


}
gen_grafica(ent_seleccionada = 9)



opciones_indicador <-  base$nument
names(opciones_indicador) <- base$entidad_federativa

gen_grafica(ent_seleccionada = 7)


### grafica 2----

gen_grafica2 <- function(ent_seleccionada){
  bd_i <- base %>%
    filter(nument == ent_seleccionada & clasifiacion_oit != "NA") %>%
    count(clasifiacion_oit)

plt <-   bd_i %>%
    ggplot()+
    geom_col(aes(x= fct_rev(fct_reorder(clasifiacion_oit, desc(n))), y = n, fill = clasifiacion_oit,
                 text = str_c(
                   "<b>Número de programas: </b>", n )))+
    guides(fill=guide_legend(title="Tipo de ajuste"))+
    scale_fill_manual(values = wes_palette("FantasticFox1", 3, type = "discrete"))+
    coord_flip()+
    labs(title = "Programas por tipo de ajuste", face= "bold",
         subtitle = "",
         x= " ",
         y = "",
         caption = "Fuente: Coneval 2021a y Cejudo et al. 2020")+
    theme(axis.title = element_text(family = "Times New Roman", colour = "black"))+
    theme(axis.text.y = element_text(angle = 0, size = 8, family="Times New Roman", colour = "black"))+
    theme(legend.position = "none")+
    theme(plot.title = element_text(hjust = 0, face = "bold", family="Times New Roman", colour = "black"),
          plot.subtitle = element_text(hjust = 0.01, face = "bold", family="Times New Roman", colour = "black"),
          plot.caption = element_text(hjust = 1, family="Times New Roman", colour = "black"))+
    theme(plot.title = element_markdown(size = 12,
                                        color = "black",
                                        face = "bold"),
          axis.text.x = element_blank(),
          plot.subtitle = element_markdown(size = 8,
                                           color = "black",
                                           face = "bold"),
          legend.title=element_text(size=9),
          legend.text=element_text(size=8),
          plot.caption = element_markdown(color = "black"),
          plot.background = element_rect(fill = "gray82"),
          panel.background = element_rect(fill = "gray85"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y  = element_blank(),
          panel.grid.major.y = element_line(color = "gray70"))

plt2 <- ggplotly(plt, tooltip = "text")

return(plt2)

}


gen_grafica2(4)

###grafica 4----

gen_grafica4 <- function(ent_seleccionada){
  bd_i <- base %>%
    filter(nument == ent_seleccionada & funcion_de_proteccion_social != "NA") %>%
    count(funcion_de_proteccion_social)

plt <- bd_i %>%
  ggplot()+
  geom_col(aes(x= fct_rev(fct_reorder(funcion_de_proteccion_social, desc(n))), y = n, fill = funcion_de_proteccion_social,
               text = str_c(
                 "<b>Número de programas: </b>", n )))+
  scale_y_continuous(breaks = c(0, 15))+
  guides(fill=guide_legend(title="Tipo de ajuste"))+
  scale_fill_manual(values = wes_palette("FantasticFox1", 13, type = "continuous"))+
  coord_flip()+
  labs(title = "Clasificación de programas por función de protección social", face= "bold",
       subtitle = "",
       x= " ",
       y = "",
       caption = "Fuente: Coneval 2021a y Cejudo et al. 2020")+
  theme(axis.title = element_text(family = "Times New Roman", colour = "black"))+
  theme(axis.text.y = element_text(angle = 0, size = 7, family="Times New Roman", colour = "black"))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 1, face = "bold", family="Times New Roman", colour = "black"),
        plot.subtitle = element_text(hjust = 0.01, face = "bold", family="Times New Roman", colour = "black"),
        plot.caption = element_text(hjust = 1, family="Times New Roman", colour = "black"))+
  theme(plot.title = element_markdown(size = 12,
                                      color = "black",
                                      face = "bold"),
        plot.subtitle = element_markdown(size = 8,
                                         color = "black",
                                         face = "bold"),
        axis.text.x = element_blank(),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        plot.caption = element_markdown(color = "black"),
        plot.background = element_rect(fill = "gray82"),
        panel.background = element_rect(fill = "gray85"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y  = element_blank(),
        panel.grid.major.y = element_line(color = "gray70"))

plt2 <- ggplotly(plt, tooltip = "text")

return(plt2)
}

gen_grafica4(ent_seleccionada = 7)

###grafica 5 ----

gen_grafica5 <- function(ent_seleccionada){
  bd_i <- base %>%
    filter(nument == ent_seleccionada & poblacion_objetivo != "NA") %>%
    count(poblacion_objetivo)

plt <- bd_i %>%
  ggplot()+
  geom_col(aes(poblacion_objetivo, y = n, fill = poblacion_objetivo,
               text = str_c(
                 "<b>Número de programas: </b>", n )))+
  guides(fill=guide_legend(title="Población objetivo"))+
  scale_fill_manual(values = wes_palette("FantasticFox1", 22, type = "continuous"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
  coord_flip()+
  labs(title = "Número de programas aplicados por población objetivo", face= "bold",
       subtitle = "",
       x= "",
       y = "",
       caption = "Fuente: Coneval 2021a y Cejudo et al. 2020")+
  theme(axis.text.y = element_text(angle = 0, size = 8, family="Times New Roman", colour = "black"))+
  theme(axis.title = element_text(family = "Times New Roman", colour = "black"))+
  theme(plot.title = element_text(hjust = 0, face = "bold", family="Times New Roman", colour = "black"),
        plot.subtitle = element_text(hjust = 0.01, face = "bold", family="Times New Roman", colour = "black"),
        plot.caption = element_text(hjust = 1, family="Times New Roman", colour = "black"))+
  theme(legend.position = "none")+
  theme(plot.title = element_markdown(size = 12,
                                      color = "black",
                                      face = "bold"),
        plot.subtitle = element_markdown(size = 8,
                                         color = "black",
                                         face = "bold"),
        legend.title=element_text(size=9, family="Times New Roman", colour = "black"),
        legend.text=element_text(size=8, family="Times New Roman", colour = "black"),
        axis.text.x = element_blank(),
        plot.caption = element_markdown(color = "black"),
        plot.background = element_rect(fill = "gray82"),
        panel.background = element_rect(fill = "gray85"),
        legend.background = element_rect(fill = "gray81"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y  = element_blank(),
        panel.grid.major.y = element_line(color = "gray70"))

plt2 <- ggplotly(plt, tooltip = "text")

return(plt2)
}

gen_grafica5(ent_seleccionada = 7)

#grafica 6----

gen_grafica6 <- function(ent_seleccionada){
  bd_i <- base %>%
    filter(nument == ent_seleccionada & poblacion_objetivo != "NA" & funcion_de_proteccion_social != "NA") %>%
    group_by(poblacion_objetivo) %>%
    count(funcion_de_proteccion_social)


plt <- bd_i %>%
  group_by(poblacion_objetivo) %>%
  ggplot()+
  geom_col(aes(x= poblacion_objetivo, y = n, fill = funcion_de_proteccion_social,
               text = str_c(
                 "<b>Número de programas: </b>", n,
                 "<b> Función de protección social: </b>", funcion_de_proteccion_social)))+
  scale_fill_manual(values = wes_palette("FantasticFox1", 13, type = "continuous"), str_wrap(bd_i$funcion_de_proteccion_social, width = 25))+
  guides(fill=guide_legend(title="", ncol = 3))+
  coord_flip()+
  labs(title = "Funciones de protección social por poblaciones objetivo", face= "bold",
       subtitle = "",
       x= "",
       y = "",
       caption = "Fuente: Coneval 2021a y Cejudo et al. 2020")+
  theme(axis.text.y = element_text(angle = 0, size = 8, family="Times New Roman", colour = "black"))+
  theme(axis.title = element_text(family = "Times New Roman", colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", family="Times New Roman", colour = "black"),
        plot.subtitle = element_text(hjust = -0.0999999999, face = "bold", family="Times New Roman", colour = "black"),
        plot.caption = element_text(hjust = 1, family="Times New Roman", colour = "black"))+
  theme(plot.title = element_markdown(size = 12,
                                      color = "black",
                                      face = "bold"),
        plot.subtitle = element_markdown(size = 8,
                                         color = "black",
                                         face = "bold"),
        legend.title=element_text(size=9, family="Times New Roman", colour = "black"),
        axis.text.x = element_blank(),
        legend.text=element_text(size=6, family="Times New Roman", colour = "black"),
        plot.caption = element_markdown(color = "black"),
        plot.background = element_rect(fill = "gray82"),
        legend.position = "none",
        legend.key.height= unit(0.3, 'cm'),
        legend.key.width= unit(0.3, 'cm'),
        panel.background = element_rect(fill = "gray85"),
        legend.background = element_rect(fill = "gray81"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y  = element_blank(),
        panel.grid.major.y = element_line(color = "gray70"))

plt2 <- ggplotly(plt, tooltip = "text")

return(plt2)
}

gen_grafica6(ent_seleccionada = 5)

###grafica 8----

gen_grafica8 <- function(ent_seleccionada){
  bd_i <- base %>%
    filter(nument == ent_seleccionada & clasificacion_de_las_medidas != "NA") %>%
    count(clasificacion_de_las_medidas)

plt <- bd_i %>%
  ggplot()+
  geom_col(aes(x= fct_rev(fct_reorder(clasificacion_de_las_medidas, desc(n))), y = n, fill = clasificacion_de_las_medidas,
               text = str_c(
                 "<b>Número de programas: </b>", n )))+
  guides(fill=guide_legend(title="Tipo de ajuste"), labels = function(x) str_wrap(x, width = 25))+
  scale_fill_manual(values = wes_palette("FantasticFox1", 9, type = "continuous"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
  coord_flip()+
  labs(title = "Programas por tipo de medidas adoptadas", face= "bold",
       subtitle = "",
       x= " ",
       y = "",
       caption = "Fuente: Coneval 2021a y Cejudo et al. 2020")+
  theme(axis.title = element_text(family = "Times New Roman", colour = "black"))+
  theme(axis.text.y = element_text(angle = 0, size = 7, family="Times New Roman", colour = "black"))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0, face = "bold", family="Times New Roman", colour = "black"),
        plot.subtitle = element_text(hjust = 0.01, face = "bold", family="Times New Roman", colour = "black"),
        plot.caption = element_text(hjust = 1, family="Times New Roman", colour = "black"))+
  theme(plot.title = element_markdown(size = 12,
                                      color = "black",
                                      face = "bold"),
        plot.subtitle = element_markdown(size = 8,
                                         color = "black",
                                         face = "bold"),
        axis.text.x = element_blank(),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        plot.caption = element_markdown(color = "black"),
        plot.background = element_rect(fill = "gray82"),
        panel.background = element_rect(fill = "gray85"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y  = element_blank(),
        panel.grid.major.y = element_line(color = "gray70"))

plt2 <- ggplotly(plt, tooltip = "text")

return(plt2)
}

gen_grafica8(ent_seleccionada = 7)
