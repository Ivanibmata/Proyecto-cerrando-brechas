library(pacman)

library("viridis")
library("wesanderson")
library(ggtext)


p_load(ggplot2, dplyr, readxl, ggtext, tidyverse, lubridate, janitor, stringr)

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

base <- base %>%
  filter(clasifiacion_oit %in% c("Ajuste presupuestario", "Nuevo programa o beneficio", "Programa ajustado", "NA"))



base$nument <-  factor(base$entidad_federativa,
                       labels = c(1:32))

##grafica 4----

gen_grafica4 <- gen_grafica5 <- function(ent_seleccionada){
  bd_i <- base %>%
    filter(nument == ent_seleccionada & funcion_de_proteccion_social != "NA") %>%
    count(funcion_de_proteccion_social)

  subtitulo <- base %>%
    filter(nument == ent_seleccionada) %>%
    summarise(entidad_federativa[1])


  plt <- bd_i %>%
    ggplot()+
    geom_col(aes(x= fct_rev(fct_reorder(funcion_de_proteccion_social, desc(n))), y = n, fill = funcion_de_proteccion_social))+
    scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300))+
    guides(fill=guide_legend(title="Tipo de ajuste"))+
    scale_fill_manual(values = wes_palette("FantasticFox1", 13, type = "continuous"))+
    geom_text(aes(funcion_de_proteccion_social, y = n, label = n), vjust = 0.5, hjust = -0.3, family="Times New Roman", size = 3, fontface = "bold")+
    coord_flip()+
    labs(title = "Clasificación de programas por función de protección social", face= "bold",
         subtitle = subtitulo,
         x= " ",
         y = "",
         caption = "")+
    theme(axis.title = element_text(family = "Times New Roman", colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, size = 8, family="Times New Roman", colour = "black"))+
    theme(axis.text.y = element_text(angle = 0, size = 7, family="Times New Roman", colour = "black"))+
    theme(legend.position = "none")+
    theme(plot.title = element_text(hjust = 0.1, face = "bold", family="Times New Roman", colour = "black"),
          plot.subtitle = element_text(hjust = 0.01, face = "bold", family="Times New Roman", colour = "black"),
          plot.caption = element_text(hjust = 1, family="Times New Roman", colour = "black"))+
    theme(plot.title = element_markdown(size = 12,
                                        color = "black",
                                        face = "bold"),
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
  return(plt)
}

gen_grafica4(ent_seleccionada = 7)


###grafica 5----

gen_grafica5 <- function(ent_seleccionada){
  bd_i <- base %>%
    filter(nument == ent_seleccionada & poblacion_objetivo != "NA") %>%
    count(poblacion_objetivo)

  subtitulo <- base %>%
    filter(nument == ent_seleccionada) %>%
    summarise(entidad_federativa[1])


  plt <- bd_i %>%
    ggplot()+
    geom_col(aes(poblacion_objetivo, y = n, fill = poblacion_objetivo))+
    scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300))+
    guides(fill=guide_legend(title="Población objetivo"))+
    scale_fill_manual(values = wes_palette("FantasticFox1", 22, type = "continuous"))+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
    geom_text(aes(poblacion_objetivo, y = n, label = n), vjust = 0.7, hjust = -0.3, family="Times New Roman", size = 3, fontface = "bold")+
    coord_flip()+
    labs(title = "Número de programas aplicados por población objetivo", face= "bold",
         subtitle = subtitulo,
         x= "",
         y = "",
         caption = "")+
    theme(axis.text.y = element_text(angle = 0, size = 8, family="Times New Roman", colour = "black"))+
    theme(axis.title = element_text(family = "Times New Roman", colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, size = 10, family="Times New Roman", colour = "black"))+
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
          plot.caption = element_markdown(color = "black"),
          plot.background = element_rect(fill = "gray82"),
          panel.background = element_rect(fill = "gray85"),
          legend.background = element_rect(fill = "gray81"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y  = element_blank(),
          panel.grid.major.y = element_line(color = "gray70"))

  return(plt)
}
gen_grafica5(ent_seleccionada = 7)

#grafica 8----

gen_grafica8 <- function(ent_seleccionada){
  bd_i <- base %>%
    filter(nument == ent_seleccionada & clasificacion_de_las_medidas != "NA") %>%
    count(clasificacion_de_las_medidas)

  subtitulo <- base %>%
    filter(nument == ent_seleccionada) %>%
    summarise(entidad_federativa[1])


  plt <- bd_i %>%
    ggplot()+
    geom_col(aes(x= fct_rev(fct_reorder(clasificacion_de_las_medidas, desc(n))), y = n, fill = clasificacion_de_las_medidas))+
    scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300))+
    guides(fill=guide_legend(title="Tipo de ajuste"), labels = function(x) str_wrap(x, width = 25))+
    scale_fill_manual(values = wes_palette("FantasticFox1", 9, type = "continuous"))+
    geom_text(aes(clasificacion_de_las_medidas, y = n, label = n), vjust = 0.7, hjust = -0.3, family="Times New Roman", size = 3, fontface = "bold")+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
    coord_flip()+
    labs(title = "Programas por tipo de medidas adoptadas", face= "bold",
         subtitle = subtitulo,
         x= " ",
         y = "",
         caption = "")+
    theme(axis.title = element_text(family = "Times New Roman", colour = "black"))+
    theme(axis.text.x = element_text(angle = 90, size = 8, family="Times New Roman", colour = "black"))+
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
          legend.title=element_text(size=9),
          legend.text=element_text(size=8),
          plot.caption = element_markdown(color = "black"),
          plot.background = element_rect(fill = "gray82"),
          panel.background = element_rect(fill = "gray85"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y  = element_blank(),
          panel.grid.major.y = element_line(color = "gray70"))



  return(plt)
}

gen_grafica8(ent_seleccionada = 7)

###Anahi----

###ent1
gen_grafica4(ent_seleccionada = 1)

gen_grafica5(ent_seleccionada = 1)

gen_grafica8(ent_seleccionada = 1)

#ent2
gen_grafica4(ent_seleccionada = 2)

gen_grafica5(ent_seleccionada = 2)

gen_grafica8(ent_seleccionada = 2)

#ent3

gen_grafica4(ent_seleccionada = 3)

gen_grafica5(ent_seleccionada = 3)

gen_grafica8(ent_seleccionada = 3)

#ent4

gen_grafica4(ent_seleccionada = 4)

gen_grafica5(ent_seleccionada = 4)

gen_grafica8(ent_seleccionada = 4)

#ent5

gen_grafica4(ent_seleccionada = 5)

gen_grafica5(ent_seleccionada = 5)

gen_grafica8(ent_seleccionada = 5)

#ent6

gen_grafica4(ent_seleccionada = 6)

gen_grafica5(ent_seleccionada = 6)

gen_grafica8(ent_seleccionada = 6)


#ent7

gen_grafica4(ent_seleccionada = 7)

gen_grafica5(ent_seleccionada = 7)

gen_grafica8(ent_seleccionada = 7)

#ent8

gen_grafica4(ent_seleccionada = 8)

gen_grafica5(ent_seleccionada = 8)

gen_grafica8(ent_seleccionada = 8)

#ent9

gen_grafica4(ent_seleccionada = 9)

gen_grafica5(ent_seleccionada = 9)

gen_grafica8(ent_seleccionada = 9)

gen_grafica5(ent_seleccionada = 10)

gen_grafica5(ent_seleccionada = 11)

gen_grafica5(ent_seleccionada = 12)

gen_grafica4(ent_seleccionada = 13)

gen_grafica5(ent_seleccionada = 13)

gen_grafica8(ent_seleccionada = 13)

#ent14

gen_grafica4(ent_seleccionada = 14)

gen_grafica5(ent_seleccionada = 14)

gen_grafica8(ent_seleccionada = 14)

#ent15

gen_grafica4(ent_seleccionada = 15)

gen_grafica5(ent_seleccionada = 15)

gen_grafica8(ent_seleccionada = 15)

#ent16

gen_grafica4(ent_seleccionada = 16)

gen_grafica5(ent_seleccionada = 16)

gen_grafica8(ent_seleccionada = 16)

#ent17

gen_grafica4(ent_seleccionada = 17)

gen_grafica5(ent_seleccionada = 17)

gen_grafica8(ent_seleccionada = 17)

#ent18

gen_grafica4(ent_seleccionada = 18)

gen_grafica5(ent_seleccionada = 18)

gen_grafica8(ent_seleccionada = 18)

#ent19

gen_grafica4(ent_seleccionada = 19)

gen_grafica5(ent_seleccionada = 19)

gen_grafica8(ent_seleccionada = 19)

#ent20

gen_grafica4(ent_seleccionada = 20)

gen_grafica5(ent_seleccionada = 20)

gen_grafica8(ent_seleccionada = 20)

#ent21

gen_grafica4(ent_seleccionada = 21)

gen_grafica5(ent_seleccionada = 21)

gen_grafica8(ent_seleccionada = 21)

#ent22

gen_grafica4(ent_seleccionada = 22)

gen_grafica5(ent_seleccionada = 22)

gen_grafica8(ent_seleccionada = 22)

#ent23

gen_grafica4(ent_seleccionada = 23)

gen_grafica5(ent_seleccionada = 23)

gen_grafica8(ent_seleccionada = 23)

#ent24

gen_grafica4(ent_seleccionada = 24)

gen_grafica5(ent_seleccionada = 24)

gen_grafica8(ent_seleccionada = 24)

#ent25

gen_grafica4(ent_seleccionada = 25)

gen_grafica5(ent_seleccionada = 25)

gen_grafica8(ent_seleccionada = 25)

#ent26

gen_grafica4(ent_seleccionada = 26)

gen_grafica5(ent_seleccionada = 26)

gen_grafica8(ent_seleccionada = 26)

#ent27

gen_grafica4(ent_seleccionada = 27)

gen_grafica5(ent_seleccionada = 27)

gen_grafica8(ent_seleccionada = 27)

#ent28

gen_grafica4(ent_seleccionada = 28)

gen_grafica5(ent_seleccionada = 28)

gen_grafica8(ent_seleccionada = 28)

#ent29

gen_grafica4(ent_seleccionada = 29)

gen_grafica5(ent_seleccionada = 29)

gen_grafica8(ent_seleccionada = 29)

#ent30

gen_grafica4(ent_seleccionada = 30)

gen_grafica5(ent_seleccionada = 30)

gen_grafica8(ent_seleccionada = 30)

#ent31

gen_grafica4(ent_seleccionada = 31)

gen_grafica5(ent_seleccionada = 31)

gen_grafica8(ent_seleccionada = 31)

#ent32

gen_grafica4(ent_seleccionada = 32)

gen_grafica5(ent_seleccionada = 32)

gen_grafica8(ent_seleccionada = 32)
