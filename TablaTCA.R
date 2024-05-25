### Tabla para presentación Tasa de Crecimiento Absoluta

CrecimientoE1<-readRDS(file = "Resultados/CrecimientoE1_v2.rds")
CrecimientoE2<-readRDS(file = "Resultados/CrecimientoE2_v2.rds")

TCAensayo1<-readRDS(file = "Resultados/TCAensayo1.rds")
TCAensayo2<-readRDS(file = "Resultados/TCAensayo2.rds")

TCAensayo1$temporada<-"Primavera/Verano"
TCAensayo2$temporada<-"Otoño/Invierno"

tabla1<-data.frame( "temporada"=TCAensayo1$temporada,
                   "efecto"= TCAensayo1$factor,
                   "nivel"= TCAensayo1$nivel,
                   "fecha"= TCAensayo1$fecha,
                   "TCA"= TCAensayo1$media,
                   "EE"=TCAensayo1$EE)
tabla2<-data.frame( "temporada"=TCAensayo2$temporada,
                    "efecto"= TCAensayo2$factor,
                    "nivel"= TCAensayo2$nivel,
                    "fecha"= TCAensayo2$fecha,
                    "TCA"= TCAensayo2$media,
                    "EE"=TCAensayo2$EE)

library(tidyverse)

tabla_ancha<-pivot_wider(data=tabla1, 
                         id_cols= c(temporada, efecto, nivel), 
                         names_from= fecha, 
                         values_from=c(TCA, EE))
tabla_ancha2<-pivot_wider(data=tabla2, 
                          id_cols= c(temporada, efecto, nivel), 
                          names_from= fecha, 
                          values_from=c(TCA, EE))
agregarFilas<-data.frame( temporada = c("Primavera/Verano", "Primavera/Verano", "Primavera/Verano", "Primavera/Verano", "Primavera/Verano"), 
                          efecto = c("Media", "CV", "p-valor", "p-valor", "p-valor"), 
                          nivel =c("Media", "CV", "dosis abono", "variedad", "dosis*variedad"),
                          TCA1 =c(mean(CrecimientoE1$TCA1), 62.95, 0.56836, 0.38565, 0.09082),
                          TCA2 =c(mean(CrecimientoE1$TCA2), 47.18, 0.87827, 0.01117, 0.52060),
                          TCA3 =c(mean(CrecimientoE1$TCA3), 49.99, 0.08878, 0.01668, 0.76183),
                          TCA4 = c(mean(CrecimientoE1$TCA4), 74.76, 0.97600, 0.09586, 0.89188), 
                          EE1 = c(NA, NA, NA, NA, NA), 
                          EE2 = c(NA, NA, NA, NA, NA), 
                          EE3 = c(NA, NA, NA, NA, NA),
                          EE4 = c(NA, NA, NA, NA, NA))

colnames(tabla_ancha)<-colnames(agregarFilas)
tabla_ancha<-rbind(tabla_ancha, agregarFilas)

agregarFilas2<-data.frame( temporada = c("Otoño/Invierno", "Otoño/Invierno", "Otoño/Invierno", "Otoño/Invierno", "Otoño/Invierno"), 
                           efecto = c("media", "CV", "p-valor", "p-valor", "p-valor"), 
                           nivel =c("media", "CV", "dosis abono", "variedad", "dosis*variedad"),
                           TCA1 =c(mean(CrecimientoE2$TCA1), 49.79, 0.50570, 0.06488, 0.85820),
                           TCA2 =c(mean(CrecimientoE2$TCA2), 43.06, 0.20870, 0.00130, 0.58012),
                           TCA3 =c(mean(CrecimientoE2$TCA3), 141.23, 0.84369, 0.09034, 0.61232),
                           TCA4 = c(mean(CrecimientoE2$TCA4), 93.88, 0.792, 0.01726, 0.71802), 
                           EE1 = c(NA, NA, NA, NA, NA), 
                           EE2 = c(NA, NA, NA, NA, NA), 
                           EE3 = c(NA, NA, NA, NA, NA),
                           EE4 = c(NA, NA, NA, NA, NA))


colnames(tabla_ancha2)<-colnames(agregarFilas2)

tabla_ancha2<-rbind(tabla_ancha2, agregarFilas2)

tabla_ancha<-rbind(tabla_ancha, tabla_ancha2)

install.packages("flextable")
library(flextable)


library(dplyr)
library(officer)
library(tidyverse)
library(flextable)

tp<-  flextable(data= tabla_ancha, 
                col_keys= c("temporada", "efecto", "nivel", "TCA1", "TCA2", "TCA3", "TCA4"), 
)%>% compose(i=c(1:6, 12:17), 
             j= "TCA1", 
             value= as_paragraph(
               as_chunk(format(TCA1, digits=2), props= fp_text(color="black") ),
               " ± ", 
               as_chunk(format(EE1, digits=2), props= fp_text(color="black", italic=TRUE))),
             part="body" ) %>%
  compose(i=c(1:6, 12:17), 
          j= "TCA2", 
          value= as_paragraph(
            as_chunk(format(TCA2, digits=2), props= fp_text(color="black")),
            " ± ", 
            as_chunk(format(EE2, digits=2), props= fp_text(color="black", italic=TRUE))),
          part="body" )%>% 
  compose(i=c(1:6, 12:17), 
          j= "TCA3", 
          value= as_paragraph(
            as_chunk(format(TCA3, digits=2), props= fp_text(color="black")),
            " ± ", 
            as_chunk(format(EE3, digits=2), props= fp_text(color="black", italic=TRUE))),
          part="body" ) %>% compose(i=c(1:6, 12:17), 
                                    j= "TCA4", 
                                    value= as_paragraph(
                                      as_chunk(format(TCA4, digits=2), props= fp_text(color="black")),
                                      " ± ", 
                                      as_chunk(format(EE4, digits=2), props= fp_text(color="black", italic=TRUE))),
                                    part="body" )

tp<-colformat_double(tp, i=c(9:11, 20:22), j= c(4:7), digits= 4)
tp<-colformat_double(tp, i=c(7:8, 18:19), j=4:7, digits=2)

tp<-tp %>% add_header_row(
  top= TRUE, 
  values= c("Temporada",
            "Tratamientos",
            "",
            "Muestreo 1",
            "Muestreo 2",
            "Muestreo 3", 
            "Muestreo 4" )) %>% set_header_labels(
              temporada= "Temporada",
              efecto = "Factor", 
              nivel= "Nivel",
              MS1 = "TCA1",
              MS2 = "",
              MS3 = "",
              MS4 = "") %>% 
  merge_at(i=1, j=2:3, part="header") %>% 
  merge_at(i=2, j=4:7, part="header")

tp<-compose(tp, i=2, j=4, 
                   value= as_paragraph(
                     "Tasa de Crecimiento Absoluta", " (g día", as_sup("-1"), ")"
                   ), 
                   part="header")
  
  
  
tp<-tp %>% merge_at(i=7, j=2:3, part="body")%>%
  merge_at(i=8, j=2:3, part="body")%>%
  merge_at(i=18, j=2:3, part="body")%>%
  merge_at(i=19, j=2:3, part="body")

tp


tp<- tp %>% merge_at(i=1:11, j=1, part="body") %>% 
  merge_at(i=12:22, j=1, part="body") %>% 
  merge_at(i=1:2, j=1, part="header")%>%
  merge_v(j=2, part="body")

tp

tp <- tp %>% flextable::align(align="left", part="all")
tp<- tp %>% flextable::align(align="center", i=c(7:8, 18:19), j=2:3, part="body")
tp<-tp %>% fontsize(size = 8, part = "all")%>%
  bold( part="header")

tp<-hline(tp, border= fp_border(color="gray", style="solid", width=1), i=c(4, 6, 8, 11, 15, 17, 19, 22), part="body")

tp<-compose(tp, i=c(1,12), j=2, 
            value= as_paragraph(
              as_chunk(efecto), " (g m", as_sup("-2"), ")"
            ), 
            part="body")
tp
save_as_docx("Materia Seca"= tp, path= "Resultados/TasaAbsoluta.docx")
