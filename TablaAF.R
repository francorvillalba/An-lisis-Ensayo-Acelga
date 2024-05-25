#Tabla para presentación Area foliar

AreaFoliarE1<-readRDS(file = "Resultados/Afoliar1_v2.rds")
AreaFoliarE2<-readRDS(file = "Resultados/Afoliar2.rds")

AFensayo1<-readRDS(file = "Resultados/AFensayo1.rds")
AFensayo2<-readRDS(file = "Resultados/AFensayo2.rds")

AFensayo1$temporada<-"Primavera/Verano"
AFensayo2$temporada<-"Otoño/Invierno"

tabla1<-data.frame( "temporada"=AFensayo1$temporada,
                    "efecto"= AFensayo1$factor,
                    "nivel"= AFensayo1$Nivel,
                    "fecha"= AFensayo1$fecha,
                    "AF"= AFensayo1$media,
                    "EE"=AFensayo1$EE)
tabla2<-data.frame( "temporada"=AFensayo2$temporada,
                    "efecto"= AFensayo2$factor,
                    "nivel"= AFensayo2$Nivel,
                    "fecha"= AFensayo2$fecha,
                    "AF"= AFensayo2$media,
                    "EE"=AFensayo2$EE)

library(tidyverse)

tabla_ancha<-pivot_wider(data=tabla1, 
                         id_cols= c(temporada, efecto, nivel), 
                         names_from= fecha, 
                         values_from=c(AF, EE))
tabla_ancha2<-pivot_wider(data=tabla2, 
                          id_cols= c(temporada, efecto, nivel), 
                          names_from= fecha, 
                          values_from=c(AF, EE))
agregarFilas<-data.frame( temporada = c("Primavera/Verano", "Primavera/Verano", "Primavera/Verano", "Primavera/Verano", "Primavera/Verano"), 
                          efecto = c(" x̅", "CV", "p-valor", "p-valor", "p-valor"), 
                          nivel =c(" x̅", "CV", "Dosis abono", "Variedad", "Dosis*Variedad"),
                          AF1 =c(mean(AreaFoliarE1$AF1), 39.38, 0.41269, 0.21107, 0.04254),
                          AF2 =c(mean(AreaFoliarE1$AF2), 43.77, 0.98704, 0.00109, 0.61713),
                          AF3 =c(mean(AreaFoliarE1$AF3), 30.16, 0.20356, 0.00958, 0.59339),
                          AF4 = c(mean(AreaFoliarE1$AF4), 29.52, 0.09451, 0.53305, 0.82409), 
                          EE1 = c(NA, NA, NA, NA, NA), 
                          EE2 = c(NA, NA, NA, NA, NA), 
                          EE3 = c(NA, NA, NA, NA, NA),
                          EE4 = c(NA, NA, NA, NA, NA))

colnames(tabla_ancha)<-colnames(agregarFilas)
tabla_ancha<-rbind(tabla_ancha, agregarFilas)

agregarFilas2<-data.frame( temporada = c("Otoño/Invierno", "Otoño/Invierno", "Otoño/Invierno", "Otoño/Invierno", "Otoño/Invierno"), 
                           efecto = c(" x̅", "CV", "p-valor", "p-valor", "p-valor"), 
                           nivel =c(" x̅", "CV", "Dosis abono", "Variedad", "Dosis*Variedad"),
                           AF1 =c(mean(AreaFoliarE2$AF1), 48.66, 0.7225, 0.0333, 0.7696),
                           AF2 =c(mean(AreaFoliarE2$AF2), 40.14, 0.21691, 0.03376, 0.389),
                           AF3 =c(mean(AreaFoliarE2$AF3), 105.29, 0.06987, 0.45323, 0.08908),
                           AF4 = c(mean(AreaFoliarE2$AF4), 52.15, 0.54410, 0.03495, 0.32586), 
                           EE1 = c(NA, NA, NA, NA, NA), 
                           EE2 = c(NA, NA, NA, NA, NA), 
                           EE3 = c(NA, NA, NA, NA, NA),
                           EE4 = c(NA, NA, NA, NA, NA))


colnames(tabla_ancha2)<-colnames(agregarFilas2)

tabla_ancha2<-rbind(tabla_ancha2, agregarFilas2)

tabla_ancha<-rbind(tabla_ancha, tabla_ancha2)

View(tabla_ancha)
install.packages("flextable")
library(flextable)


library(dplyr)
library(officer)
library(tidyverse)
library(flextable)

tp<-  flextable(data= tabla_ancha, 
                col_keys= c("temporada", "efecto", "nivel", "AF1", "AF2", "AF3", "AF4"), 
)%>% compose(i=c(1:6, 12:17), 
             j= "AF1", 
             value= as_paragraph(
               as_chunk(format(AF1, digits=2), props= fp_text(color="black", font.size=8) ),
               " ± ", 
               as_chunk(format(EE1, digits=2), props= fp_text(color="black", italic=TRUE, font.size=8))),
             part="body" ) %>%
  compose(i=c(1:6, 12:17), 
          j= "AF2", 
          value= as_paragraph(
            as_chunk(format(AF2, digits=2), props= fp_text(color="black", font.size=8)),
            " ± ", 
            as_chunk(format(EE2, digits=2), props= fp_text(color="black", italic=TRUE, font.size=8))),
          part="body" )%>% 
  compose(i=c(1:6, 12:17), 
          j= "AF3", 
          value= as_paragraph(
            as_chunk(format(AF3, digits=2), props= fp_text(color="black", font.size=8)),
            " ± ", 
            as_chunk(format(EE3, digits=2), props= fp_text(color="black", italic=TRUE, font.size=8))),
          part="body" ) %>% compose(i=c(1:6, 12:17), 
                                    j= "AF4", 
                                    value= as_paragraph(
                                      as_chunk(format(AF4, digits=2), props= fp_text(color="black", font.size=8)),
                                      " ± ", 
                                      as_chunk(format(EE4, digits=2), props= fp_text(color="black", italic=TRUE, font.size = 8))),
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
              MS1 = "AF",
              MS2 = "",
              MS3 = "",
              MS4 = "") %>% 
  merge_at(i=1, j=2:3, part="header") %>% 
  merge_at(i=2, j=4:7, part="header")

tp<-compose(tp, i=2, j=4, 
            value= as_paragraph(
              "Area Foliar (cm", 
              as_sup("2"),
              ")"
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
save_as_docx("Area Foliar"= tp, path= "Resultados/AreaFoliar.docx")
