#Tabla para presentación Area foliar

AreaFoliarE1<-readRDS(file = "Resultados/Afoliar1_v2.rds")
AreaFoliarE2<-readRDS(file = "Resultados/Afoliar2.rds")

TALensayo1<-readRDS(file = "Resultados/TALensayo1_v2.rds")
TALensayo2<-readRDS(file = "Resultados/TALensayo2.rds")

tabla1<-data.frame( "temporada"=TALensayo1$temporada,
                    "efecto"= TALensayo1$factor,
                    "nivel"= TALensayo1$Nivel,
                    "fecha"= TALensayo1$fecha,
                    "TAL"= TALensayo1$media,
                    "EE"=TALensayo1$EE)
tabla2<-data.frame( "temporada"=TALensayo2$temporada,
                    "efecto"= TALensayo2$factor,
                    "nivel"= TALensayo2$Nivel,
                    "fecha"= TALensayo2$fecha,
                    "TAL"= TALensayo2$media,
                    "EE"=TALensayo2$EE)

library(tidyverse)

tabla_ancha<-pivot_wider(data=tabla1, 
                         id_cols= c(temporada, efecto, nivel), 
                         names_from= fecha, 
                         values_from=c(TAL, EE))
tabla_ancha2<-pivot_wider(data=tabla2, 
                          id_cols= c(temporada, efecto, nivel), 
                          names_from= fecha, 
                          values_from=c(TAL, EE))

agregarFilas<-data.frame( temporada = c("Primavera/Verano", "Primavera/Verano", "Primavera/Verano", "Primavera/Verano", "Primavera/Verano"), 
                          efecto = c(" x̅", "CV", "p-valor", "p-valor", "p-valor"), 
                          nivel =c(" x̅", "CV", "Dosis abono", "Variedad", "Dosis*Variedad"),
                          TAL1 =c(mean(AreaFoliarE1$TAL1), 23.25, 0.68633, 0.75163, 0.16174),
                          TAL2 =c(mean(AreaFoliarE1$TAL2), 24.31, 0.66481, 0.73664, 0.03131),
                          TAL3 =c(mean(AreaFoliarE1$TAL3), 48.32, 0.38469, 0.33795, 0.71395),
                          TAL4 = c(mean(AreaFoliarE1$TAL4), 64.73, 0.71345, 0.01474, 0.92418), 
                          EE1 = c(NA, NA, NA, NA, NA), 
                          EE2 = c(NA, NA, NA, NA, NA), 
                          EE3 = c(NA, NA, NA, NA, NA),
                          EE4 = c(NA, NA, NA, NA, NA))

colnames(tabla_ancha)<-colnames(agregarFilas)
tabla_ancha<-rbind(tabla_ancha, agregarFilas)

agregarFilas2<-data.frame( temporada = c("Otoño/Invierno", "Otoño/Invierno", "Otoño/Invierno", "Otoño/Invierno", "Otoño/Invierno"), 
                           efecto = c(" x̅", "CV", "p-valor", "p-valor", "p-valor"), 
                           nivel =c(" x̅", "CV", "Dosis abono", "Variedad", "Dosis*Variedad"),
                           TAL1 =c(mean(AreaFoliarE2$TAL1), 22.85, 0.10948, 0.70595, 0.99034),
                           TAL2 =c(mean(AreaFoliarE2$TAL2), 28.93, 0.62713, 0.15221, 0.76208),
                           TAL3 =c(mean(AreaFoliarE2$TAL3), 134.86, 0.91511, 0.01215, 0.91690),
                           TAL4 = c(mean(AreaFoliarE1$TAL4), 105.76, 0.55787, 0.01742, 0.97393), 
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
                col_keys= c("temporada", "efecto", "nivel", "TAL1", "TAL2", "TAL3", "TAL4"), 
)%>% compose(i=c(1:6, 12:17), 
             j= "TAL1", 
             value= as_paragraph(
               as_chunk(format(TAL1, digits=2), props= fp_text(color="black", font.size=8) ),
               " ± ", 
               as_chunk(format(EE1, digits=2), props= fp_text(color="black", italic=TRUE, font.size=8))),
             part="body" ) %>%
  compose(i=c(1:6, 12:17), 
          j= "TAL2", 
          value= as_paragraph(
            as_chunk(format(TAL2, digits=2), props= fp_text(color="black", font.size=8)),
            " ± ", 
            as_chunk(format(EE2, digits=2), props= fp_text(color="black", italic=TRUE, font.size=8))),
          part="body" )%>% 
  compose(i=c(1:6, 12:17), 
          j= "TAL3", 
          value= as_paragraph(
            as_chunk(format(TAL3, digits=2), props= fp_text(color="black", font.size=8)),
            " ± ", 
            as_chunk(format(EE3, digits=2), props= fp_text(color="black", italic=TRUE, font.size=8))),
          part="body" ) %>% compose(i=c(1:6, 12:17), 
                                    j= "TAL4", 
                                    value= as_paragraph(
                                      as_chunk(format(TAL4, digits=2), props= fp_text(color="black", font.size=8)),
                                      " ± ", 
                                      as_chunk(format(EE4, digits=2), props= fp_text(color="black", italic=TRUE, font.size = 8))),
                                    part="body" )

tp<-colformat_double(tp, i=c(9:11, 20:22), j= c(4:7), digits= 4)
tp<-colformat_double(tp, i=c(7:8, 18:19), j=4:7, digits=2)
tp<-colformat_double(tp, i=c(7,18), j=4:7, digits=5)

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
              MS1 = "TAL",
              MS2 = "",
              MS3 = "",
              MS4 = "") %>% 
  merge_at(i=1, j=2:3, part="header") %>% 
  merge_at(i=2, j=4:7, part="header")

tp<-compose(tp, i=2, j=4, 
            value= as_paragraph(
              "Tasa de Asimilación Liquida (g cm", 
              as_sup("-2"),
              " día",
              as_sup("-1"),
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
save_as_docx("Tasa de Asimilación Líquida"= tp, path= "Resultados/TasaAsimilacion.docx")