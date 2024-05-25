### Tabla para presentación Tasa de Crecimiento Relativa

CrecimientoE1<-readRDS(file = "Resultados/CrecimientoE1_v2.rds")
CrecimientoE2<-readRDS(file = "Resultados/CrecimientoE2_v2.rds")

TCRensayo1<-readRDS(file = "Resultados/TCRensayo1.rds")
TCRensayo2<-readRDS(file = "Resultados/TCRensayo2.rds")

TCRensayo1$temporada<-"Primavera/Verano"
TCRensayo2$temporada<-"Otoño/Invierno"

tabla1<-data.frame( "temporada"=TCRensayo1$temporada,
                    "efecto"= TCRensayo1$factor,
                    "nivel"= TCRensayo1$nivel,
                    "fecha"= TCRensayo1$fecha,
                    "TCR"= TCRensayo1$media,
                    "EE"=TCRensayo1$EE)
tabla2<-data.frame( "temporada"=TCRensayo2$temporada,
                    "efecto"= TCRensayo2$factor,
                    "nivel"= TCRensayo2$nivel,
                    "fecha"= TCRensayo2$fecha,
                    "TCR"= TCRensayo2$media,
                    "EE"=TCRensayo2$EE)

library(tidyverse)

tabla_ancha<-pivot_wider(data=tabla1, 
                         id_cols= c(temporada, efecto, nivel), 
                         names_from= fecha, 
                         values_from=c(TCR, EE))
tabla_ancha2<-pivot_wider(data=tabla2, 
                          id_cols= c(temporada, efecto, nivel), 
                          names_from= fecha, 
                          values_from=c(TCR, EE))
agregarFilas<-data.frame( temporada = c("Primavera/Verano", "Primavera/Verano", "Primavera/Verano", "Primavera/Verano", "Primavera/Verano"), 
                          efecto = c(" x̅", "CV", "p-valor", "p-valor", "p-valor"), 
                          nivel =c(" x̅", "CV", "Dosis abono", "Variedad", "Dosis*Variedad"),
                          TCR1 =c(mean(CrecimientoE1$TCR1), 54.23, 0.49290, 0.88306, 0.05932),
                          TCR2 =c(mean(CrecimientoE1$TCR2), 23.81, 0.76019, 0.05921, 0.03073),
                          TCR3 =c(mean(CrecimientoE1$TCR3), 45.31, 0.37596, 0.35654, 0.83484),
                          TCR4 = c(mean(CrecimientoE1$TCR4), 62.05, 0.86253, 0.00675, 0.06268), 
                          EE1 = c(NA, NA, NA, NA, NA), 
                          EE2 = c(NA, NA, NA, NA, NA), 
                          EE3 = c(NA, NA, NA, NA, NA),
                          EE4 = c(NA, NA, NA, NA, NA))

colnames(tabla_ancha)<-colnames(agregarFilas)
tabla_ancha<-rbind(tabla_ancha, agregarFilas)

agregarFilas2<-data.frame( temporada = c("Otoño/Invierno", "Otoño/Invierno", "Otoño/Invierno", "Otoño/Invierno", "Otoño/Invierno"), 
                           efecto = c(" x̅", "CV", "p-valor", "p-valor", "p-valor"), 
                           nivel =c(" x̅", "CV", "Dosis abono", "Variedad", "Dosis*Variedad"),
                           TCR1 =c(mean(CrecimientoE2$TCR1), 22.59, 0.22121, 0.35825, 0.87807),
                           TCR2 =c(mean(CrecimientoE2$TCR2), 27.27, 0.5315, 0.20228, 0.77738),
                           TCR3 =c(mean(CrecimientoE2$TCR3), 136.94, 0.92948, 0.01169, 0.96094),
                           TCR4 = c(mean(CrecimientoE2$TCR4), 104.36, 0.99325, 0.00874, 0.89632), 
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
                col_keys= c("temporada", "efecto", "nivel", "TCR1", "TCR2", "TCR3", "TCR4"), 
)%>% compose(i=c(1:6, 12:17), 
             j= "TCR1", 
             value= as_paragraph(
               as_chunk(format(TCR1, digits=2), props= fp_text(color="black", font.size=8) ),
               " ± ", 
               as_chunk(format(EE1, digits=2), props= fp_text(color="black", italic=TRUE, font.size=8))),
             part="body" ) %>%
  compose(i=c(1:6, 12:17), 
          j= "TCR2", 
          value= as_paragraph(
            as_chunk(format(TCR2, digits=2), props= fp_text(color="black", font.size=8)),
            " ± ", 
            as_chunk(format(EE2, digits=2), props= fp_text(color="black", italic=TRUE, font.size=8))),
          part="body" )%>% 
  compose(i=c(1:6, 12:17), 
          j= "TCR3", 
          value= as_paragraph(
            as_chunk(format(TCR3, digits=2), props= fp_text(color="black", font.size=8)),
            " ± ", 
            as_chunk(format(EE3, digits=2), props= fp_text(color="black", italic=TRUE, font.size=8))),
          part="body" ) %>% compose(i=c(1:6, 12:17), 
                                    j= "TCR4", 
                                    value= as_paragraph(
                                      as_chunk(format(TCR4, digits=2), props= fp_text(color="black", font.size=8)),
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
              MS1 = "TCR1",
              MS2 = "",
              MS3 = "",
              MS4 = "") %>% 
  merge_at(i=1, j=2:3, part="header") %>% 
  merge_at(i=2, j=4:7, part="header")

tp<-compose(tp, i=2, j=4, 
            value= as_paragraph(
              "Tasa de Crecimiento Relativa (g g", 
              as_sup("-1"),
              "día",
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
save_as_docx("Tasa de Crecimiento Relativa"= tp, path= "Resultados/TasaRelativa.docx")
