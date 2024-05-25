### Tabla para presentación Materia Seca

CrecimientoE1<-readRDS(file = "Resultados/CrecimientoE1_v2.rds")
CrecimientoE2<-readRDS(file = "Resultados/CrecimientoE2_v2.rds")

msensayo1<-readRDS(file = "Resultados/msensayo1_v2.rds")
msensayo2<-readRDS(file = "Resultados/msensayo2_v2.rds")

msensayo1$temporada<-"Primavera/Verano"
msensayo2$temporada<-"Otoño/Invierno"

tabla<-data.frame( "temporada"=msensayo1$temporada,
                   "efecto"= msensayo1$group,
                   "nivel"= msensayo1$nivel,
                   "fecha"= msensayo1$fecha,
                   "MS"= msensayo1$media,
                    "EE"=msensayo1$EE)
tabla2<-data.frame( "temporada"=msensayo2$temporada,
                   "efecto"= msensayo2$group,
                   "nivel"= msensayo2$nivel,
                   "fecha"= msensayo2$fecha,
                   "MS"= msensayo2$media,
                   "EE"=msensayo2$EE)

library(tidyverse)

tabla_ancha<-pivot_wider(data=tabla, 
                         id_cols= c(temporada, efecto, nivel), 
                         names_from= fecha, 
                         values_from=c(MS, EE))
tabla_ancha2<-pivot_wider(data=tabla2, 
                         id_cols= c(temporada, efecto, nivel), 
                         names_from= fecha, 
                         values_from=c(MS, EE))
agregarFilas<-data.frame( temporada = c("Primavera/Verano", "Primavera/Verano", "Primavera/Verano", "Primavera/Verano", "Primavera/Verano"), 
                          efecto = c("Media", "CV", "p-valor", "p-valor", "p-valor"), 
                          nivel =c("Media", "CV", "dosis abono", "variedad", "dosis*variedad"), 
    MS0 =c(mean(CrecimientoE1$MS0), (sd(CrecimientoE1$MS0)/mean(CrecimientoE1$MS0))*100, NA, NA, NA),
    MS1 =c(mean(CrecimientoE1$MS1), 37.08, 0.56836, 0.41887, 0.0908),
    MS2 =c(mean(CrecimientoE1$MS2), 42.99, 0.85271, 0.01120, 0.61108),
    MS3 =c(mean(CrecimientoE1$MS3), 34.43, 0.18481, 0.00142, 0.79788),
    MS4 = c(mean(CrecimientoE1$MS4), 33.73, 0.88944, 0.99721, 0.79842), 
    EE0 = c(NA, NA, NA, NA, NA), 
    EE1 = c(NA, NA, NA, NA, NA), 
    EE2 = c(NA, NA, NA, NA, NA), 
    EE3 = c(NA, NA, NA, NA, NA),
    EE4 = c(NA, NA, NA, NA, NA))

colnames(tabla_ancha)<-colnames(agregarFilas)
tabla_ancha<-rbind(tabla_ancha, agregarFilas)

agregarFilas2<-data.frame( temporada = c("Otoño/Invierno", "Otoño/Invierno", "Otoño/Invierno", "Otoño/Invierno", "Otoño/Invierno"), 
                          efecto = c("media", "CV", "p-valor", "p-valor", "p-valor"), 
                          nivel =c("media", "CV", "dosis abono", "variedad", "dosis*variedad"), 
                          MS0 =c(mean(CrecimientoE2$MS0), (sd(CrecimientoE2$MS0)/mean(CrecimientoE2$MS0))*100, NA, NA, NA),
                          MS1 =c(mean(CrecimientoE2$MS1), 45.02, 0.50570, 0.05626, 0.85820),
                          MS2 =c(mean(CrecimientoE2$MS2), 39.31, 0.19159, 0.00077, 0.57616),
                          MS3 =c(mean(CrecimientoE2$MS3), 54.29, 0.43558, 0.6545, 0.50585),
                          MS4 = c(mean(CrecimientoE2$MS4), 49.85, 0.40416, 0.05587, 0.46054), 
                          EE0 = c(NA, NA, NA, NA, NA), 
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
                col_keys= c("temporada", "efecto", "nivel", "MS1", "MS2", "MS3", "MS4"), 
                )%>% compose(i=c(1:6, 12:17), 
                    j= "MS1", 
                    value= as_paragraph(
                    as_chunk(format(MS1, digits=2), props= fp_text(color="black") ),
                    " ± ", 
                    as_chunk(format(EE1, digits=2), props= fp_text(color="black", italic=TRUE))),
                    part="body" ) %>%
                    compose(i=c(1:6, 12:17), 
                    j= "MS2", 
                    value= as_paragraph(
                      as_chunk(format(MS2, digits=2), props= fp_text(color="black")),
                      "±", 
                      as_chunk(format(EE2, digits=2), props= fp_text(color="black", italic=TRUE))),
                    part="body" )%>% 
         compose(i=c(1:6, 12:17), 
          j= "MS3", 
          value= as_paragraph(
            as_chunk(format(MS3, digits=2), props= fp_text(color="black")),
            "±", 
            as_chunk(format(EE3, digits=2), props= fp_text(color="black", italic=TRUE))),
          part="body" ) %>% compose(i=c(1:6, 12:17), 
                                    j= "MS4", 
                                    value= as_paragraph(
                                      as_chunk(format(MS4, digits=2), props= fp_text(color="black")),
                                      "±", 
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
                                   MS1 = "Materia Seca (g)",
                                   MS2 = "Materia Seca (g)",
                                   MS3 = "Materia Seca (g)",
                                   MS4 = "Materia Seca (g)") %>% 
                   merge_at(i=1, j=2:3, part="header") %>% 
                 merge_at(i=2, j=4:7, part="header")

tp
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
save_as_docx("Materia Seca"= tp, path= "Resultados/MatSeca.docx")
