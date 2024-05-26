Nensayo1<-readRDS("Resultados/Nensayo1.rds")
Nensayo2<-readRDS("Resultados/Nensayo2.rds")

library(tidyr)
NL1<-pivot_longer(data=Nensayo1,
                  cols=c("MS", 
                         "Nx100", "Px100", "Kx100", "Cax100", "Mgx100", 
                         "Nacu","Pacu", "Kacu", "Caacu", "Mgacu"),
                  names_to = "Variables",
                  values_to="Valores")
tipo<-c()
for (i in c(1:264)){
  if (NL1$Variables[i]=="Nx100" | NL1$Variables[i]=="Px100" |
      NL1$Variables[i]=="Kx100"| NL1$Variables[i]=="Cax100"| NL1$Variables[i]=="Mgx100"){
    tipo<-c(tipo, "Porcentaje")
  } else {
    tipo<-c(tipo, "Acumulacion")
  }
  
}
NL1$dato<-tipo

nutriente<-c()
for (j in c(1:264)){
  if(NL1$Variables[j]=="Nx100"|NL1$Variables[j]=="Nacu"){nutriente<-c(nutriente, "Nitrógeno")}
  if(NL1$Variables[j]=="Px100"|NL1$Variables[j]=="Pacu"){nutriente<-c(nutriente, "Fósforo")}
  if(NL1$Variables[j]=="Kx100"|NL1$Variables[j]=="Kacu"){nutriente<-c(nutriente, "Potasio")}
  if(NL1$Variables[j]=="Cax100"|NL1$Variables[j]=="Caacu"){nutriente<-c(nutriente, "Calcio")}
  if(NL1$Variables[j]=="Mgx100"|NL1$Variables[j]=="Mgacu"){nutriente<-c(nutriente, "Magnesio")}
  if(NL1$Variables[j]=="MS"){nutriente<-c(nutriente, "Materia Seca")}
}
nutriente
NL1$nutriente<-nutriente
NL1$Variables<-NULL
NW1<-pivot_wider(data=NL1,
                 names_from = "dato", 
                  values_from= "Valores")

NXW1<-pivot_wider(data=NL1, 
                  id_cols=c("Fecha", "DDT", "nutriente"), 
                  names_from = c("Factor", "Nivel", "dato"),
                  values_from="Valores")

NXW1$nutriente<-factor(NXW1$nutriente, levels=c("Nitrógeno", "Fósforo", "Potasio", "Calcio", "Magnesio", "Materia Seca"))
NXW1ordenado<-NXW1[order(NXW1$nutriente, NXW1$DDT),]

TablaNutrientes1<-NXW1ordenado[NXW1ordenado$nutriente!="Materia Seca",]

#Pasar a miligramos
TablaNutrientes1$Abono_0_Acumulacion<-TablaNutrientes1$Abono_0_Acumulacion*1000
TablaNutrientes1$Abono_427_Acumulacion<-TablaNutrientes1$Abono_427_Acumulacion*1000
TablaNutrientes1$Abono_854_Acumulacion<-TablaNutrientes1$Abono_854_Acumulacion*1000
TablaNutrientes1$Abono_1282_Acumulacion<-TablaNutrientes1$Abono_1282_Acumulacion*1000
TablaNutrientes1$`variedad_Penca Ancha_Acumulacion`<-TablaNutrientes1$`variedad_Penca Ancha_Acumulacion`*1000
TablaNutrientes1$`variedad_Penca Verde_Acumulacion`<-TablaNutrientes1$`variedad_Penca Verde_Acumulacion`*1000


library(flextable)

tp<- flextable(data = TablaNutrientes1,
               col_keys= c("nutriente", "DDT", 
                           colnames(TablaNutrientes1)[4:15])) 
library(dplyr)
tp <- tp %>% add_header_row(top= TRUE, 
                            values= c("", 
                                      "Tratamientos",
                                      "0"," ",
                                      "427", " ",
                                      "854"," ",
                                      "1282 "," ", 
                                      "Penca Ancha"," ",
                                      "Penca Verde", ""))
tp<- tp %>% add_header_row(top= TRUE, 
                           values= c("Primavera/Verano", 
                                      "",
                                      "Abono"," ",
                                       " ", " ",
                                        " "," ",
                                        " "," ", 
                                         "Variedad"," ",
                                          " ", ""))

tp<-tp %>% set_header_labels( nutriente= "Nutrientes",
                                    Abono_0_Acumulacion = "mg", Abono_0_Porcentaje= "%",
                                    Abono_427_Acumulacion = "mg", Abono_427_Porcentaje= "%",
                                    Abono_854_Acumulacion = "mg", Abono_854_Porcentaje= "%",
                                    Abono_1282_Acumulacion = "mg", Abono_1282_Porcentaje= "%",
                                    `variedad_Penca Ancha_Acumulacion`="mg", `variedad_Penca Ancha_Porcentaje`= "%",
                                    `variedad_Penca Verde_Acumulacion`="mg", `variedad_Penca Verde_Porcentaje`= "%"
                                    )
tp<- tp %>% merge_at(i=1, j=c(3:10), part="header")%>%
  merge_at(i=1, j=c(11:14), part="header")%>%
  merge_at(i=2, j=c(3:4), part="header")%>%
  merge_at(i=2, j=c(5:6), part="header")%>%
  merge_at(i=2, j=c(7:8), part="header")%>%
  merge_at(i=2, j=c(9:10), part="header")%>%
  merge_at(i=2, j=c(11:12), part="header")%>%
  merge_at(i=2, j=c(13:14), part="header")
tp<- compose(tp, i=1, j=3, 
             value= as_paragraph(
               "Dosis de Abono (g m",
               as_sup("-2"),
               ")"
             ),
             part="header")
tp<-tp %>% merge_at(i=c(1:4), j=1, part="body")
tp<-tp %>% merge_at(i=c(5:8), j=1, part="body")%>%
  merge_at(i=c(9:12), j=1, part="body")%>%
  merge_at(i=c(13:16), j=1, part="body")%>%
  merge_at(i=c(17:20), j=1, part="body")

tp<-colformat_double(tp, i=1:20, j=c(3, 5, 7, 9, 11, 13), digits=2)
tp<-colformat_double(tp, i=1:20, j=c(4, 6, 8, 10, 12, 14), digits=3)

tp<- tp %>% flextable::align(align="center", i=1:2, j=3:14, part="header")
tp<-tp %>% fontsize(size = 8, part = "all")%>%
  bold( part="header")

library(officer)
tp<-hline(tp, border= fp_border(color="gray", style="solid", width=1), i=c(4, 8, 12, 16, 20), part="body")

tp
save_as_docx("Nutrientes Primavera-Verano"= tp, path= "Resultados/Nutrientes1.docx")

###ENSAYO 2
library(tidyr)
NL2<-pivot_longer(data=Nensayo2,
                  cols=c("MS", 
                         "Nx100", "Px100", "Kx100", "Cax100", "Mgx100", 
                         "Nacu","Pacu", "Kacu", "Caacu", "Mgacu"),
                  names_to = "Variables",
                  values_to="Valores")
tipo<-c()
for (i in c(1:264)){
  if (NL2$Variables[i]=="Nx100" | NL2$Variables[i]=="Px100" |
      NL2$Variables[i]=="Kx100"| NL2$Variables[i]=="Cax100"| NL2$Variables[i]=="Mgx100"){
    tipo<-c(tipo, "Porcentaje")
  } else {
    tipo<-c(tipo, "Acumulacion")
  }
  
}
NL2$dato<-tipo

nutriente<-c()
for (j in c(1:264)){
  if(NL2$Variables[j]=="Nx100"|NL2$Variables[j]=="Nacu"){nutriente<-c(nutriente, "Nitrógeno")}
  if(NL2$Variables[j]=="Px100"|NL2$Variables[j]=="Pacu"){nutriente<-c(nutriente, "Fósforo")}
  if(NL2$Variables[j]=="Kx100"|NL2$Variables[j]=="Kacu"){nutriente<-c(nutriente, "Potasio")}
  if(NL2$Variables[j]=="Cax100"|NL2$Variables[j]=="Caacu"){nutriente<-c(nutriente, "Calcio")}
  if(NL2$Variables[j]=="Mgx100"|NL2$Variables[j]=="Mgacu"){nutriente<-c(nutriente, "Magnesio")}
  if(NL2$Variables[j]=="MS"){nutriente<-c(nutriente, "Materia Seca")}
}
nutriente
NL2$nutriente<-nutriente
NL2$Variables<-NULL

NXW2<-pivot_wider(data=NL2, 
                  id_cols=c("Fecha", "DDT", "nutriente"), 
                  names_from = c("Factor", "Nivel", "dato"),
                  values_from="Valores")

NXW2$nutriente<-factor(NXW2$nutriente, levels=c("Nitrógeno", "Fósforo", "Potasio", "Calcio", "Magnesio", "Materia Seca"))
NXW2ordenado<-NXW2[order(NXW2$nutriente, NXW2$DDT),]

TablaNutrientes2<-NXW2ordenado[NXW2ordenado$nutriente!="Materia Seca",]

#Pasar a miligramos
TablaNutrientes2$Abono_0_Acumulacion<-TablaNutrientes2$Abono_0_Acumulacion*1000
TablaNutrientes2$Abono_492_Acumulacion<-TablaNutrientes2$Abono_492_Acumulacion*1000
TablaNutrientes2$Abono_984_Acumulacion<-TablaNutrientes2$Abono_984_Acumulacion*1000
TablaNutrientes2$Abono_1476_Acumulacion<-TablaNutrientes2$Abono_1476_Acumulacion*1000
TablaNutrientes2$`variedad_Penca Ancha_Acumulacion`<-TablaNutrientes2$`variedad_Penca Ancha_Acumulacion`*1000
TablaNutrientes2$`variedad_Penca Verde_Acumulacion`<-TablaNutrientes2$`variedad_Penca Verde_Acumulacion`*1000


library(flextable)

tp<- flextable(data = TablaNutrientes2,
               col_keys= c("nutriente", "DDT", 
                           colnames(TablaNutrientes2)[4:15])) 
library(dplyr)
tp <- tp %>% add_header_row(top= TRUE, 
                            values= c("", 
                                      "Tratamientos",
                                      "0"," ",
                                      "492", " ",
                                      "984"," ",
                                      "1476"," ", 
                                      "Penca Ancha"," ",
                                      "Penca Verde", ""))
tp<- tp %>% add_header_row(top= TRUE, 
                           values= c("Primavera/Verano", 
                                     "",
                                     "Abono"," ",
                                     " ", " ",
                                     " "," ",
                                     " "," ", 
                                     "Variedad"," ",
                                     " ", ""))

tp<-tp %>% set_header_labels( nutriente= "Nutrientes",
                              Abono_0_Acumulacion = "mg", Abono_0_Porcentaje= "%",
                              Abono_492_Acumulacion = "mg", Abono_492_Porcentaje= "%",
                              Abono_984_Acumulacion = "mg", Abono_984_Porcentaje= "%",
                              Abono_1476_Acumulacion = "mg", Abono_1476_Porcentaje= "%",
                              `variedad_Penca Ancha_Acumulacion`="mg", `variedad_Penca Ancha_Porcentaje`= "%",
                              `variedad_Penca Verde_Acumulacion`="mg", `variedad_Penca Verde_Porcentaje`= "%"
)
tp<- tp %>% merge_at(i=1, j=c(3:10), part="header")%>%
  merge_at(i=1, j=c(11:14), part="header")%>%
  merge_at(i=2, j=c(3:4), part="header")%>%
  merge_at(i=2, j=c(5:6), part="header")%>%
  merge_at(i=2, j=c(7:8), part="header")%>%
  merge_at(i=2, j=c(9:10), part="header")%>%
  merge_at(i=2, j=c(11:12), part="header")%>%
  merge_at(i=2, j=c(13:14), part="header")
tp<- compose(tp, i=1, j=3, 
             value= as_paragraph(
               "Dosis de Abono (g m",
               as_sup("-2"),
               ")"
             ),
             part="header")
tp<-tp %>% merge_at(i=c(1:4), j=1, part="body")
tp<-tp %>% merge_at(i=c(5:8), j=1, part="body")%>%
  merge_at(i=c(9:12), j=1, part="body")%>%
  merge_at(i=c(13:16), j=1, part="body")%>%
  merge_at(i=c(17:20), j=1, part="body")

tp<-colformat_double(tp, i=1:20, j=c(3, 5, 7, 9, 11, 13), digits=2)
tp<-colformat_double(tp, i=1:20, j=c(4, 6, 8, 10, 12, 14), digits=3)

tp<- tp %>% flextable::align(align="center", i=1:2, j=3:14, part="header")
tp<-tp %>% fontsize(size = 8, part = "all")%>%
  bold( part="header")

library(officer)
tp<-hline(tp, border= fp_border(color="gray", style="solid", width=1), i=c(4, 8, 12, 16, 20), part="body")

tp
save_as_docx("Nutrientes Otoño-Invierno"= tp, path= "Resultados/Nutrientes2.docx")
