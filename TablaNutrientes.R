msensayo1<- readRDS("Resultados/msensayo1_v2.rds")
msensayo2<-readRDS("Resultados/msensayo2_v2.rds")
f01<-as.Date("24/11/2022", format="%d/%m/%Y")
f02<-as.Date("18/05/2023", format="%d/%m/%Y")
Nutriente1<-read.csv("NutrientesE1.csv", sep=",", dec=",", header = TRUE)
Nutriente2<-read.csv("NutrientesE2.csv", sep=",", dec=",", header = TRUE)

AcuNutrientesE1<-data.frame("Fecha"= msensayo1$fecha[msensayo1$fecha>f01],
                            "Factor"=msensayo1$group[msensayo1$fecha>f01],
                             "Nivel"=msensayo1$nivel[msensayo1$fecha>f01],
                            "MS"=msensayo1$media[msensayo1$fecha>f01])

Nutriente1$Medicion<-trimws(Nutriente1$Medicion)

mediciones<-c("PRIMERA", "SEGUNDA", "TERCERA", "CUARTA")
dosis<-c(1:4)
variedad<-c(1:2)

##Crear un vector con concentraciones de Nitrogeno
concNdosis<-c()
indicador<-c()

for (i in mediciones){
  for (j in dosis){
   concNdosis<- c(concNdosis, mean(Nutriente1$N.[Nutriente1$Medicion== i & 
                         Nutriente1$dosis== j]))
   indicador<-c(indicador, paste(i,j))
  }
  for (v in variedad){
    concNdosis<- c(concNdosis, mean(Nutriente1$N.[Nutriente1$Medicion== i & 
                                                    Nutriente1$variedad== v]))
    indicador<-c(indicador, paste(i,v))
  }
  j<-1
  v<-1
}
AcuNutrientesE1$Nx100<-concNdosis

###Lo mismo con Fosforo
concP<-c()
indicador<-c()

for (i in mediciones){
  for (j in dosis){
    concP<- c(concP, mean(Nutriente1$P.[Nutriente1$Medicion== i & 
                                                    Nutriente1$dosis== j]))
    indicador<-c(indicador, paste(i,j))
  }
  for (v in variedad){
    concP<- c(concP, mean(Nutriente1$P.[Nutriente1$Medicion== i & 
                                                    Nutriente1$variedad== v]))
    indicador<-c(indicador, paste(i,v))
  }
  j<-1
  v<-1
}
indicador
AcuNutrientesE1$Px100<-concP

##### Ahora el potasio
concK<-c()
indicador<-c()

for (i in mediciones){
  for (j in dosis){
    concK<- c(concK, mean(Nutriente1$K.[Nutriente1$Medicion== i & 
                                          Nutriente1$dosis== j]))
    indicador<-c(indicador, paste(i,j))
  }
  for (v in variedad){
    concK<- c(concK, mean(Nutriente1$K.[Nutriente1$Medicion== i & 
                                          Nutriente1$variedad== v]))
    indicador<-c(indicador, paste(i,v))
  }
  j<-1
  v<-1
}
indicador
AcuNutrientesE1$Kx100<-concK
#### el Calcio
concCa<-c()
indicador<-c()

for (i in mediciones){
  for (j in dosis){
    concCa<- c(concCa, mean(Nutriente1$Ca.[Nutriente1$Medicion== i & 
                                          Nutriente1$dosis== j]))
    indicador<-c(indicador, paste(i,j))
  }
  for (v in variedad){
    concCa<- c(concCa, mean(Nutriente1$Ca.[Nutriente1$Medicion== i & 
                                          Nutriente1$variedad== v]))
    indicador<-c(indicador, paste(i,v))
  }
  j<-1
  v<-1
}
indicador
AcuNutrientesE1$Cax100<-concCa

#Y por ultimo el magnesio

concMg<-c()
indicador<-c()

for (i in mediciones){
  for (j in dosis){
    concMg<- c(concMg, mean(Nutriente1$Mg.[Nutriente1$Medicion== i & 
                                             Nutriente1$dosis== j]))
    indicador<-c(indicador, paste(i,j))
  }
  for (v in variedad){
    concMg<- c(concMg, mean(Nutriente1$Mg.[Nutriente1$Medicion== i & 
                                             Nutriente1$variedad== v]))
    indicador<-c(indicador, paste(i,v))
  }
  j<-1
  v<-1
}
indicador
AcuNutrientesE1$Mgx100<-concMg

###Y ahora se calcula la acumulación de Nutrientes Multiplicando:

AcuNutrientesE1$Nacu<-AcuNutrientesE1$MS*AcuNutrientesE1$Nx100/100
AcuNutrientesE1$Pacu<-AcuNutrientesE1$MS*AcuNutrientesE1$Px100/100
AcuNutrientesE1$Kacu<-AcuNutrientesE1$MS*AcuNutrientesE1$Kx100/100
AcuNutrientesE1$Caacu<-AcuNutrientesE1$MS*AcuNutrientesE1$Cax100/100
AcuNutrientesE1$Mgacu<-AcuNutrientesE1$MS*AcuNutrientesE1$Mgx100/100

##Guardar esta tabla:
saveRDS(AcuNutrientesE1, file="Resultados/Nensayo1.rds")

#####ACA REPETIMOS LO MISMO PARA EL ENSAYO 2:

AcuNutrientesE2<-data.frame("Fecha"= msensayo2$fecha[msensayo2$fecha>f02],
                            "Factor"=msensayo2$group[msensayo2$fecha>f02],
                            "Nivel"=msensayo2$nivel[msensayo2$fecha>f02],
                            "MS"=msensayo2$media[msensayo2$fecha>f02])

Nutriente2$Medicion<-trimws(Nutriente2$Medicion)

mediciones<-c("PRIMERA", "SEGUNDA", "TERCERA", "CUARTA")
dosis<-c(1:4)
variedad<-c(1:2)

##Crear un vector con concentraciones de Nitrogeno
concNdosis<-c()
indicador<-c()

for (i in mediciones){
  for (j in dosis){
    concNdosis<- c(concNdosis, mean(Nutriente2$N.[Nutriente2$Medicion== i & 
                                                    Nutriente2$dosis== j]))
    indicador<-c(indicador, paste(i,j))
  }
  for (v in variedad){
    concNdosis<- c(concNdosis, mean(Nutriente2$N.[Nutriente2$Medicion== i & 
                                                    Nutriente2$variedad== v]))
    indicador<-c(indicador, paste(i,v))
  }
  j<-1
  v<-1
}
AcuNutrientesE2$Nx100<-concNdosis

###Lo mismo con Fosforo
concP<-c()
indicador<-c()

for (i in mediciones){
  for (j in dosis){
    concP<- c(concP, mean(Nutriente2$P.[Nutriente2$Medicion== i & 
                                          Nutriente2$dosis== j]))
    indicador<-c(indicador, paste(i,j))
  }
  for (v in variedad){
    concP<- c(concP, mean(Nutriente2$P.[Nutriente2$Medicion== i & 
                                          Nutriente2$variedad== v]))
    indicador<-c(indicador, paste(i,v))
  }
  j<-1
  v<-1
}
indicador
AcuNutrientesE2$Px100<-concP

##### Ahora el potasio
concK<-c()
indicador<-c()

for (i in mediciones){
  for (j in dosis){
    concK<- c(concK, mean(Nutriente2$K.[Nutriente2$Medicion== i & 
                                          Nutriente2$dosis== j]))
    indicador<-c(indicador, paste(i,j))
  }
  for (v in variedad){
    concK<- c(concK, mean(Nutriente2$K.[Nutriente2$Medicion== i & 
                                          Nutriente2$variedad== v]))
    indicador<-c(indicador, paste(i,v))
  }
  j<-1
  v<-1
}
indicador
AcuNutrientesE2$Kx100<-concK
#### el Calcio
concCa<-c()
indicador<-c()

for (i in mediciones){
  for (j in dosis){
    concCa<- c(concCa, mean(Nutriente2$Ca.[Nutriente2$Medicion== i & 
                                             Nutriente2$dosis== j]))
    indicador<-c(indicador, paste(i,j))
  }
  for (v in variedad){
    concCa<- c(concCa, mean(Nutriente2$Ca.[Nutriente2$Medicion== i & 
                                             Nutriente2$variedad== v]))
    indicador<-c(indicador, paste(i,v))
  }
  j<-1
  v<-1
}
indicador
AcuNutrientesE2$Cax100<-concCa

#Y por ultimo el magnesio

concMg<-c()
indicador<-c()

for (i in mediciones){
  for (j in dosis){
    concMg<- c(concMg, mean(Nutriente2$Mg.[Nutriente2$Medicion== i & 
                                             Nutriente2$dosis== j]))
    indicador<-c(indicador, paste(i,j))
  }
  for (v in variedad){
    concMg<- c(concMg, mean(Nutriente2$Mg.[Nutriente2$Medicion== i & 
                                             Nutriente2$variedad== v]))
    indicador<-c(indicador, paste(i,v))
  }
  j<-1
  v<-1
}
indicador
AcuNutrientesE2$Mgx100<-concMg

###Y ahora se calcula la acumulación de Nutrientes Multiplicando:

AcuNutrientesE2$Nacu<-AcuNutrientesE2$MS*AcuNutrientesE2$Nx100/100
AcuNutrientesE2$Pacu<-AcuNutrientesE2$MS*AcuNutrientesE2$Px100/100
AcuNutrientesE2$Kacu<-AcuNutrientesE2$MS*AcuNutrientesE2$Kx100/100
AcuNutrientesE2$Caacu<-AcuNutrientesE2$MS*AcuNutrientesE2$Cax100/100
AcuNutrientesE2$Mgacu<-AcuNutrientesE2$MS*AcuNutrientesE2$Mgx100/100

##Guardar esta tabla:
saveRDS(AcuNutrientesE2, file="Resultados/Nensayo2.rds")
