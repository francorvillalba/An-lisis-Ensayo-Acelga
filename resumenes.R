MS06DIC<-read.csv("MS06DIC.csv", header=TRUE, sep=",", dec=",")
MS06DIC["Tratamients"]<-paste(MS06DIC$Dosis, MS06DIC$NombreV, sep="_")

resumir2fat<-function(fat1, 
                      fat2, 
                      resp,
                      trat,
                      df
){
  nf1<-unique(fat1)
  nf2<-unique(fat2)
  alltrat<-unique(trat)
  
  medias<-c()
  Desvio_Standar<-c()
  numero<-c()
  
  for (f1 in nf1){
    medias<-c(medias, mean(resp[fat1==f1]))
    Desvio_Standar<-c(Desvio_Standar, sd(resp[fat1==f1]))
    numero<-c(numero, sum(with(df, fat1==f1)))
  }
 
  for (f2 in nf2){
    medias<-c(medias, mean(resp[fat2==f2]))
    Desvio_Standar<-c(Desvio_Standar, sd(resp[fat2==f2]))
    numero<-c(numero, sum(with(df, fat2==f2)))
  }
  for (t in alltrat){
    medias<-c(medias, mean(resp[trat==t]))
    Desvio_Standar<-c(Desvio_Standar, sd(resp[trat==t]))
    numero<-c(numero, sum(with(df, trat==t)))
  }
  agrupamientos<-c(nf1, nf2, alltrat)
  CV<-(Desvio_Standar/medias)*100
  return(data.frame(agrupamientos, numero, medias, Desvio_Standar, CV))
}


resumir2fat(MS06DIC$Dosis, MS06DIC$NombreV, MS06DIC$PesoT, MS06DIC$Tratamients, MS06DIC)
