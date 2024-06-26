### Area foliar ensayo 1
###FUNCIONES CREADAS:

#### Para crear un modelo de regresion para cada tratamiento:
regModelAF<- function(trat, AS, HM){
  #creamos vectores vacios para guardar cada parametro del modelo: ordenada al origen, pendiente de la recta, coefi. pearson de la ordenada y de la pendiente, r^2 y r^2 ajustado del modelo
  ord <-c() 
  prord<-c()
  pend<-c()
  prpend<-c()
  rsq<-c()
  radj<-c()
  tratamientos<-unique(trat)
  
  #El ciclo FOR va ir realizando una regresion por CADA TRATAMIENTO y guardando los datos en cada vector creado
  
  for (i in tratamientos){
    HMT<-HM[trat== i] #PESO HOJAS MEDIDAS DEL TRATAMIENTO I
    AFT<-AS[trat== i] #AREA FOLIAR DE LAS HOJAS MEDIDAS DEL TRATAMIENTO I
    
    regresion<-summary(lm(AFT~HMT)) #SE EJECUTA LA REGRESION
    
    ord<-c(ord, regresion$coefficients[1,1]) #guarda el intercept del tratamiento i en un vector
    prord<-c(prord, regresion$coefficients[1,4]) #guarda el coeficiente de pearson de la ordenada para el tratamiento i
    pend<-c(pend, regresion$coefficients[2,1]) #guarda la pendiente del tratamiento i en un vector
    prpend<-c(prpend, regresion$coefficients[2,4]) #guarda el coeficiente de pearson de la pendienyte en el tratamiento i
    rsq<-c(rsq, regresion$r.squared) #guarda el r^2 del modelo al tratamiento i
    radj<-c(radj, regresion$adj.r.squared) #guarda el r^2 ajustado del modelo para el tratamiento i
  }
  
  # creamos un data frame con los vectores
  MAF<-data.frame(tratamientos, ord, prord, pend, prpend, rsq, radj)
  return(MAF)
}
## Función para calcular el área foliar en base a un modelo por tratamiento

calcAf2<-function(H, trat, MAF, tratalt, MAFalt){
  #Se recorre fila por fila calculando PH*PENDIENA + ORDENADA para obtener un vector con el area foliar
  areasF<-c()
  
  for (i in c(1:32)){
    if(MAF$prpend[MAF$tratamientos== trat[i]]<0.05){
      ordenada<-MAF$ord[MAF$tratamientos== trat[i]]
      pendiente<-MAF$pend[MAF$tratamientos== trat[i]]
      pesohojas<-H[i]
      af<-ordenada + pendiente*pesohojas
      areasF<-c(areasF, af)} else {
        ordenada<-MAFalt$ord[MAFalt$tratamientos== tratalt[i]]
        pendiente<-MAFalt$pend[MAFalt$tratamientos== tratalt[i]]
        pesohojas<-H[i]
        af<-ordenada + pendiente*pesohojas
        areasF<-c(areasF, af)
      }
  }
  return(areasF)  
}




###Importar los datos de las 4 fechas
f1<-read.csv(file="MS06DIC.csv", header=TRUE, sep="," , dec=",")
f2<-read.csv(file="MS20DIC.csv", header=TRUE, sep="," , dec=",")
f3<-read.csv(file="MS03ENE.csv", header=TRUE, sep="," , dec=",")
f4<-read.csv(file="MS17ENE.csv", header=TRUE, sep="," , dec=",")

f0<-read.csv(file="plantines1.csv", header=TRUE, sep="," , dec=",")

#Calcular los modelos por tratamiento con la funcion regModelAF
tratamientos<-paste(f1$NombreV, f1$Dosis, sep="_")
f1$trat<-tratamientos
f2$trat<-tratamientos
f3$trat<-tratamientos

f4$trat<-tratamientos


modeloF2<- regModelAF(f2$trat, f2$AS, f2$HM)
modeloF3<-regModelAF(f3$trat, f3$AS, f3$HM)
modeloF4<-regModelAF(f4$trat, f4$AS, f4$HM)

#Calcular un modelo alternativo

modeloF2alt<- regModelAF(f2$NombreV, f2$AS, f2$HM)
modeloF3alt<-regModelAF(f3$NombreV, f3$AS, f3$HM)
modeloF4alt<-regModelAF(f4$NombreV, f4$AS, f4$HM)

#Calcular el area foliar de las fechas

f2$A<-calcAf2(f2$HSM+f2$HM, f2$trat, modeloF2, f2$NombreV, modeloF2alt)
f3$A<-calcAf2(f3$HSM+f3$HM, f3$trat, modeloF3, f3$NombreV, modeloF3alt)
f4$A<-calcAf2(f4$HSM+f4$HM, f4$trat, modeloF4, f4$NombreV, modeloF4alt)

fecha1<-as.Date("06/12/2022", format="%d/%m/%Y")
fecha2<-as.Date("20/12/2022", format="%d/%m/%Y")
fecha3<-as.Date("03/01/2023", format="%d/%m/%Y")
fecha4<-as.Date("17/01/2023", format="%d/%m/%Y")
fecha0<-as.Date("24/11/2022", format="%d/%m/%Y")

AreasFoliares<-data.frame(
   "Bloque"= f1$Bloque,
   "UE"= f1$UE,
   "Variedad"=f1$NombreV,
    "nVar"=f1$Variedad,
   "Dosis"=f1$GrM2,
   "MS1"=f1$PesoT/3, "MS2"=f2$PesoT/3, "MS3"=f3$PesoT/3, "MS4"=f4$PesoT/3,
    "AF1"=f1$A/3, "AF2"=f2$A/3, "AF3"=f3$A/3, "AF4"=f4$A/3)
###Agregar datos de los plantines
MSp<-c()
for (i in c(1:32)){
  MSp<-c(MSp, mean(f0$PesoTotal[f0$Variedad==AreasFoliares$nVar[i]])/3)
}
AreasFoliares$MS0<-MSp

AFp<-c()
for (i in c(1:32)){
  AFp<-c(AFp, mean(f0$A[f0$Variedad==AreasFoliares$nVar[i]]))
}
AreasFoliares$AF0<-AFp

library(ExpDes.pt)

fat2.dbc(
  AreasFoliares$Dosis,
  AreasFoliares$Variedad,
  AreasFoliares$Bloque,
  AreasFoliares$TAL3,
  quali = c(FALSE, TRUE),
  mcomp = "tukey",
  fac.names = c("Gramos de abono/m2", "Variedad"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)



AreasFoliares$TAL1<-((AreasFoliares$MS1-AreasFoliares$MS0)/
                    as.integer(difftime(fecha1, fecha0)))*
                    ((log(AreasFoliares$AF1)-log(AreasFoliares$AF0))/
                    (AreasFoliares$AF1-AreasFoliares$AF0))

AreasFoliares$TAL2<-((AreasFoliares$MS2-AreasFoliares$MS1)/
                       as.integer(difftime(fecha2, fecha1)))*
  ((log(AreasFoliares$AF2)-log(AreasFoliares$AF1))/
     (AreasFoliares$AF2-AreasFoliares$AF1))

AreasFoliares$TAL3<-((AreasFoliares$MS3-AreasFoliares$MS2)/
                       as.integer(difftime(fecha3, fecha2)))*
  ((log(AreasFoliares$AF3)-log(AreasFoliares$AF2))/
     (AreasFoliares$AF3-AreasFoliares$AF2))

AreasFoliares$TAL4<-((AreasFoliares$MS4-AreasFoliares$MS3)/
                       as.integer(difftime(fecha4, fecha3)))*
                     ((log(AreasFoliares$AF4)-log(AreasFoliares$AF3))/
                    (AreasFoliares$AF4-AreasFoliares$AF3))

saveRDS(AreasFoliares, file="Resultados/Afoliar1_v2.rds")

library(dplyr)
#Variable Area FOLIAR

###Estadistica descriptiva por dosis de abono

AreasFoliares$factorA<-as.factor(AreasFoliares$Dosis)

###Fecha 1 por dosis de abono

desc1A<-AreasFoliares %>% group_by(factorA) %>% summarise(obs = n(),
                                                          max= max(AF1),
                                                          min= min(AF1), 
                                                          media= mean(AF1),
                                                          DS= sd(AF1),
                                                          EE= sd(AF1)/sqrt(n()),
                                                          ymin= mean(AF1)-(sd(AF1)/sqrt(n())),
                                                          ymax= mean(AF1)+(sd(AF1)/sqrt(n())))

desc1A$fecha<-fecha1

### Fecha 2
desc2A<-AreasFoliares %>% group_by(factorA) %>% summarise(obs = n(),
                                                          max= max(AF2),
                                                          min= min(AF2), 
                                                          media= mean(AF2),
                                                          DS= sd(AF2),
                                                          EE= sd(AF2)/sqrt(n()),
                                                          ymin= mean(AF2)-(sd(AF2)/sqrt(n())),
                                                          ymax= mean(AF2)+(sd(AF2)/sqrt(n())))

desc2A$fecha<-fecha2

###Fecha 3
desc3A<-AreasFoliares %>% group_by(factorA) %>% summarise(obs = n(),
                                                          max= max(AF3),
                                                          min= min(AF3), 
                                                          media= mean(AF3),
                                                          DS= sd(AF3),
                                                          EE= sd(AF3)/sqrt(n()),
                                                          ymin= mean(AF3)-(sd(AF3)/sqrt(n())),
                                                          ymax= mean(AF3)+(sd(AF3)/sqrt(n())))

desc3A$fecha<-fecha3
### fecha 4
desc4A<-AreasFoliares %>% group_by(factorA) %>% summarise(obs = n(),
                                                          max= max(AF4),
                                                          min= min(AF4), 
                                                          media= mean(AF4),
                                                          DS= sd(AF4),
                                                          EE= sd(AF4)/sqrt(n()),
                                                          ymin= mean(AF4)-(sd(AF4)/sqrt(n())),
                                                          ymax= mean(AF4)+(sd(AF4)/sqrt(n())))

desc4A$fecha<-fecha4

descAFgbA<- rbind(desc1A, desc2A, desc3A, desc4A)

library(ggplot2)

ggplot(descAFgbA, mapping=aes(x=fecha, y= media, color=factorA))+
  geom_line()+
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=0.5, size= 0.5, alpha=0.5)+
  labs(title= "Area Foliar Total",
       subtitle= "Primavera-Verano",
       x= "Fecha",
       y=expression("Area Foliar (" ~cm^2~")"),
       color= expression("Dosis de abono (g " ~m^-2~ ")" )
  )

# Estadistica descriptiva agrupada por variedad 

##Fecha 1

desc1V<-AreasFoliares %>% group_by(Variedad) %>% summarise(obs = n(),
                                                          max= max(AF1),
                                                          min= min(AF1), 
                                                          media= mean(AF1),
                                                          DS= sd(AF1),
                                                          EE= sd(AF1)/sqrt(n()),
                                                          ymin= mean(AF1)-(sd(AF1)/sqrt(n())),
                                                          ymax= mean(AF1)+(sd(AF1)/sqrt(n())))

desc1V$fecha<-fecha1

### Fecha 2
desc2V<-AreasFoliares %>% group_by(Variedad) %>% summarise(obs = n(),
                                                          max= max(AF2),
                                                          min= min(AF2), 
                                                          media= mean(AF2),
                                                          DS= sd(AF2),
                                                          EE= sd(AF2)/sqrt(n()),
                                                          ymin= mean(AF2)-(sd(AF2)/sqrt(n())),
                                                          ymax= mean(AF2)+(sd(AF2)/sqrt(n())))

desc2V$fecha<-fecha2

###Fecha 3
desc3V<-AreasFoliares %>% group_by(Variedad) %>% summarise(obs = n(),
                                                          max= max(AF3),
                                                          min= min(AF3), 
                                                          media= mean(AF3),
                                                          DS= sd(AF3),
                                                          EE= sd(AF3)/sqrt(n()),
                                                          ymin= mean(AF3)-(sd(AF3)/sqrt(n())),
                                                          ymax= mean(AF3)+(sd(AF3)/sqrt(n())))

desc3V$fecha<-fecha3
### fecha 4
desc4V<-AreasFoliares %>% group_by(Variedad) %>% summarise(obs = n(),
                                                          max= max(AF4),
                                                          min= min(AF4), 
                                                          media= mean(AF4),
                                                          DS= sd(AF4),
                                                          EE= sd(AF4)/sqrt(n()),
                                                          ymin= mean(AF4)-(sd(AF4)/sqrt(n())),
                                                          ymax= mean(AF4)+(sd(AF4)/sqrt(n())))

desc4V$fecha<-fecha4

descAFgbV<- rbind(desc1V, desc2V, desc3V, desc4V)

ggplot(descAFgbV, mapping=aes(x=fecha, y= media, linetype=Variedad))+
  geom_line()+
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=0.5, size= 2, alpha=0.5)+
  labs(title= "Area Foliar Total",
       subtitle= "Primavera-Verano",
       x= "Fecha",
       y="Gramos MS/planta" )


##Exportar
descAFgbA$factor<-"Abono"
descAFgbV$factor<-"Variedad"

colnames(descAFgbA)[1]<-"Nivel"
colnames(descAFgbV)[1]<-"Nivel"

AFensayo1<-rbind(descAFgbA, descAFgbV)
AFensayo1$temporada<-"Primavera/Verano"

saveRDS(AFensayo1, file="Resultados/AFensayo1.rds")

####TASAS DE ASIMILACIÓN LÍQUIDA

###Estadistica descriptiva por dosis de abono


###Fecha 1 por dosis de abono

desc1TALA<-AreasFoliares %>% group_by(factorA) %>% summarise(obs = n(),
                                                             max= max(TAL1),
                                                             min= min(TAL1), 
                                                             media= mean(TAL1),
                                                             DS= sd(TAL1),
                                                             EE= sd(TAL1)/sqrt(n()),
                                                             ymin= mean(TAL1)-(sd(TAL1)/sqrt(n())),
                                                             ymax= mean(TAL1)+(sd(TAL1)/sqrt(n())))

desc1TALA$fecha<-fecha1

### Fecha 2
desc2TALA<-AreasFoliares %>% group_by(factorA) %>% summarise(obs = n(),
                                                             max= max(TAL2),
                                                             min= min(TAL2), 
                                                             media= mean(TAL2),
                                                             DS= sd(TAL2),
                                                             EE= sd(TAL2)/sqrt(n()),
                                                             ymin= mean(TAL2)-(sd(TAL2)/sqrt(n())),
                                                             ymax= mean(TAL2)+(sd(TAL2)/sqrt(n())))

desc2TALA$fecha<-fecha2

###Fecha 3
desc3TALA<-AreasFoliares %>% group_by(factorA) %>% summarise(obs = n(),
                                                             max= max(TAL3),
                                                             min= min(TAL3), 
                                                             media= mean(TAL3),
                                                             DS= sd(TAL3),
                                                             EE= sd(TAL3)/sqrt(n()),
                                                             ymin= mean(TAL3)-(sd(TAL3)/sqrt(n())),
                                                             ymax= mean(TAL3)+(sd(TAL3)/sqrt(n())))

desc3TALA$fecha<-fecha3

###Fecha 4
desc4TALA<-AreasFoliares %>% group_by(factorA) %>% summarise(obs = n(),
                                                             max= max(TAL4),
                                                             min= min(TAL4), 
                                                             media= mean(TAL4),
                                                             DS= sd(TAL4),
                                                             EE= sd(TAL4)/sqrt(n()),
                                                             ymin= mean(TAL4)-(sd(TAL4)/sqrt(n())),
                                                             ymax= mean(TAL4)+(sd(TAL4)/sqrt(n())))

desc4TALA$fecha<-fecha4

descTALgbA<- rbind(desc1TALA, desc2TALA, desc3TALA, desc4TALA)

library(ggplot2)
ggplot(descTALgbA, mapping=aes(x=fecha, y= media, linetype=factorA))+
  geom_line()+
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=0.5, size= 2, alpha=0.5)+
  labs(title= "Tasa de Asimilación Líquida Total",
       subtitle= "Primavera-Verano",
       x= "Fecha",
       y="Gramos MS/planta",
  )

# Estadistica descriptiva agrupada por variedad 

##Fecha 1

desc1TALV<-AreasFoliares %>% group_by(Variedad) %>% summarise(obs = n(),
                                                              max= max(TAL1),
                                                              min= min(TAL1), 
                                                              media= mean(TAL1),
                                                              DS= sd(TAL1),
                                                              EE= sd(TAL1)/sqrt(n()),
                                                              ymin= mean(TAL1)-(sd(TAL1)/sqrt(n())),
                                                              ymax= mean(TAL1)+(sd(TAL1)/sqrt(n())))

desc1TALV$fecha<-fecha1

### Fecha 2
desc2TALV<-AreasFoliares %>% group_by(Variedad) %>% summarise(obs = n(),
                                                              max= max(TAL2),
                                                              min= min(TAL2), 
                                                              media= mean(TAL2),
                                                              DS= sd(TAL2),
                                                              EE= sd(TAL2)/sqrt(n()),
                                                              ymin= mean(TAL2)-(sd(TAL2)/sqrt(n())),
                                                              ymax= mean(TAL2)+(sd(TAL2)/sqrt(n())))

desc2TALV$fecha<-fecha2

###Fecha 3
desc3TALV<-AreasFoliares %>% group_by(Variedad) %>% summarise(obs = n(),
                                                              max= max(TAL3),
                                                              min= min(TAL3), 
                                                              media= mean(TAL3),
                                                              DS= sd(TAL3),
                                                              EE= sd(TAL3)/sqrt(n()),
                                                              ymin= mean(TAL3)-(sd(TAL3)/sqrt(n())),
                                                              ymax= mean(TAL3)+(sd(TAL3)/sqrt(n())))



desc3TALV$fecha<-fecha3

###Fecha 4
desc4TALV<-AreasFoliares %>% group_by(Variedad) %>% summarise(obs = n(),
                                                              max= max(TAL4),
                                                              min= min(TAL4), 
                                                              media= mean(TAL4),
                                                              DS= sd(TAL4),
                                                              EE= sd(TAL4)/sqrt(n()),
                                                              ymin= mean(TAL4)-(sd(TAL4)/sqrt(n())),
                                                              ymax= mean(TAL4)+(sd(TAL4)/sqrt(n())))



desc4TALV$fecha<-fecha4

descTALgbV<- rbind(desc1TALV, desc2TALV, desc3TALV, desc4TALV)

ggplot(descTALgbV, mapping=aes(x=fecha, y= media, linetype=Variedad))+
  geom_line()+
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=0.5, size= 2, alpha=0.5)+
  labs(title= "Area Foliar Total",
       subtitle= "Primavera-Verano",
       x= "Fecha",
       y="Gramos MS/planta",
  )

descTALgbA$factor<-"Abono"
descTALgbV$factor<-"Variedad"

colnames(descTALgbA)[1]<-"Nivel"
colnames(descTALgbV)[1]<-"Nivel"

TALensayo1<-rbind(descTALgbA, descTALgbV)
TALensayo1$temporada<-"Primavera/Verano"

saveRDS(TALensayo1, file="Resultados/TALensayo1_v2.rds")
