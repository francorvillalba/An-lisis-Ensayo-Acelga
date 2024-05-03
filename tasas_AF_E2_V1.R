### Area foliar ensayo 1

###Importar los datos de las 4 fechas
f1<-read.csv(file="MS30May.csv", header=TRUE, sep="," , dec=",")
f2<-read.csv(file="MS27Jun.csv", header=TRUE, sep="," , dec=",")
f3<-read.csv(file="MS06Jul.csv", header=TRUE, sep="," , dec=",")
f4<-read.csv(file="MS26Jul.csv", header=TRUE, sep="," , dec=",")

f0<-read.csv(file="Plantines2.csv", header=TRUE, sep="," , dec=",")


#### f1 ya tiene el area foliar en la columna A, PLANTINES NO TIENE A

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

tratamientos<-paste(f1$NombreV, f1$Dosis, sep="_")
f1$trat<-tratamientos
f2$trat<-tratamientos
f3$trat<-tratamientos
f4$trat<-tratamientos


modeloF2<- regModelAF(f2$trat, f2$AS, f2$HM)
modeloF3<-regModelAF(f3$trat, f3$AS, f3$HM)
modeloF4<-regModelAF(f4$trat, f4$AS, f4$HM)

#### Calcular el AF del las fechas

calcAf<-function(H, trat, MAF){
  #Se recorre fila por fila calculando PH*PENDIENA + ORDENADA para obtener un vector con el area foliar
  
  areasF<-c()
  
  for (i in c(1:32)){
    ordenada<-MAF$ord[MAF$tratamientos== trat[i]]
    pendiente<-MAF$pend[MAF$tratamientos== trat[i]]
    pesohojas<-H[i]
    af<-ordenada + pendiente*pesohojas
    areasF<-c(areasF, af) 
  }
  return(areasF)  
}

f2$A<-calcAf(f2$HSM+f2$HM, f2$trat, modeloF2)
f3$A<-calcAf(f3$HSM+f4$HM, f3$trat, modeloF3)
f4$A<-calcAf(f4$HSM+f4$HM, f4$trat, modeloF4)

fecha1<-as.Date("30/05/2023", format="%d/%m/%Y")
fecha2<-as.Date("27/06/2023", format="%d/%m/%Y")
fecha3<-as.Date("06/07/2023", format="%d/%m/%Y")
fecha4<-as.Date("26/07/2023", format="%d/%m/%Y")

AreasFoliares<-data.frame(
  "Bloque"= f1$Bloque,
  "UE"= f1$UE,
  "Variedad"=f1$NombreV,
  "Dosis"=f1$GrM2,
  "MS1"=f1$PesoT, "MS2"=f2$PesoT, "MS3"=f3$PesoT, "MS4"=f4$PesoT,
  "AF1"=f1$A, "AF2"=f2$A, "AF3"=f3$A, "AF4"=f4$A)

AreasFoliares$TAL1<-((AreasFoliares$MS2-AreasFoliares$MS1)/
                       as.integer(difftime(fecha2, fecha1)))*
  ((log(AreasFoliares$AF2)-log(AreasFoliares$AF1))/
     (AreasFoliares$AF2-AreasFoliares$AF1))

AreasFoliares$TAL2<-((AreasFoliares$MS3-AreasFoliares$MS2)/
                       as.integer(difftime(fecha3, fecha2)))*
  ((log(AreasFoliares$AF3)-log(AreasFoliares$AF2))/
     (AreasFoliares$AF3-AreasFoliares$AF2))

AreasFoliares$TAL3<-((AreasFoliares$MS4-AreasFoliares$MS3)/
                       as.integer(difftime(fecha4, fecha3)))*
  ((log(AreasFoliares$AF4)-log(AreasFoliares$AF3))/
     (AreasFoliares$AF4-AreasFoliares$AF3))

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

ggplot(descAFgbA, mapping=aes(x=fecha, y= media, linetype=factorA))+
  geom_line()+
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=0.5, size= 2, alpha=0.5)+
  labs(title= "Area Foliar Total",
       subtitle= "Primavera-Verano",
       x= "Fecha",
       y="Gramos MS/planta",
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
       y="Gramos MS/planta",
  )

####TASAS DE ASIMILACIÓN LÍQUIDA

#Variable Area FOLIAR

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


descTALgbA<- rbind(desc1TALA, desc2TALA, desc3TALA)


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


descTALgbV<- rbind(desc1TALV, desc2TALV, desc3TALV)

ggplot(descTALgbV, mapping=aes(x=fecha, y= media, linetype=Variedad))+
  geom_line()+
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=0.5, size= 2, alpha=0.5)+
  labs(title= "Area Foliar Total",
       subtitle= "Primavera-Verano",
       x= "Fecha",
       y="Gramos MS/planta",
  )
