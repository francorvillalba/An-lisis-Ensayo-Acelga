#Ensayo 2

##Obtener MS TOTAL de cada fecha. 

### Importacion de datos
f1<-read.csv(file="MS30May.csv", header=TRUE, sep="," , dec=",")
f2<-read.csv(file="MS27Jun.csv", header=TRUE, sep="," , dec=",")
f3<-read.csv(file="MS06Jul.csv", header=TRUE, sep="," , dec=",")
f4<-read.csv(file="MS26Jul.csv", header=TRUE, sep="," , dec=",")

f0<-read.csv(file="Plantines2.csv", header=TRUE, sep="," , dec=",")

###Unión de datos (solo MS Total)

CrecimientoE2<-data.frame(
  "bloque"=f1$Bloque,
  "UE"=f1$UE,
  "Dosis"=f1$Dosis,
  "Var"=f1$Var,
  "GrM2"=f1$GrM2,
  "NombreV"=f1$NombreV,
  "MS1"=f1$PesoT/3,
  "MS2"=f2$PesoT/3,
  "MS3"=f3$PesoT/3,
  "MS4"=f4$PesoT/3
)
### Le agrego a cada UE la media del peso de los plantines de la variedad
MSp<-c()
for (i in c(1:32)){
  MSp<-c(MSp, mean(f0$PT[f0$Variedad==CrecimientoE2$Var[i]])/3)
}
CrecimientoE2$MS0<-MSp
##Tasa de Crecimiento Absoluta
###FECHAS
fecha0<-as.Date("18/05/2023", format="%d/%m/%Y")
fecha1<-as.Date("30/05/2023", format="%d/%m/%Y")
fecha2<-as.Date("27/06/2023", format="%d/%m/%Y")
fecha3<-as.Date("06/07/2023", format="%d/%m/%Y")


fecha4<-as.Date("26/07/2023", format="%d/%m/%Y")

###Calculo Tasas
CrecimientoE2$TCA1<-(CrecimientoE2$MS1-CrecimientoE2$MS0)/as.integer(difftime(fecha1, fecha0, units='days'))

CrecimientoE2$TCA2<-(CrecimientoE2$MS2-CrecimientoE2$MS1)/as.integer(difftime(fecha2, fecha1, units='days'))

CrecimientoE2$TCA3<-(CrecimientoE2$MS3-CrecimientoE2$MS2)/as.integer(difftime(fecha3, fecha2, units='days'))


CrecimientoE2$TCA4<-(CrecimientoE2$MS4-CrecimientoE2$MS3)/as.integer(difftime(fecha4, fecha3, units='days'))

mean(CrecimientoE2$TCA3[CrecimientoE2$Dosis==CrecimientoE2$Dosis[] & CrecimientoE2$Var==CrecimientoE2$Var[]])                                                                    

as.integer(difftime(fecha4, fecha3, units='days'))

CrecimientoE2$MS3[17]<-CrecimientoE2$MS2[17]+as.integer(difftime(fecha3, fecha2, units='days'))*mean(CrecimientoE2$TCA3[CrecimientoE2$Dosis==CrecimientoE2$Dosis[17] & CrecimientoE2$Var==CrecimientoE2$Var[17]]) 
CrecimientoE2$MS3[27]<-CrecimientoE2$MS2[27]+as.integer(difftime(fecha3, fecha2, units='days'))*mean(CrecimientoE2$TCA3[CrecimientoE2$Dosis==CrecimientoE2$Dosis[27] & CrecimientoE2$Var==CrecimientoE2$Var[27]]) 
CrecimientoE2$MS3[21]<-CrecimientoE2$MS2[21]+as.integer(difftime(fecha3, fecha2, units='days'))*mean(CrecimientoE2$TCA3[CrecimientoE2$Dosis==CrecimientoE2$Dosis[21] & CrecimientoE2$Var==CrecimientoE2$Var[21]]) 
CrecimientoE2$MS3[11]<-CrecimientoE2$MS2[11]+as.integer(difftime(fecha3, fecha2, units='days'))*mean(CrecimientoE2$TCA3[CrecimientoE2$Dosis==CrecimientoE2$Dosis[11] & CrecimientoE2$Var==CrecimientoE2$Var[11]]) 
CrecimientoE2$MS3[1]<-CrecimientoE2$MS2[1]+as.integer(difftime(fecha3, fecha2, units='days'))*mean(CrecimientoE2$TCA3[CrecimientoE2$Dosis==CrecimientoE2$Dosis[1] & CrecimientoE2$Var==CrecimientoE2$Var[1]]) 
CrecimientoE2$MS3[16]<-CrecimientoE2$MS2[16]+as.integer(difftime(fecha3, fecha2, units='days'))*mean(CrecimientoE2$TCA3[CrecimientoE2$Dosis==CrecimientoE2$Dosis[16] & CrecimientoE2$Var==CrecimientoE2$Var[16]]) 
CrecimientoE2$MS3[13]<-CrecimientoE2$MS2[13]+as.integer(difftime(fecha3, fecha2, units='days'))*mean(CrecimientoE2$TCA3[CrecimientoE2$Dosis==CrecimientoE2$Dosis[13] & CrecimientoE2$Var==CrecimientoE2$Var[13]]) 
CrecimientoE2$MS3[26]<-CrecimientoE2$MS2[26]+as.integer(difftime(fecha3, fecha2, units='days'))*mean(CrecimientoE2$TCA3[CrecimientoE2$Dosis==CrecimientoE2$Dosis[26] & CrecimientoE2$Var==CrecimientoE2$Var[26]]) 
CrecimientoE2$MS3[14]<-CrecimientoE2$MS2[14]+as.integer(difftime(fecha3, fecha2, units='days'))*mean(CrecimientoE2$TCA3[CrecimientoE2$Dosis==CrecimientoE2$Dosis[14] & CrecimientoE2$Var==CrecimientoE2$Var[14]]) 


CrecimientoE2$MS4[1]<-CrecimientoE2$MS3[1]+as.integer(difftime(fecha4, fecha3, units='days'))*mean(CrecimientoE2$TCA4[CrecimientoE2$Dosis==CrecimientoE2$Dosis[1] & CrecimientoE2$Var==CrecimientoE2$Var[1]]) 
CrecimientoE2$MS4[6]<-CrecimientoE2$MS3[6]+as.integer(difftime(fecha4, fecha3, units='days'))*mean(CrecimientoE2$TCA4[CrecimientoE2$Dosis==CrecimientoE2$Dosis[6] & CrecimientoE2$Var==CrecimientoE2$Var[6]]) 
CrecimientoE2$MS4[8]<-CrecimientoE2$MS3[8]+as.integer(difftime(fecha4, fecha3, units='days'))*mean(CrecimientoE2$TCA4[CrecimientoE2$Dosis==CrecimientoE2$Dosis[8] & CrecimientoE2$Var==CrecimientoE2$Var[8]]) 
CrecimientoE2$MS4[30]<-CrecimientoE2$MS3[30]+as.integer(difftime(fecha4, fecha3, units='days'))*mean(CrecimientoE2$TCA4[CrecimientoE2$Dosis==CrecimientoE2$Dosis[30] & CrecimientoE2$Var==CrecimientoE2$Var[30]]) 
CrecimientoE2$MS4[32]<-CrecimientoE2$MS3[32]+as.integer(difftime(fecha4, fecha3, units='days'))*mean(CrecimientoE2$TCA4[CrecimientoE2$Dosis==CrecimientoE2$Dosis[32] & CrecimientoE2$Var==CrecimientoE2$Var[32]]) 
CrecimientoE2$MS4[11]<-CrecimientoE2$MS3[11]+as.integer(difftime(fecha4, fecha3, units='days'))*mean(CrecimientoE2$TCA4[CrecimientoE2$Dosis==CrecimientoE2$Dosis[11] & CrecimientoE2$Var==CrecimientoE2$Var[11]]) 
CrecimientoE2$MS4[19]<-CrecimientoE2$MS3[19]+as.integer(difftime(fecha4, fecha3, units='days'))*mean(CrecimientoE2$TCA4[CrecimientoE2$Dosis==CrecimientoE2$Dosis[19] & CrecimientoE2$Var==CrecimientoE2$Var[19]]) 
CrecimientoE2$MS4[17]<-CrecimientoE2$MS3[17]+as.integer(difftime(fecha4, fecha3, units='days'))*mean(CrecimientoE2$TCA4[CrecimientoE2$Dosis==CrecimientoE2$Dosis[17] & CrecimientoE2$Var==CrecimientoE2$Var[17]]) 
CrecimientoE2$MS4[21]<-CrecimientoE2$MS3[21]+as.integer(difftime(fecha4, fecha3, units='days'))*mean(CrecimientoE2$TCA4[CrecimientoE2$Dosis==CrecimientoE2$Dosis[21] & CrecimientoE2$Var==CrecimientoE2$Var[21]]) 

###Calculo tasa de crecimento Relativa: 
CrecimientoE2$TCR1<-(log(CrecimientoE2$MS1)-log(CrecimientoE2$MS0))/as.integer(difftime(fecha1, fecha0, units='days'))

CrecimientoE2$TCR2<-(log(CrecimientoE2$MS2)-log(CrecimientoE2$MS1))/as.integer(difftime(fecha2, fecha1, units='days'))

CrecimientoE2$TCR3<-(log(CrecimientoE2$MS3)-log(CrecimientoE2$MS2))/as.integer(difftime(fecha3, fecha2, units='days'))


CrecimientoE2$TCR4<-(log(CrecimientoE2$MS4)-log(CrecimientoE2$MS3))/as.integer(difftime(fecha4, fecha3, units='days'))
#-----Exportar CrecimientoE2

saveRDS(CrecimientoE2, file="Resultados/CrecimientoE2_v2.rds")

#-----
library(ExpDes.pt)                                                                     
?fat2.dbc
fat2.dbc(
  CrecimientoE2$GrM2,
  CrecimientoE2$NombreV,
  CrecimientoE2$bloque,
  CrecimientoE2$TCA4,
  quali = c(FALSE, TRUE),
  mcomp = "tukey",
  fac.names = c("Gramos de abono/m2", "Variedad"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

CrecimientoE2$TCA5<-(CrecimientoE2$TCA4-CrecimientoE2$TCA2)/as.integer(difftime(fecha4, fecha2, units='days'))


###Analisis TCA1
fat2.dbc(
  CrecimientoE2$GrM2,
  CrecimientoE2$NombreV,
  CrecimientoE2$bloque,
  CrecimientoE2$TCR4,
  quali = c(FALSE, TRUE),
  mcomp = "tukey",
  fac.names = c("Gramos de abono/m2", "Variedad"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)
####Analisis TCA2
fat2.dbc(
  CrecimientoE2$GrM2,
  CrecimientoE2$NombreV,
  CrecimientoE2$bloque,
  CrecimientoE2$TCA2,
  quali = c(FALSE, TRUE),
  mcomp = "tukey",
  fac.names = c("Gramos de abono/m2", "Variedad"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)
####Analisis TCA3
fat2.dbc(
  CrecimientoE2$GrM2,
  CrecimientoE2$NombreV,
  CrecimientoE2$bloque,
  CrecimientoE2$TCA3,
  quali = c(FALSE, TRUE),
  mcomp = "tukey",
  fac.names = c("Gramos de abono/m2", "Variedad"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)
####Analisis TCA4
fat2.dbc(
  CrecimientoE2$GrM2,
  CrecimientoE2$NombreV,
  CrecimientoE2$bloque,
  CrecimientoE2$MS4,
  quali = c(FALSE, TRUE),
  mcomp = "tukey",
  fac.names = c("Gramos de abono/m2", "Variedad"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

### Estadistica descripitiva por dosis de abono
library(dplyr)

###Por dosis de abono
CrecimientoE2$factorA<-as.factor(CrecimientoE2$GrM2)
###Fecha 1 por variedad
desc1A<-CrecimientoE2 %>% group_by(factorA) %>% summarise(obs = n(),
                                                          max= max(MS1),
                                                          min= min(MS1), 
                                                          media= mean(MS1),
                                                          DS= sd(MS1),
                                                          EE= sd(MS1)/sqrt(n()),
                                                          ymin= mean(MS1)-(sd(MS1)/sqrt(n())),
                                                          ymax= mean(MS1)+(sd(MS1)/sqrt(n())))

desc1A$fecha<-fecha1
###Fecha 2 por variedad
desc2A<-CrecimientoE2 %>% group_by(factorA) %>% summarise(obs = n(),
                                                          max= max(MS2),
                                                          min= min(MS2), 
                                                          media= mean(MS2),
                                                          DS= sd(MS2),
                                                          EE= sd(MS2)/sqrt(n()),
                                                          ymin= mean(MS2)-(sd(MS2)/sqrt(n())),
                                                          ymax= mean(MS2)+(sd(MS2)/sqrt(n())))

desc2A$fecha<-fecha2

### Fecha 3 por variedad
desc3A<-CrecimientoE2 %>% group_by(factorA) %>% summarise(obs = n(),
                                                          max= max(MS3),
                                                          min= min(MS3), 
                                                          media= mean(MS3),
                                                          DS= sd(MS3),
                                                          EE= sd(MS3)/sqrt(n()),
                                                          ymin= mean(MS3)-(sd(MS3)/sqrt(n())),
                                                          ymax= mean(MS3)+(sd(MS3)/sqrt(n())))

desc3A$fecha<-fecha3
#por variedad
desc4A<-CrecimientoE2 %>% group_by(factorA) %>% summarise(obs = n(),
                                                          max= max(MS4),
                                                          min= min(MS4), 
                                                          media= mean(MS4),
                                                          DS= sd(MS4),
                                                          EE= sd(MS4)/sqrt(n()),
                                                          ymin= mean(MS4)-(sd(MS4)/sqrt(n())),
                                                          ymax= mean(MS4)+(sd(MS4)/sqrt(n())))
desc4A$fecha<-fecha4
###plantines por variedad
desc0A<-CrecimientoE2 %>% group_by(factorA) %>% summarise(obs = n(),
                                                          max= max(MS0),
                                                          min= min(MS0), 
                                                          media= mean(MS0),
                                                          DS= sd(MS0),
                                                          EE= sd(MS0)/sqrt(n()),
                                                          ymin= mean(MS0)-(sd(MS0)/sqrt(n())),
                                                          ymax= mean(MS0)-(sd(MS0)/sqrt(n())))

desc0A$fecha<-fecha0

###Unir en un data frame:

msensayo2A<-rbind(desc0A, desc1A, desc2A, desc3A, desc4A)
msensayo2A$group<-"Abono"
library(ggplot2)

ggplot(msensayo1A, mapping=aes(x=fecha, y= media, color=factorA))+
  geom_line()+
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=0.5, size= 2, alpha=0.5)+
  labs(title= "Acumulación de Materia Seca total",
       subtitle= "Primavera-Verano",
       x= "Fecha",
       y="Gramos MS/planta",
       color="Gramos de Abono por M2"
  )
####### Por Variedad
###Fecha 1 por variedad
desc1<-CrecimientoE2 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                         max= max(MS1),
                                                         min= min(MS1), 
                                                         media= mean(MS1),
                                                         DS= sd(MS1),
                                                         EE= sd(MS1)/sqrt(n()),
                                                         ymin= mean(MS1)-(sd(MS1)/sqrt(n())),
                                                         ymax= mean(MS1)+(sd(MS1)/sqrt(n())))

desc1$fecha<-fecha1
###Fecha 2 por variedad
desc2<-CrecimientoE2 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                         max= max(MS2),
                                                         min= min(MS2), 
                                                         media= mean(MS2),
                                                         DS= sd(MS2),
                                                         EE= sd(MS2)/sqrt(n()),
                                                         ymin= mean(MS2)-(sd(MS2)/sqrt(n())),
                                                         ymax= mean(MS2)+(sd(MS2)/sqrt(n())))


desc2$fecha<-fecha2

### Fecha 3 por variedad
desc3<-CrecimientoE2 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                         max= max(MS3),
                                                         min= min(MS3), 
                                                         media= mean(MS3),
                                                         DS= sd(MS3),
                                                         EE= sd(MS3)/sqrt(n()),
                                                         ymin= mean(MS3)-(sd(MS3)/sqrt(n())),
                                                         ymax= mean(MS3)+(sd(MS3)/sqrt(n())))

desc3$fecha<-fecha3
#por variedad
desc4<-CrecimientoE2 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                         max= max(MS4),
                                                         min= min(MS4), 
                                                         media= mean(MS4),
                                                         DS= sd(MS4),
                                                         EE= sd(MS4)/sqrt(n()),
                                                         ymin= mean(MS4)-(sd(MS4)/sqrt(n())),
                                                         ymax= mean(MS4)+(sd(MS4)/sqrt(n())))
desc4$fecha<-fecha4
###plantines por variedad
desc0<-CrecimientoE2 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                         max= max(MS0),
                                                         min= min(MS0), 
                                                         media= mean(MS0),
                                                         DS= sd(MS0),
                                                         EE= sd(MS0)/sqrt(n()),
                                                         ymin= mean(MS0)-(sd(MS0)/sqrt(n())),
                                                         ymax= mean(MS0)-(sd(MS0)/sqrt(n())))

desc0$fecha<-fecha0

###Unir en un data frame:

msensayo2V<-rbind(desc0, desc1, desc2, desc3, desc4)
msensayo2V$group<-"variedad"

ggplot(msensayo1, mapping=aes(x=fecha, y= media/3, color=NombreV))+
  geom_line()+
  geom_errorbar(aes(ymin=ymin/3, ymax=ymax/3), width=0.5, size= 2, alpha=0.5)+
  labs(title= "Acumulación de Materia Seca total",
       subtitle= "Otoño-Invierno",
       x= "Fecha",
       y="Gramos MS/planta",
       color="Variedad"
  )
###Exportar MSENSA2

colnames(msensayo2A)[1]<-"nivel"
colnames(msensayo2V)[1]<-"nivel"
msensayo2<-rbind(msensayo2A, msensayo2V)
saveRDS(msensayo2, file="Resultados/msensayo2_v2.rds")



#Est desc TCA 
####### Por Variedad
###Fecha 1 por variedad
descVarTCA1<-CrecimientoE2 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                               max= max(TCA1),
                                                               min= min(TCA1), 
                                                               media= mean(TCA1),
                                                               DS= sd(TCA1),
                                                               EE= sd(TCA1)/sqrt(n()),
                                                               ymin= mean(TCA1)-(sd(TCA1)/sqrt(n())),
                                                               ymax= mean(TCA1)+(sd(TCA1)/sqrt(n())))

descVarTCA1$fecha<-fecha1
###Fecha 2 por variedad
descVarTCA2<-CrecimientoE2 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                               max= max(TCA2),
                                                               min= min(TCA2), 
                                                               media= mean(TCA2),
                                                               DS= sd(TCA2),
                                                               EE= sd(TCA2)/sqrt(n()),
                                                               ymin= mean(TCA2)-(sd(TCA2)/sqrt(n())),
                                                               ymax= mean(TCA2)+(sd(TCA2)/sqrt(n())))

descVarTCA2$fecha<-fecha2

### Fecha 3 por variedad
descVarTCA3<-CrecimientoE2 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                               max= max(TCA3),
                                                               min= min(TCA3), 
                                                               media= mean(TCA3),
                                                               DS= sd(TCA3),
                                                               EE= sd(TCA3)/sqrt(n()),
                                                               ymin= mean(TCA3)-(sd(TCA3)/sqrt(n())),
                                                               ymax= mean(TCA3)+(sd(TCA3)/sqrt(n())))

descVarTCA3$fecha<-fecha3
#por variedad
descVarTCA4<-CrecimientoE2 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                               max= max(TCA4),
                                                               min= min(TCA4), 
                                                               media= mean(TCA4),
                                                               DS= sd(TCA4),
                                                               EE= sd(TCA4)/sqrt(n()),
                                                               ymin= mean(TCA4)-(sd(TCA4)/sqrt(n())),
                                                               ymax= mean(TCA4)+(sd(TCA4)/sqrt(n())))
descVarTCA4$fecha<-fecha4

###Unir en un data frame:

TCA_Var_E2<-rbind(descVarTCA1, descVarTCA2, descVarTCA3, descVarTCA4)

ggplot(TCA_Var_E1, mapping=aes(x=fecha, y= media, color=NombreV))+
  geom_line()+
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=0.5, size= 2, alpha=0.5)+
  labs(title= "Tasa de Crecimiento Absoluta",
       subtitle= "Otoño-Invierno",
       x= "Fecha",
       y="Gramos MS/día",
       color="Variedad"
  )

#### Por dosis de abono
###Fecha 1 por dosis de abono
descAbTCA1<-CrecimientoE2 %>% group_by(factorA) %>% summarise(obs = n(),
                                                              max= max(TCA1),
                                                              min= min(TCA1), 
                                                              media= mean(TCA1),
                                                              DS= sd(TCA1),
                                                              EE= sd(TCA1)/sqrt(n()),
                                                              ymin= mean(TCA1)-(sd(TCA1)/sqrt(n())),
                                                              ymax= mean(TCA1)+(sd(TCA1)/sqrt(n())))

descAbTCA1$fecha<-fecha1
###Fecha 2 por variedad
descAbTCA2<-CrecimientoE2 %>% group_by(factorA) %>% summarise(obs = n(),
                                                              max= max(TCA2),
                                                              min= min(TCA2), 
                                                              media= mean(TCA2),
                                                              DS= sd(TCA2),
                                                              EE= sd(TCA2)/sqrt(n()),
                                                              ymin= mean(TCA2)-(sd(TCA2)/sqrt(n())),
                                                              ymax= mean(TCA2)+(sd(TCA2)/sqrt(n())))

descAbTCA2$fecha<-fecha2

### Fecha 3 por variedad
descAbTCA3<-CrecimientoE2 %>% group_by(factorA) %>% summarise(obs = n(),
                                                              max= max(TCA3),
                                                              min= min(TCA3), 
                                                              media= mean(TCA3),
                                                              DS= sd(TCA3),
                                                              EE= sd(TCA3)/sqrt(n()),
                                                              ymin= mean(TCA3)-(sd(TCA3)/sqrt(n())),
                                                              ymax= mean(TCA3)+(sd(TCA3)/sqrt(n())))

descAbTCA3$fecha<-fecha3
#por variedad
descAbTCA4<-CrecimientoE2 %>% group_by(factorA) %>% summarise(obs = n(),
                                                              max= max(TCA4),
                                                              min= min(TCA4), 
                                                              media= mean(TCA4),
                                                              DS= sd(TCA4),
                                                              EE= sd(TCA4)/sqrt(n()),
                                                              ymin= mean(TCA4)-(sd(TCA4)/sqrt(n())),
                                                              ymax= mean(TCA4)+(sd(TCA4)/sqrt(n())))
descAbTCA4$fecha<-fecha4


###Unir en un data frame:

TCA_Ab_E2<-rbind(descAbTCA1, descAbTCA2, descAbTCA3, descAbTCA4)

ggplot(TCA_Ab_E1, mapping=aes(x=fecha, y= media, color=factorA))+
  geom_line()+
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=0.5, size= 2, alpha=0.5)+
  labs(title= "Tasa de Crecimiento Absoluta",
       subtitle= "Primavera-Verano",
       x= "Fecha",
       y="Gramos MS/día",
       color="Gramos de Abono/m2"
  )

TCA_Ab_E2$factor<-"Abono"
TCA_Var_E2$factor<-"Variedad"
colnames(TCA_Ab_E2)[1]<-"nivel"
colnames(TCA_Var_E2)[1]<-"nivel"
TCAensayo2<-rbind( TCA_Ab_E2, TCA_Var_E2)
saveRDS(TCAensayo2, file="Resultados/TCAensayo2.rds")


#Est desc TCR 
####### Por Variedad
###Fecha 1 por variedad
descVarTCR1<-CrecimientoE2 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                               max= max(TCR1),
                                                               min= min(TCR1), 
                                                               media= mean(TCR1),
                                                               DS= sd(TCR1),
                                                               EE= sd(TCR1)/sqrt(n()),
                                                               ymin= mean(TCR1)-(sd(TCR1)/sqrt(n())),
                                                               ymax= mean(TCR1)+(sd(TCR1)/sqrt(n())))

descVarTCR1$fecha<-fecha1
###Fecha 2 por variedad
descVarTCR2<-CrecimientoE2 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                               max= max(TCR2),
                                                               min= min(TCR2), 
                                                               media= mean(TCR2),
                                                               DS= sd(TCR2),
                                                               EE= sd(TCR2)/sqrt(n()),
                                                               ymin= mean(TCR2)-(sd(TCR2)/sqrt(n())),
                                                               ymax= mean(TCR2)+(sd(TCR2)/sqrt(n())))

descVarTCR2$fecha<-fecha2

### Fecha 3 por variedad
descVarTCR3<-CrecimientoE2 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                               max= max(TCR3),
                                                               min= min(TCR3), 
                                                               media= mean(TCR3),
                                                               DS= sd(TCR3),
                                                               EE= sd(TCR3)/sqrt(n()),
                                                               ymin= mean(TCR3)-(sd(TCR3)/sqrt(n())),
                                                               ymax= mean(TCR3)+(sd(TCR3)/sqrt(n())))

descVarTCR3$fecha<-fecha3
#por variedad
descVarTCR4<-CrecimientoE2 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                               max= max(TCR4),
                                                               min= min(TCR4), 
                                                               media= mean(TCR4),
                                                               DS= sd(TCR4),
                                                               EE= sd(TCR4)/sqrt(n()),
                                                               ymin= mean(TCR4)-(sd(TCR4)/sqrt(n())),
                                                               ymax= mean(TCR4)+(sd(TCR4)/sqrt(n())))
descVarTCR4$fecha<-fecha4

###Unir en un data frame:

TCR_Var_E1<-rbind(descVarTCR1, descVarTCR2, descVarTCR3, descVarTCR4)

ggplot(TCR_Var_E1, mapping=aes(x=fecha, y= media, color=NombreV))+
  geom_line()+
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=0.5, size= 2, alpha=0.5)+
  labs(title= "Tasa de Crecimiento Relativa",
       subtitle= "Primavera-Verano",
       x= "Fecha",
       y="Gramos MS/Gr*día",
       color="Variedad"
  )

#### Por dosis de abono
###Fecha 1 por dosis de abono
descAbTCR1<-CrecimientoE2 %>% group_by(factorA) %>% summarise(obs = n(),
                                                              max= max(TCR1),
                                                              min= min(TCR1), 
                                                              media= mean(TCR1),
                                                              DS= sd(TCR1),
                                                              EE= sd(TCR1)/sqrt(n()),
                                                              ymin= mean(TCR1)-(sd(TCR1)/sqrt(n())),
                                                              ymax= mean(TCR1)+(sd(TCR1)/sqrt(n())))

descAbTCR1$fecha<-fecha1
###Fecha 2 por variedad
descAbTCR2<-CrecimientoE2 %>% group_by(factorA) %>% summarise(obs = n(),
                                                              max= max(TCR2),
                                                              min= min(TCR2), 
                                                              media= mean(TCR2),
                                                              DS= sd(TCR2),
                                                              EE= sd(TCR2)/sqrt(n()),
                                                              ymin= mean(TCR2)-(sd(TCR2)/sqrt(n())),
                                                              ymax= mean(TCR2)+(sd(TCR2)/sqrt(n())))

descAbTCR2$fecha<-fecha2

### Fecha 3 por variedad
descAbTCR3<-CrecimientoE2 %>% group_by(factorA) %>% summarise(obs = n(),
                                                              max= max(TCR3),
                                                              min= min(TCR3), 
                                                              media= mean(TCR3),
                                                              DS= sd(TCR3),
                                                              EE= sd(TCR3)/sqrt(n()),
                                                              ymin= mean(TCR3)-(sd(TCR3)/sqrt(n())),
                                                              ymax= mean(TCR3)+(sd(TCR3)/sqrt(n())))

descAbTCR3$fecha<-fecha3
#por variedad
descAbTCR4<-CrecimientoE2 %>% group_by(factorA) %>% summarise(obs = n(),
                                                              max= max(TCR4),
                                                              min= min(TCR4), 
                                                              media= mean(TCR4),
                                                              DS= sd(TCR4),
                                                              EE= sd(TCR4)/sqrt(n()),
                                                              ymin= mean(TCR4)-(sd(TCR4)/sqrt(n())),
                                                              ymax= mean(TCR4)+(sd(TCR4)/sqrt(n())))
descAbTCR4$fecha<-fecha4


###Unir en un data frame:

TCR_Ab_E2<-rbind(descAbTCR1, descAbTCR2, descAbTCR3, descAbTCR4)

ggplot(TCR_Ab_E1, mapping=aes(x=fecha, y= media, color=factorA))+
  geom_line()+
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=0.5, size= 2, alpha=0.5)+
  labs(title= "Tasa de Crecimiento Relativa",
       subtitle= "Primavera-Verano",
       x= "Fecha",
       y="Gramos MS/Gr*día",
       color="Gramos de Abono/m2"
  )

TCR_Ab_E2$factor<-"Abono"
TCR_Var_E1$factor<-"Variedad"
colnames(TCR_Ab_E2)[1]<-"nivel"
colnames(TCR_Var_E1)[1]<-"nivel"
TCRensayo2<-rbind( TCR_Ab_E2, TCR_Var_E1)
saveRDS(TCRensayo2, file="Resultados/TCRensayo2.rds")
