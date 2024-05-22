#Ensayo 1

##Obtener MS TOTAL de cada fecha. 

### Importacion de datos
f1<-read.csv(file="MS06DIC.csv", header=TRUE, sep="," , dec=",")
f2<-read.csv(file="MS20DIC.csv", header=TRUE, sep="," , dec=",")
f3<-read.csv(file="MS03ENE.csv", header=TRUE, sep="," , dec=",")
f4<-read.csv(file="MS17ENE.csv", header=TRUE, sep="," , dec=",")

f0<-read.csv(file="plantines1.csv", header=TRUE, sep="," , dec=",")

###Unión de datos (solo MS Total)

CrecimientoE1<-data.frame(
  "bloque"=f1$Bloque,
  "UE"=f1$UE,
  "Dosis"=f1$Dosis,
  "Var"=f1$Variedad,
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
  MSp<-c(MSp, mean(f0$PesoTotal[f0$Variedad==CrecimientoE1$Var[i]]))
}
CrecimientoE1$MS0<-MSp
##Tasa de Crecimiento Absoluta
###FECHAS
fecha0<-as.Date("24/11/2022", format="%d/%m/%Y")
fecha1<-as.Date("06/12/2022", format="%d/%m/%Y")
fecha2<-as.Date("20/12/2022", format="%d/%m/%Y")
fecha3<-as.Date("03/01/2023", format="%d/%m/%Y")
fecha4<-as.Date("17/01/2023", format="%d/%m/%Y")

###Calculo Tasas
CrecimientoE1$TCA1<-(CrecimientoE1$MS1-CrecimientoE1$MS0)/as.integer(difftime(fecha1, fecha0, units='days'))

CrecimientoE1$TCA2<-(CrecimientoE1$MS2-CrecimientoE1$MS1)/as.integer(difftime(fecha2, fecha1, units='days'))

CrecimientoE1$TCA3<-(CrecimientoE1$MS3-CrecimientoE1$MS2)/as.integer(difftime(fecha3, fecha2, units='days'))

CrecimientoE1$TCA4<-(CrecimientoE1$MS4-CrecimientoE1$MS3)/as.integer(difftime(fecha4, fecha3, units='days'))

mean(CrecimientoE1$TCA4[CrecimientoE1$Dosis==4 & CrecimientoE1$Var==2])                                                                    

as.integer(difftime(fecha4, fecha3, units='days'))

CrecimientoE1$MS4[15]<-52.31
CrecimientoE1$MS4[6]<-41.04
CrecimientoE1$MS4[21]<-58.69
CrecimientoE1$MS4[24]<-42.12
CrecimientoE1$MS4[25]<-44.12

###Calculo tasa de crecimento Relativa: 
CrecimientoE1$TCR1<-(log(CrecimientoE1$MS1)-log(CrecimientoE1$MS0))/as.integer(difftime(fecha1, fecha0, units='days'))

CrecimientoE1$TCR2<-(log(CrecimientoE1$MS2)-log(CrecimientoE1$MS1))/as.integer(difftime(fecha2, fecha1, units='days'))

CrecimientoE1$TCR3<-(log(CrecimientoE1$MS3)-log(CrecimientoE1$MS2))/as.integer(difftime(fecha3, fecha2, units='days'))

CrecimientoE1$TCR4<-(log(CrecimientoE1$MS4)-log(CrecimientoE1$MS3))/as.integer(difftime(fecha4, fecha3, units='days'))
###Exportar CrecimientoE1

saveRDS(CrecimientoE1, file="Resultados/CrecimientoE1_v2.rds")


#-----

library(ExpDes.pt)                                                                     
?fat2.dbc
fat2.dbc(
  CrecimientoE1$GrM2,
  CrecimientoE1$NombreV,
  CrecimientoE1$bloque,
  CrecimientoE1$MS3,
  quali = c(FALSE, TRUE),
  mcomp = "tukey",
  fac.names = c("Gramos de abono/m2", "Variedad"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

###Analisis TCA1
fat2.dbc(
  CrecimientoE1$GrM2,
  CrecimientoE1$NombreV,
  CrecimientoE1$bloque,
  CrecimientoE1$TCA1,
  quali = c(FALSE, TRUE),
  mcomp = "tukey",
  fac.names = c("Gramos de abono/m2", "Variedad"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)
####Analisis TCA2
fat2.dbc(
  CrecimientoE1$GrM2,
  CrecimientoE1$NombreV,
  CrecimientoE1$bloque,
  CrecimientoE1$TCA2,
  quali = c(FALSE, TRUE),
  mcomp = "tukey",
  fac.names = c("Gramos de abono/m2", "Variedad"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)
####Analisis TCA3
fat2.dbc(
  CrecimientoE1$GrM2,
  CrecimientoE1$NombreV,
  CrecimientoE1$bloque,
  CrecimientoE1$TCA3,
  quali = c(FALSE, TRUE),
  mcomp = "tukey",
  fac.names = c("Gramos de abono/m2", "Variedad"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)
####Analisis TCA4
fat2.dbc(
  CrecimientoE1$GrM2,
  CrecimientoE1$NombreV,
  CrecimientoE1$bloque,
  CrecimientoE1$TCR2,
  quali = c(FALSE, TRUE),
  mcomp = "tukey",
  fac.names = c("Gramos de abono/m2", "Variedad"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

### Estadistica descripitiva por dosis de abono


install.packages("dplyr")
library(dplyr)


###Por dosis de abono
CrecimientoE1$factorA<-as.factor(CrecimientoE1$GrM2)
###Fecha 1 por variedad
desc1A<-CrecimientoE1 %>% group_by(factorA) %>% summarise(obs = n(),
                                                          max= max(MS1),
                                                          min= min(MS1), 
                                                          media= mean(MS1),
                                                          DS= sd(MS1),
                                                          EE= sd(MS1)/sqrt(n()),
                                                          ymin= mean(MS1)-(sd(MS1)/sqrt(n())),
                                                          ymax= mean(MS1)+(sd(MS1)/sqrt(n())))

desc1A$fecha<-fecha1
###Fecha 2 por variedad
desc2A<-CrecimientoE1 %>% group_by(factorA) %>% summarise(obs = n(),
                                                         max= max(MS2),
                                                         min= min(MS2), 
                                                         media= mean(MS2),
                                                         DS= sd(MS2),
                                                         EE= sd(MS2)/sqrt(n()),
                                                         ymin= mean(MS2)-(sd(MS2)/sqrt(n())),
                                                         ymax= mean(MS2)+(sd(MS2)/sqrt(n())))

desc2A$fecha<-fecha2

### Fecha 3 por variedad
desc3A<-CrecimientoE1 %>% group_by(factorA) %>% summarise(obs = n(),
                                                         max= max(MS3),
                                                         min= min(MS3), 
                                                         media= mean(MS3),
                                                         DS= sd(MS3),
                                                         EE= sd(MS3)/sqrt(n()),
                                                         ymin= mean(MS3)-(sd(MS3)/sqrt(n())),
                                                         ymax= mean(MS3)+(sd(MS3)/sqrt(n())))

desc3A$fecha<-fecha3

#por variedad
desc4A<-CrecimientoE1 %>% group_by(factorA) %>% summarise(obs = n(),
                                                         max= max(MS4),
                                                         min= min(MS4), 
                                                         media= mean(MS4),
                                                         DS= sd(MS4),
                                                         EE= sd(MS4)/sqrt(n()),
                                                         ymin= mean(MS4)-(sd(MS4)/sqrt(n())),
                                                         ymax= mean(MS4)+(sd(MS4)/sqrt(n())))
desc4A$fecha<-fecha4
###plantines por variedad
desc0A<-CrecimientoE1 %>% group_by(factorA) %>% summarise(obs = n(),
                                                         max= max(MS0),
                                                         min= min(MS0), 
                                                         media= mean(MS0),
                                                         DS= sd(MS0),
                                                         EE= sd(MS0)/sqrt(n()),
                                                         ymin= mean(MS0)-(sd(MS0)/sqrt(n())),
                                                         ymax= mean(MS0)+(sd(MS0)/sqrt(n())))

desc0A$fecha<-fecha0

###Unir en un data frame:

msensayo1A<-rbind(desc0A, desc1A, desc2A, desc3A, desc4A)
msensayo1A$group<-"Abono"

library(ggplot2)

ggplot(msensayo1A, mapping=aes(x=fecha, y= media, linetype=factorA))+
  geom_line()+
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=0.5, size= 2, alpha=0.5)+
  labs(title= "Acumulación de Materia Seca total",
       subtitle= "Primavera-Verano",
       x= "Fecha",
       y="Gramos MS/planta",
       color="G"
  )
####### Por Variedad
###Fecha 1 por variedad
desc1V<-CrecimientoE1 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                         max= max(MS1),
                                                         min= min(MS1), 
                                                         media= mean(MS1),
                                                         DS= sd(MS1),
                                                         EE= sd(MS1)/sqrt(n()),
                                                         ymin= mean(MS1)-(sd(MS1)/sqrt(n())),
                                                         ymax= mean(MS1)+(sd(MS1)/sqrt(n())))

desc1V$fecha<-fecha1
###Fecha 2 por variedad
desc2<-CrecimientoE1 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                         max= max(MS2),
                                                         min= min(MS2), 
                                                         media= mean(MS2),
                                                         DS= sd(MS2),
                                                         EE= sd(MS2)/sqrt(n()),
                                                         ymin= mean(MS2)-(sd(MS2)/sqrt(n())),
                                                         ymax= mean(MS2)+(sd(MS2)/sqrt(n())))

desc2$fecha<-fecha2

### Fecha 3 por variedad
desc3<-CrecimientoE1 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                         max= max(MS3),
                                                         min= min(MS3), 
                                                         media= mean(MS3),
                                                         DS= sd(MS3),
                                                         EE= sd(MS3)/sqrt(n()),
                                                         ymin= mean(MS3)-(sd(MS3)/sqrt(n())),
                                                         ymax= mean(MS3)+(sd(MS3)/sqrt(n())))

desc3$fecha<-fecha3
#por variedad
desc4<-CrecimientoE1 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                         max= max(MS4),
                                                         min= min(MS4), 
                                                         media= mean(MS4),
                                                         DS= sd(MS4),
                                                         EE= sd(MS4)/sqrt(n()),
                                                         ymin= mean(MS4)-(sd(MS4)/sqrt(n())),
                                                         ymax= mean(MS4)+(sd(MS4)/sqrt(n())))
desc4$fecha<-fecha4
###plantines por variedad
desc0<-CrecimientoE1 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                         max= max(MS0),
                                                         min= min(MS0), 
                                                         media= mean(MS0),
                                                         DS= sd(MS0),
                                                         EE= sd(MS0)/sqrt(n()),
                                                         ymin= mean(MS0)-(sd(MS0)/sqrt(n())),
                                                         ymax= mean(MS0)-(sd(MS0)/sqrt(n())))

desc0$fecha<-fecha0

###Unir en un data frame:

msensayo1V<-rbind(desc0, desc1V, desc2, desc3, desc4)
msensayo1V$group<-"variedad"

ggplot(msensayo1, mapping=aes(x=fecha, y= media/3, color=NombreV))+
  geom_line()+
  geom_errorbar(aes(ymin=ymin/3, ymax=ymax/3), width=0.5, size= 2, alpha=0.5)+
  labs(title= "Acumulación de Materia Seca total",
       subtitle= "Primavera-Verano",
       x= "Fecha",
       y="Gramos MS/planta",
       color="Variedad"
  )

###Exportar MSENSAYO1


colnames(msensayo1A)[1]<-"nivel"
colnames(msensayo1V)[1]<-"nivel"
msensayo1<-rbind(msensayo1A, msensayo1V)
saveRDS(msensayo1, file="Resultados/msensayo1_v2.rds")

#Est desc TCA 
####### Por Variedad
###Fecha 1 por variedad
descVarTCA1<-CrecimientoE1 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                         max= max(TCA1),
                                                         min= min(TCA1), 
                                                         media= mean(TCA1),
                                                         DS= sd(TCA1),
                                                         EE= sd(TCA1)/sqrt(n()),
                                                         ymin= mean(TCA1)-(sd(TCA1)/sqrt(n())),
                                                         ymax= mean(TCA1)+(sd(TCA1)/sqrt(n())))

descVarTCA1$fecha<-fecha1
###Fecha 2 por variedad
descVarTCA2<-CrecimientoE1 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                         max= max(TCA2),
                                                         min= min(TCA2), 
                                                         media= mean(TCA2),
                                                         DS= sd(TCA2),
                                                         EE= sd(TCA2)/sqrt(n()),
                                                         ymin= mean(TCA2)-(sd(TCA2)/sqrt(n())),
                                                         ymax= mean(TCA2)+(sd(TCA2)/sqrt(n())))

descVarTCA2$fecha<-fecha2

### Fecha 3 por variedad
descVarTCA3<-CrecimientoE1 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                         max= max(TCA3),
                                                         min= min(TCA3), 
                                                         media= mean(TCA3),
                                                         DS= sd(TCA3),
                                                         EE= sd(TCA3)/sqrt(n()),
                                                         ymin= mean(TCA3)-(sd(TCA3)/sqrt(n())),
                                                         ymax= mean(TCA3)+(sd(TCA3)/sqrt(n())))

descVarTCA3$fecha<-fecha3
#por variedad
descVarTCA4<-CrecimientoE1 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                         max= max(TCA4),
                                                         min= min(TCA4), 
                                                         media= mean(TCA4),
                                                         DS= sd(TCA4),
                                                         EE= sd(TCA4)/sqrt(n()),
                                                         ymin= mean(TCA4)-(sd(TCA4)/sqrt(n())),
                                                         ymax= mean(TCA4)+(sd(TCA4)/sqrt(n())))
descVarTCA4$fecha<-fecha4

###Unir en un data frame:

TCA_Var_E1<-rbind(descVarTCA1, descVarTCA2, descVarTCA3, descVarTCA4)

ggplot(TCA_Var_E1, mapping=aes(x=fecha, y= media, color=NombreV))+
  geom_line()+
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=0.5, size= 2, alpha=0.5)+
  labs(title= "Tasa de Crecimiento Absoluta",
       subtitle= "Primavera-Verano",
       x= "Fecha",
       y="Gramos MS/día",
       color="Variedad"
  )

#### Por dosis de abono
###Fecha 1 por dosis de abono
descAbTCA1<-CrecimientoE1 %>% group_by(factorA) %>% summarise(obs = n(),
                                                            max= max(TCA1),
                                                            min= min(TCA1), 
                                                            media= mean(TCA1),
                                                            DS= sd(TCA1),
                                                            EE= sd(TCA1)/sqrt(n()),
                                                            ymin= mean(TCA1)-(sd(TCA1)/sqrt(n())),
                                                            ymax= mean(TCA1)+(sd(TCA1)/sqrt(n())))

descAbTCA1$fecha<-fecha1
###Fecha 2 por variedad
descAbTCA2<-CrecimientoE1 %>% group_by(factorA) %>% summarise(obs = n(),
                                                            max= max(TCA2),
                                                            min= min(TCA2), 
                                                            media= mean(TCA2),
                                                            DS= sd(TCA2),
                                                            EE= sd(TCA2)/sqrt(n()),
                                                            ymin= mean(TCA2)-(sd(TCA2)/sqrt(n())),
                                                            ymax= mean(TCA2)+(sd(TCA2)/sqrt(n())))

descAbTCA2$fecha<-fecha2

### Fecha 3 por variedad
descAbTCA3<-CrecimientoE1 %>% group_by(factorA) %>% summarise(obs = n(),
                                                            max= max(TCA3),
                                                            min= min(TCA3), 
                                                            media= mean(TCA3),
                                                            DS= sd(TCA3),
                                                            EE= sd(TCA3)/sqrt(n()),
                                                            ymin= mean(TCA3)-(sd(TCA3)/sqrt(n())),
                                                            ymax= mean(TCA3)+(sd(TCA3)/sqrt(n())))

descAbTCA3$fecha<-fecha3
#por variedad
descAbTCA4<-CrecimientoE1 %>% group_by(factorA) %>% summarise(obs = n(),
                                                            max= max(TCA4),
                                                            min= min(TCA4), 
                                                            media= mean(TCA4),
                                                            DS= sd(TCA4),
                                                            EE= sd(TCA4)/sqrt(n()),
                                                            ymin= mean(TCA4)-(sd(TCA4)/sqrt(n())),
                                                            ymax= mean(TCA4)+(sd(TCA4)/sqrt(n())))
descAbTCA4$fecha<-fecha4


###Unir en un data frame:

TCA_Ab_E1<-rbind(descAbTCA1, descAbTCA2, descAbTCA3, descAbTCA4)

ggplot(TCA_Ab_E1, mapping=aes(x=fecha, y= media, color=factorA))+
  geom_line()+
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=0.5, size= 2, alpha=0.5)+
  labs(title= "Tasa de Crecimiento Absoluta",
       subtitle= "Primavera-Verano",
       x= "Fecha",
       y="Gramos MS/día",
       color="Gramos de Abono/m2"
  )
# Exportar TCAensayo1


TCA_Ab_E1$factor<-"Abono"
TCA_Var_E1$factor<-"Variedad"
colnames(TCA_Ab_E1)[1]<-"nivel"
colnames(TCA_Var_E1)[1]<-"nivel"
TCAensayo1<-rbind( TCA_Ab_E1, TCA_Var_E1)
saveRDS(TCAensayo1, file="Resultados/TCAensayo1.rds")



#Est desc TCR 
####### Por Variedad
###Fecha 1 por variedad
descVarTCR1<-CrecimientoE1 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                               max= max(TCR1),
                                                               min= min(TCR1), 
                                                               media= mean(TCR1),
                                                               DS= sd(TCR1),
                                                               EE= sd(TCR1)/sqrt(n()),
                                                               ymin= mean(TCR1)-(sd(TCR1)/sqrt(n())),
                                                               ymax= mean(TCR1)+(sd(TCR1)/sqrt(n())))

descVarTCR1$fecha<-fecha1
###Fecha 2 por variedad
descVarTCR2<-CrecimientoE1 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                               max= max(TCR2),
                                                               min= min(TCR2), 
                                                               media= mean(TCR2),
                                                               DS= sd(TCR2),
                                                               EE= sd(TCR2)/sqrt(n()),
                                                               ymin= mean(TCR2)-(sd(TCR2)/sqrt(n())),
                                                               ymax= mean(TCR2)+(sd(TCR2)/sqrt(n())))

descVarTCR2$fecha<-fecha2

### Fecha 3 por variedad
descVarTCR3<-CrecimientoE1 %>% group_by(NombreV) %>% summarise(obs = n(),
                                                               max= max(TCR3),
                                                               min= min(TCR3), 
                                                               media= mean(TCR3),
                                                               DS= sd(TCR3),
                                                               EE= sd(TCR3)/sqrt(n()),
                                                               ymin= mean(TCR3)-(sd(TCR3)/sqrt(n())),
                                                               ymax= mean(TCR3)+(sd(TCR3)/sqrt(n())))

descVarTCR3$fecha<-fecha3
#por variedad
descVarTCR4<-CrecimientoE1 %>% group_by(NombreV) %>% summarise(obs = n(),
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
descAbTCR1<-CrecimientoE1 %>% group_by(factorA) %>% summarise(obs = n(),
                                                              max= max(TCR1),
                                                              min= min(TCR1), 
                                                              media= mean(TCR1),
                                                              DS= sd(TCR1),
                                                              EE= sd(TCR1)/sqrt(n()),
                                                              ymin= mean(TCR1)-(sd(TCR1)/sqrt(n())),
                                                              ymax= mean(TCR1)+(sd(TCR1)/sqrt(n())))

descAbTCR1$fecha<-fecha1
###Fecha 2 por variedad
descAbTCR2<-CrecimientoE1 %>% group_by(factorA) %>% summarise(obs = n(),
                                                              max= max(TCR2),
                                                              min= min(TCR2), 
                                                              media= mean(TCR2),
                                                              DS= sd(TCR2),
                                                              EE= sd(TCR2)/sqrt(n()),
                                                              ymin= mean(TCR2)-(sd(TCR2)/sqrt(n())),
                                                              ymax= mean(TCR2)+(sd(TCR2)/sqrt(n())))

descAbTCR2$fecha<-fecha2

### Fecha 3 por variedad
descAbTCR3<-CrecimientoE1 %>% group_by(factorA) %>% summarise(obs = n(),
                                                              max= max(TCR3),
                                                              min= min(TCR3), 
                                                              media= mean(TCR3),
                                                              DS= sd(TCR3),
                                                              EE= sd(TCR3)/sqrt(n()),
                                                              ymin= mean(TCR3)-(sd(TCR3)/sqrt(n())),
                                                              ymax= mean(TCR3)+(sd(TCR3)/sqrt(n())))

descAbTCR3$fecha<-fecha3
#por variedad
descAbTCR4<-CrecimientoE1 %>% group_by(factorA) %>% summarise(obs = n(),
                                                              max= max(TCR4),
                                                              min= min(TCR4), 
                                                              media= mean(TCR4),
                                                              DS= sd(TCR4),
                                                              EE= sd(TCR4)/sqrt(n()),
                                                              ymin= mean(TCR4)-(sd(TCR4)/sqrt(n())),
                                                              ymax= mean(TCR4)+(sd(TCR4)/sqrt(n())))
descAbTCR4$fecha<-fecha4


###Unir en un data frame:

TCR_Ab_E1<-rbind(descAbTCR1, descAbTCR2, descAbTCR3, descAbTCR4)

ggplot(TCR_Ab_E1, mapping=aes(x=fecha, y= media, color=factorA))+
  geom_line()+
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=0.5, size= 2, alpha=0.5)+
  labs(title= "Tasa de Crecimiento Relativa",
       subtitle= "Primavera-Verano",
       x= "Fecha",
       y="Gramos MS/Gr*día",
       color="Gramos de Abono/m2"
  )

# Exportar TCRensayo1

TCR_Ab_E1$factor<-"Abono"
TCR_Var_E1$factor<-"Variedad"
colnames(TCR_Ab_E1)[1]<-"nivel"
colnames(TCR_Var_E1)[1]<-"nivel"
TCRensayo1<-rbind( TCR_Ab_E1, TCR_Var_E1)
saveRDS(TCRensayo1, file="Resultados/TCRensayo1.rds")


