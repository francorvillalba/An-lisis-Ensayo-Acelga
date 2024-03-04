tratamientos<-unique(MS20DIC$Tratamientos) #Creamos un vector con los tratamientos
 
#creamos vectores vacios para guardar cada parametro del modelo: ordenada al origen, pendiente de la recta
#pearson de la ordenada, pearson de la pendiente, r^2 y r^2 ajustado del modelo
ord <-c() 
prord<-c()
pend<-c()
prpend<-c()
rsq<-c()
radj<-c()

#El ciclo FOR va ir realizando una regresion por CADA TRATAMIENTO y guardando los datos en cada vector creado

for (i in tratamientos){
  HMT<-MS20DIC$HM[MS20DIC$Tratamientos== i] #PESO HOJAS MEDIDAS DEL TRATAMIENTO I
  AFT<-MS20DIC$AS[MS20DIC$Tratamientos== i] #AREA FOLIAR DE LAS HOJAS MEDIDAS DEL TRATAMIENTO I
  
  regresion<-summary(lm(AFT~HMT)) #SE EJECUTA LA REGRESION
  
  ord<-c(ord, regresion$coefficients[1,1]) #guarda el intercept del tratamiento i en un vector
  prord<-c(prord, regresion$coefficients[1,4]) #guarda el coeficiente de pearson de la ordenada para el tratamiento i
  pend<-c(pend, regresion$coefficients[2,1]) #guarda la pendiente del tratamiento i en un vector
  prpend<-c(prpend, regresion$coefficients[2,4]) #guarda el coeficiente de pearson de la pendienyte en el tratamiento i
  rsq<-c(rsq, regresion$r.squared) #guarda el r^2 del modelo al tratamiento i
  radj<-c(radj, regresion$adj.r.squared) #guarda el r^2 ajustado del modelo para el tratamiento i
}

# creamos un data frame con los vectores

modelosAF<-data.frame(tratamientos, ord, prord, pend, prpend, rsq, radj)

#Vemos el data frame
View(modelosAF)

ordenada<-modelosAF$ord[modelosAF$tratamientos=='Penca Verde_1']

#CALCULO DEL AREA FOLIAR
#Creamos una columna con el peso de las hojas:
MS20DIC$PH<-MS20DIC$HM+MS20DIC$HSM

#Se recorre fila por fila calculando PH*PENDIENA + ORDENADA para obtener un vector con el area foliar

areasF<-c()
paiterar<-c(1:32)

for (i in paiterar){
  ordenada<-modelosAF$ord[modelosAF$tratamientos== MS20DIC$Tratamientos[i]]
  pendiente<-modelosAF$pend[modelosAF$tratamientos== MS20DIC$Tratamientos[i]]
  pesohojas<-MS20DIC$PH[i]
  af<-ordenada + pendiente*pesohojas
  areasF<-c(areasF, af) 
}

#Se agrega el vector al data frame

MS20DIC$AREAF<-areasF
  
#Analisis estadistico del AreaFoliar

library(ExpDes.pt)

fat2.dbc( MS20DIC$GrM2,
          MS20DIC$NombreV,
          MS20DIC$Bloque,
          MS20DIC$AREAF,
          quali= c(FALSE, TRUE),
          mcomp="tukey",
          fac.names=c("Gramos Abono por m2", "Variedad")
)

