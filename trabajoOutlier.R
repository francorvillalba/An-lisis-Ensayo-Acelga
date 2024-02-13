library(ggplot2)
library(ExpDes.pt)

MS06DIC<-read.csv("MS06DIC.csv", header=TRUE, sep=",", dec=",")

MS06DIC["Tratamientos"]<-paste(MS06DIC$NombreV, MS06DIC$Dosis, sep="_")
#funcion para reemplazar 1 dato


datofaltante<-function(r, t, nr, nt,
                       bloque,
                       tratamiento,
                       variable){sumb<- sum(variable[bloque==nr&is.na(variable)==FALSE])
                       sumt<-sum(variable[tratamiento==nt&is.na(variable)==FALSE])
                       sumS<-sum(variable[is.na(variable)==FALSE])
                       vf<-(r*sumb+t*sumt-sumS)/((r-1)*(t-1))
                       return(vf)}

#funcion cuando se pierden 2 UE
estimacionI<-function (nt, nr, bloque, tratamiento, variable){
  avT<-mean(variable[tratamiento==nt&is.na(variable)==FALSE])
  avB<-mean(variable[tratamiento==nt&is.na(variable)==FALSE])
  xi<-(avT+avB)/2
  return(xi)
}

##para practicar vamos a asignar 2 NA a la base de datos
MS06DIC$Peso_Raiz[14]<-NA
View(MS06DIC)

##estimacion inicial de 13, bloque 2, tratamiento Penca Verde_1
estimacionI("Penca Verde_1", 2, MS06DIC$Bloque, MS06DIC$Tratamientos, MS06DIC$Peso_Raiz)

##suplantamos 13 en la base de datos y estimamos 14
MS06DIC$Peso_Raiz[13]<-estimacionI("Penca Verde_1", 2, MS06DIC$Bloque, MS06DIC$Tratamientos, MS06DIC$Peso_Raiz)


datofaltante(4, 8, 2, "Penca Verde_2", 
             MS06DIC$Bloque,
             MS06DIC$Tratamientos,
             MS06DIC$Peso_Raiz)
##reemplazamos y estimamos 13

MS06DIC$Peso_Raiz[14]<-datofaltante(4, 8, 2, "Penca Verde_2", 
                      MS06DIC$Bloque,
                      MS06DIC$Tratamientos,
                      MS06DIC$Peso_Raiz)
MS06DIC$Peso_Raiz[13]<-datofaltante(4, 8, 2, "Penca Verde_1", 
                                    MS06DIC$Bloque,
                                    MS06DIC$Tratamientos,
                                    MS06DIC$Peso_Raiz)
#luego llamamos continuamente a la funcion hasta que no veamos diferencia

#PROBEMOS AHORA
  fat2.dbc( MS06DIC$GrM2,
            MS06DIC$NombreV,
            MS06DIC$Bloque,
            MS06DIC$Peso_Raiz,
            quali= c(FALSE, TRUE),
            mcomp="tukey",
            fac.names=c("Gramos Abono por m2", "Variedad")
  )
plot(aov(Peso_Raiz~Bloque+GrM2*NombreV, MS06DIC))  
