##Altos gráficos: Area Foliar

AFensayo1<- readRDS("Resultados/AFensayo1.rds")
AFensayo2<-readRDS("Resultados/AFensayo2.rds")

AFensayo1$Nivel<-factor(AFensayo1$Nivel, 
                         levels=c("0","427","854","1282",
                                  "Penca Ancha", "Penca Verde"))
AFensayo2$Nivel<-factor(AFensayo2$Nivel, 
                         levels=c("0","492","984","1476",
                                  "Penca Ancha", "Penca Verde"))

f01<-as.Date("24/11/2022", format="%d/%m/%Y") #colocar aqui la fecha de trasplante del ensayo 1
f02<-as.Date("18/05/2023", format="%d/%m/%Y") #colocar aqui la fecha del trasplante del ensayo 2

AFensayo1$DDT<-as.integer(difftime(AFensayo1$fecha, f01, units="days"))
AFensayo2$DDT<-as.integer(difftime(AFensayo2$fecha, f02, units="days"))


library(ggplot2)
library(patchwork)

g1<-ggplot(data=AFensayo1[AFensayo1$factor=="Abono",],
           mapping= aes(x= DDT, y= media, color= Nivel))+
  geom_line()+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.5, size= 0.5, alpha=0.5)+
  labs(title= "A.",
       x= "DDT",
       y=expression("AF ( "~cm^2~")"),
       color= expression("Abono (g "~m^-2~")"))+
  theme(legend.position = "bottom")

g2<- ggplot(data=AFensayo1[AFensayo1$factor=="Variedad",],
            mapping= aes(x= DDT, y= media, color= Nivel))+
  geom_line()+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.5, size= 0.5, alpha=0.5)+
  labs(title= "B.",
       x= "DDT",
       y=expression("AF ( "~cm^2~")"),
       color= "Variedad")+
  theme(legend.position = "bottom")

g3<-ggplot(data=AFensayo2[AFensayo2$factor=="Abono",],
           mapping= aes(x= DDT, y= media, color= Nivel))+
  geom_line()+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.5, size= 0.5, alpha=0.5)+
  labs(title= "C.",
       x= "DDT",
       y=expression("AF ("~cm^2~")"),
       color= expression("Abono (g "~m^-2~")"))+
  theme(legend.position = "bottom")

g4<- ggplot(data=AFensayo2[AFensayo2$factor=="Variedad",],
            mapping= aes(x= DDT, y= media, color= Nivel))+
  geom_line()+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.5, size= 0.5, alpha=0.5)+
  labs(title= "D.",
       x= "DDT",
       y=expression("AF ("~cm^2~")"),
       color= "Variedad")+
  theme(legend.position = "bottom")
g1 + g2 + g3 + g4


