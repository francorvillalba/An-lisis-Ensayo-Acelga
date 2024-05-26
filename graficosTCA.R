##Altos gráficos: Tasa de Crecimiento Absoluta

TCAensayo1<- readRDS("Resultados/TCAensayo1.rds")
TCAensayo2<-readRDS("Resultados/TCAensayo2.rds")

TCAensayo1$nivel<-factor(TCAensayo1$nivel, 
                        levels=c("0","427","854","1282",
                                 "Penca Ancha", "Penca Verde"))
TCAensayo2$nivel<-factor(TCAensayo2$nivel, 
                        levels=c("0","492","984","1476",
                                 "Penca Ancha", "Penca Verde"))

f01<-as.Date("24/11/2022", format="%d/%m/%Y") #colocar aqui la fecha de trasplante del ensayo 1
f02<-as.Date("18/05/2023", format="%d/%m/%Y") #colocar aqui la fecha del trasplante del ensayo 2

TCAensayo1$DDT<-as.integer(difftime(TCAensayo1$fecha, f01, units="days"))
TCAensayo2$DDT<-as.integer(difftime(TCAensayo2$fecha, f02, units="days"))

colnames(msensayo2)

library(ggplot2)
library(patchwork)

g1<-ggplot(data=TCAensayo1[TCAensayo1$factor=="Abono",],
           mapping= aes(x= DDT, y= media, color= nivel))+
  geom_line()+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.5, size= 0.5, alpha=0.5)+
  labs(title= "A.",
       x= "DDT",
       y=expression("TCA (g "~día^-1~")"),
       color= expression("Abono (g "~m^-2~")"))+
  theme(legend.position = "bottom")

g2<- ggplot(data=TCAensayo1[TCAensayo1$factor=="Variedad",],
            mapping= aes(x= DDT, y= media, color= nivel))+
  geom_line()+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.5, size= 0.5, alpha=0.5)+
  labs(title= "B.",
       x= "DDT",
       y=expression("TCA (g "~día^-1~")"),
       color= "Variedad")+
  theme(legend.position = "bottom")

g3<-ggplot(data=TCAensayo2[TCAensayo2$factor=="Abono",],
           mapping= aes(x= DDT, y= media, color= nivel))+
  geom_line()+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.5, size= 0.5, alpha=0.5)+
  labs(title= "C.",
       x= "DDT",
       y=expression("TCA (g "~día^-1~")"),
       color= expression("Abono (g "~m^-2~")"))+
  theme(legend.position = "bottom")

g4<- ggplot(data=TCAensayo2[TCAensayo2$factor=="Variedad",],
            mapping= aes(x= DDT, y= media, color= nivel))+
  geom_line()+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.5, size= 0.5, alpha=0.5)+
  labs(title= "D.",
       x= "DDT",
       y=expression("TCA (g "~día^-1~")"),
       color= "Variedad")+
  theme(legend.position = "bottom")
g1 + g2 + g3 + g4
