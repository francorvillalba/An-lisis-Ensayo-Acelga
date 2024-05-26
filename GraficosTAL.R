##Altos gráficos: TAL

TALensayo1<- readRDS("Resultados/TALensayo1_v2.rds")
TALensayo2<-readRDS("Resultados/TALensayo2.rds")

TALensayo1$Nivel<-factor(TALensayo1$Nivel, 
                        levels=c("0","427","854","1282",
                                 "Penca Ancha", "Penca Verde"))
TALensayo2$Nivel<-factor(TALensayo2$Nivel, 
                        levels=c("0","492","984","1476",
                                 "Penca Ancha", "Penca Verde"))

f01<-as.Date("24/11/2022", format="%d/%m/%Y") #colocar aqui la fecha de trasplante del ensayo 1
f02<-as.Date("18/05/2023", format="%d/%m/%Y") #colocar aqui la fecha del trasplante del ensayo 2

TALensayo1$DDT<-as.integer(difftime(TALensayo1$fecha, f01, units="days"))
TALensayo2$DDT<-as.integer(difftime(TALensayo2$fecha, f02, units="days"))


library(ggplot2)
library(patchwork)

g1<-ggplot(data=TALensayo1[TALensayo1$factor=="Abono",],
           mapping= aes(x= DDT, y= media, color= Nivel))+
  geom_line()+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.5, size= 0.5, alpha=0.5)+
  labs(title= "A.",
       x= "DDT",
       y=expression("TAL (g "~cm^-2~" "~dia^-1~")"),
       color= expression("Abono (g "~m^-2~")"))+
  theme(legend.position = "bottom")

g2<- ggplot(data=TALensayo1[TALensayo1$factor=="Variedad",],
            mapping= aes(x= DDT, y= media, color= Nivel))+
  geom_line()+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.5, size= 0.5, alpha=0.5)+
  labs(title= "B.",
       x= "DDT",
       y=expression("TAL (g "~cm^-2~" "~dia^-1~")"),
       color= "Variedad")+
  theme(legend.position = "bottom")

g3<-ggplot(data=TALensayo2[TALensayo2$factor=="Abono",],
           mapping= aes(x= DDT, y= media, color= Nivel))+
  geom_line()+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.5, size= 0.5, alpha=0.5)+
  labs(title= "C.",
       x= "DDT",
       y=expression("TAL (g "~cm^-2~" "~dia^-1~")"),
       color= expression("Abono (g "~m^-2~")"))+
  theme(legend.position = "bottom")

g4<- ggplot(data=TALensayo2[TALensayo2$factor=="Variedad",],
            mapping= aes(x= DDT, y= media, color= Nivel))+
  geom_line()+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.5, size= 0.5, alpha=0.5)+
  labs(title= "D.",
       x= "DDT",
       y=expression("TAL (g "~cm^-2~" "~dia^-1~")"),
       color= "Variedad")+
  theme(legend.position = "bottom")
g1 + g2 + g3 + g4
