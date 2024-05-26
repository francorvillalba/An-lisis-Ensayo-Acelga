##Altos gráficos: Tasa de Crecimiento Relativa

TCRensayo1<- readRDS("Resultados/TCRensayo1.rds")
TCRensayo2<-readRDS("Resultados/TCRensayo2.rds")

TCRensayo1$nivel<-factor(TCRensayo1$nivel, 
                         levels=c("0","427","854","1282",
                                  "Penca Ancha", "Penca Verde"))
TCRensayo2$nivel<-factor(TCRensayo2$nivel, 
                         levels=c("0","492","984","1476",
                                  "Penca Ancha", "Penca Verde"))

f01<-as.Date("24/11/2022", format="%d/%m/%Y") #colocar aqui la fecha de trasplante del ensayo 1
f02<-as.Date("18/05/2023", format="%d/%m/%Y") #colocar aqui la fecha del trasplante del ensayo 2

TCRensayo1$DDT<-as.integer(difftime(TCRensayo1$fecha, f01, units="days"))
TCRensayo2$DDT<-as.integer(difftime(TCRensayo2$fecha, f02, units="days"))


library(ggplot2)
library(patchwork)

g1<-ggplot(data=TCRensayo1[TCRensayo1$factor=="Abono",],
           mapping= aes(x= DDT, y= media, color= nivel))+
  geom_line()+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.5, size= 0.5, alpha=0.5)+
  labs(title= "A.",
       x= "DDT",
       y=expression("TCR (g "~g^-1~" "~dia^-1~")"),
       color= expression("Abono (g "~m^-2~")"))+
  theme(legend.position = "bottom")

g2<- ggplot(data=TCRensayo1[TCRensayo1$factor=="Variedad",],
            mapping= aes(x= DDT, y= media, color= nivel))+
  geom_line()+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.5, size= 0.5, alpha=0.5)+
  labs(title= "B.",
       x= "DDT",
       y=expression("TCR (g "~g^-1~" "~dia^-1~")"),
       color= "Variedad")+
  theme(legend.position = "bottom")

g3<-ggplot(data=TCRensayo2[TCRensayo2$factor=="Abono",],
           mapping= aes(x= DDT, y= media, color= nivel))+
  geom_line()+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.5, size= 0.5, alpha=0.5)+
  labs(title= "C.",
       x= "DDT",
       y=expression("TCR (g "~g^-1~" "~dia^-1~")"),
       color= expression("Abono (g "~m^-2~")"))+
  theme(legend.position = "bottom")

g4<- ggplot(data=TCRensayo2[TCRensayo2$factor=="Variedad",],
            mapping= aes(x= DDT, y= media, color= nivel))+
  geom_line()+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.5, size= 0.5, alpha=0.5)+
  labs(title= "D.",
       x= "DDT",
       y=expression("TCR (g "~g^-1~" "~dia^-1~")"),
       color= "Variedad")+
  theme(legend.position = "bottom")
g1 + g2 + g3 + g4
