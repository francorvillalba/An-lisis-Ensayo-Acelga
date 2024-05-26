##Altos gráficos: Materia Seca

msensayo1<- readRDS("Resultados/msensayo1_v2.rds")
msensayo2<-readRDS("Resultados/msensayo2_v2.rds")

msensayo1$nivel<-factor(msensayo1$nivel, 
                        levels=c("0","427","854","1282",
                                 "Penca Ancha", "Penca Verde"))
msensayo2$nivel<-factor(msensayo2$nivel, 
                        levels=c("0","492","984","1476",
                                 "Penca Ancha", "Penca Verde"))

f01<-as.Date("24/11/2022", format="%d/%m/%Y") #colocar aqui la fecha de trasplante del ensayo 1
f02<-as.Date("18/05/2023", format="%d/%m/%Y") #colocar aqui la fecha del trasplante del ensayo 2

msensayo1$DDT<-as.integer(difftime(msensayo1$fecha, f01, units="days"))
msensayo2$DDT<-as.integer(difftime(msensayo2$fecha, f02, units="days"))

colnames(msensayo2)

library(ggplot2)
library(patchwork)

g1<-ggplot(data=msensayo1[msensayo1$group=="Abono",],
           mapping= aes(x= DDT, y= media, color= nivel))+
  geom_line()+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.5, size= 0.5, alpha=0.5)+
  labs(title= "A.",
       x= "DDT",
       y="MS (g)",
       color= expression("Abono (g "~m^-2~")"))+
  theme(legend.position = "bottom")

g2<- ggplot(data=msensayo1[msensayo1$group=="variedad",],
            mapping= aes(x= DDT, y= media, color= nivel))+
  geom_line()+
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.5, size= 0.5, alpha=0.5)+
  labs(title= "B.",
       x= "DDT",
       y="MS (g)",
       color= "Variedad")+
  theme(legend.position = "bottom")
  
  g3<-ggplot(data=msensayo2[msensayo2$group=="Abono",],
             mapping= aes(x= DDT, y= media, color= nivel))+
    geom_line()+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.5, size= 0.5, alpha=0.5)+
    labs(title= "C.",
         x= "DDT",
         y="MS (g)",
         color= expression("Abono (g "~m^-2~")"))+
    theme(legend.position = "bottom")
  
  g4<- ggplot(data=msensayo2[msensayo2$group=="variedad",],
              mapping= aes(x= DDT, y= media, color= nivel))+
    geom_line()+
    geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), width=0.5, size= 0.5, alpha=0.5)+
    labs(title= "D.",
         x= "DDT",
         y="MS (g)",
         color= "Variedad")+
    theme(legend.position = "bottom")
g1 + g2 + g3 + g4

#--------------
