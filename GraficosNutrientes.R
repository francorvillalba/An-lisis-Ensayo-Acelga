Nensayo1<-readRDS("Resultados/Nensayo1.rds")
Nensayo2<-readRDS("Resultados/Nensayo2.rds")

library(ggplot2)
library(patchwork)

n1 <-ggplot(data=Nensayo1[Nensayo1$Factor=="Abono",], mapping=aes(x=DDT, y=Nacu, color= Nivel))+
  geom_line()+
  labs(
    title="A.",
    x="DDT",
    y="Nitrógeno (g)",
    color= expression("Abono (g "~m^-2~")"))+
     theme(legend.position = "bottom")

n2 <-ggplot(data=Nensayo1[Nensayo1$Factor=="variedad",], mapping=aes(x=DDT, y=Nacu, color= Nivel))+
  geom_line()+
  labs(
    title="B.",
    x="DDT",
    y="Nitrógeno (g)",
    color= "Variedad")+
  theme(legend.position = "bottom")

n3 <-ggplot(data=Nensayo2[Nensayo1$Factor=="Abono",], mapping=aes(x=DDT, y=Nacu, color= Nivel))+
  geom_line()+
  labs(
    title="C.",
    x="DDT",
    y="Nitrógeno (g)",
    color= expression("Abono (g "~m^-2~")"))+
  theme(legend.position = "bottom")
n4 <-ggplot(data=Nensayo2[Nensayo2$Factor=="Variedad",], mapping=aes(x=DDT, y=Nacu, color= Nivel))+
  geom_line()+
  labs(
    title="D.",
    x="DDT",
    y="Nitrógeno (g)",
    color= "Variedad")+
  theme(legend.position = "bottom")


n1+n2+n3+n4

###Potasio

k1 <-ggplot(data=Nensayo1[Nensayo1$Factor=="Abono",], mapping=aes(x=DDT, y=Kacu, color= Nivel))+
  geom_line()+
  labs(
    title="A.",
    x="DDT",
    y="Potasio (g)",
    color= expression("Abono (g "~m^-2~")"))+
  theme(legend.position = "bottom")

k2 <-ggplot(data=Nensayo1[Nensayo1$Factor=="variedad",], mapping=aes(x=DDT, y=Kacu, color= Nivel))+
  geom_line()+
  labs(
    title="B.",
    x="DDT",
    y="Potasio (g)",
    color= "Variedad")+
  theme(legend.position = "bottom")

k3 <-ggplot(data=Nensayo2[Nensayo1$Factor=="Abono",], mapping=aes(x=DDT, y=Kacu, color= Nivel))+
  geom_line()+
  labs(
    title="C.",
    x="DDT",
    y="Potasio (g)",
    color= expression("Abono (g "~m^-2~")"))+
  theme(legend.position = "bottom")
k4 <-ggplot(data=Nensayo2[Nensayo2$Factor=="variedad",], mapping=aes(x=DDT, y=Kacu, color= Nivel))+
  geom_line()+
  labs(
    title="D.",
    x="DDT",
    y="Potasio (g)",
    color= "Variedad")+
  theme(legend.position = "bottom")

k1+k2+k3+k4


## Fósforo


p1 <-ggplot(data=Nensayo1[Nensayo1$Factor=="Abono",], mapping=aes(x=DDT, y=Pacu, color= Nivel))+
  geom_line()+
  labs(
    title="A.",
    x="DDT",
    y="Fósforo (g)",
    color= expression("Abono (g "~m^-2~")"))+
  theme(legend.position = "bottom")

p2 <-ggplot(data=Nensayo1[Nensayo1$Factor=="variedad",], mapping=aes(x=DDT, y=Pacu, color= Nivel))+
  geom_line()+
  labs(
    title="B.",
    x="DDT",
    y="Fósforo (g)",
    color= "Variedad")+
  theme(legend.position = "bottom")

p3 <-ggplot(data=Nensayo2[Nensayo1$Factor=="Abono",], mapping=aes(x=DDT, y=Pacu, color= Nivel))+
  geom_line()+
  labs(
    title="C.",
    x="DDT",
    y="Fósforo (g)",
    color= expression("Abono (g "~m^-2~")"))+
  theme(legend.position = "bottom")
p4 <-ggplot(data=Nensayo2[Nensayo2$Factor=="variedad",], mapping=aes(x=DDT, y=Pacu, color= Nivel))+
  geom_line()+
  labs(
    title="D.",
    x="DDT",
    y="Fósforo (g)",
    color= "Variedad")+
  theme(legend.position = "bottom")

p1 + p2 + p3 + p4

#Calcio

Ca1 <-ggplot(data=Nensayo1[Nensayo1$Factor=="Abono",], mapping=aes(x=DDT, y=Caacu, color= Nivel))+
  geom_line()+
  labs(
    title="A.",
    x="DDT",
    y="Calcio (g)",
    color= expression("Abono (g "~m^-2~")"))+
  theme(legend.position = "bottom")

Ca2 <-ggplot(data=Nensayo1[Nensayo1$Factor=="variedad",], mapping=aes(x=DDT, y=Caacu, color= Nivel))+
  geom_line()+
  labs(
    title="B.",
    x="DDT",
    y="Calcio (g)",
    color= "Variedad")+
  theme(legend.position = "bottom")

Ca3 <-ggplot(data=Nensayo2[Nensayo1$Factor=="Abono",], mapping=aes(x=DDT, y=Caacu, color= Nivel))+
  geom_line()+
  labs(
    title="C.",
    x="DDT",
    y="Calcio (g)",
    color= expression("Abono (g "~m^-2~")"))+
  theme(legend.position = "bottom")
Ca4 <-ggplot(data=Nensayo2[Nensayo2$Factor=="variedad",], mapping=aes(x=DDT, y=Caacu, color= Nivel))+
  geom_line()+
  labs(
    title="D.",
    x="DDT",
    y="Calcio (g)",
    color= "Variedad")+
  theme(legend.position = "bottom")

Ca1 + Ca2 + Ca3 + Ca4

#Calcio

Mg1 <-ggplot(data=Nensayo1[Nensayo1$Factor=="Abono",], mapping=aes(x=DDT, y=Mgacu, color= Nivel))+
  geom_line()+
  labs(
    title="A.",
    x="DDT",
    y="Magnesio (g)",
    color= expression("Abono (g "~m^-2~")"))+
  theme(legend.position = "bottom")

Mg2 <-ggplot(data=Nensayo1[Nensayo1$Factor=="variedad",], mapping=aes(x=DDT, y=Mgacu, color= Nivel))+
  geom_line()+
  labs(
    title="B.",
    x="DDT",
    y="Magnesio (g)",
    color= "Variedad")+
  theme(legend.position = "bottom")

Mg3 <-ggplot(data=Nensayo2[Nensayo1$Factor=="Abono",], mapping=aes(x=DDT, y=Mgacu, color= Nivel))+
  geom_line()+
  labs(
    title="C.",
    x="DDT",
    y="Magnesio (g)",
    color= expression("Abono (g "~m^-2~")"))+
  theme(legend.position = "bottom")
Mg4 <-ggplot(data=Nensayo2[Nensayo2$Factor=="variedad",], mapping=aes(x=DDT, y=Mgacu, color= Nivel))+
  geom_line()+
  labs(
    title="D.",
    x="DDT",
    y="Magnesio (g)",
    color= "Variedad")+
  theme(legend.position = "bottom")

Mg1 + Mg2 + Mg3 + Mg4

