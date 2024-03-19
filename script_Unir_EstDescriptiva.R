library(openxlsx)

descPR26Jul$Fecha<-as.Date("2023-7-26", format="%Y-%m-%d")
descPA26Jul$Fecha<-as.Date("2023-7-26", format="%Y-%m-%d")
descPT26Jul$Fecha<-as.Date("2023-7-26", format="%Y-%m-%d")
descAF26Jul$Fecha<-as.Date("2023-7-26", format="%Y-%m-%d")
descRPA26Jul$Fecha<-as.Date("2023-7-26", format="%Y-%m-%d")

descPR26Jul$Variable<-"Peso Raiz"
descPA26Jul$Variable<-"Peso Parte Aerea"
descPT26Jul$Variable<-"Peso Total"
descAF26Jul$Variable<-"Area Foliar"
descRPA26Jul$Variable<-"Relación Raiz Parte Aerea"

desc26Jul<-rbind(descPR26Jul,
  descPA26Jul,
  descPT26Jul,
  descAF26Jul,
  descRPA26Jul
)


write.xlsx(desc26Jul, "Resultados/26Jul.xlsx")
