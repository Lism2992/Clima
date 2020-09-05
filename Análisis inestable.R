paquetes <- c("ade4", "corrplot", "xlsx", "openxlsx", "readxl")
lapply(paquetes, require, character.only=TRUE)




library(readxl)

Cfbinvierno <- read_excel("CfbEstaciones/Cfb inestable otonho/Cfb inestable Otonho.xlsx")
View(Csa_inestable_otonho)

Cfbinvierno <- read_excel("C:/Users/Lism_/Desktop/AnaSTATIS/CfbEstaciones/Cfb inestable verano/Cfb inestable verano 2.xlsx")


Csa_todo <- read_excel("C:/Users/Lism_/Desktop/Compromiso inestable/Todo.xlsx")
View(Csa_todo)


Data <- Cfbinvierno

Data <- Csa_todo



str(Data)


Data$fecha <- as.factor(Data$fecha) ### Definimos la fecha como un factor de 27 niveles.
Data$Clima <- as.factor(Data$Clima)

str(Data$fecha)

clima <- Data$Clima

fecha<-Data$fecha  
class(fecha)   

Data.num <- Data[,5:14]
Data.num <- Data[,-c(1,2,3,4,9,11,12,15,18,20)] 
Data.num <- Data[,-11]
Data.num <- Data[,-c(1,12)]

str(Data.num)

Data.within <- withinpca(Data.num,clima, scann=FALSE)  ##Estandarizamos nuestros datos.
Data.ktab<-ktab.within(Data.within)
Data.statis<-statis(Data.ktab, scann=FALSE)



Data.within <- withinpca(Data.num,fecha, scann=FALSE)  ##Estandarizamos nuestros datos.
Data.ktab<-ktab.within(Data.within)
Data.statis<-statis(Data.ktab, scann=FALSE)

cor.plot <- corrplot(Data.statis$RV)

cor.plot2 <- corrplot(Data.statis$RV, order = c("hclust"))

s.corcircle(Data.statis$RV.coo, lab=Data.statis$tab.names, sub="INTERESTRUCTURA")

s.arrow(Data.statis$C.li, sub="VARIABLES EN EL ESPACIO COMPROMISO")

s.arrow(Data.statis$C.li[,2:3], sub="VARIABLES EN EL ESPACIO COMPROMISO")


##################################################33

library(writexl)


tcoord <- t(Data.statis$C.li)

tcoord.std <- scale(tcoord)

tcoord.cov <- cov(tcoord.std)

corrplot(tcoord.cov)

write_xlsx(Data.ktab$`2019-09-13` ,"C:\\Users\\Lism_\\Desktop\\Compromisos\\Csaprimavera.xlsx")

#################################################

str(tcoord)

tcoord.dataframe <- as.data.frame(tcoord)

write_xlsx(tcoord.dataframe ,"C:\\Users\\Lism_\\Desktop\\Compromiso inestable\\Cfbverano 2.xlsx")


