## Importamos librerías.

paquetes <- c("ade4", "corrplot", "xlsx", "openxlsx", "readxl")
lapply(paquetes, require, character.only=TRUE)

## Importamos datos.

library(writexl)



library(readxl)

Estepa_ano_todo_perfecto_y_revisado <- read_excel("C:/Users/Lism_/Desktop/TFM Data/Cfb, ano todo perfecto y revisado.xlsx")
View(Estepa_ano_todo_perfecto_y_revisado)

Data <- Estepa_ano_todo_perfecto_y_revisado

str(Data)

str(Data$fecha)   

## Convertimos la fecha en un factor de 365 niveles.

Data$fecha <- as.factor(Data$fecha)   ### Definimos la fecha como un factor de 27 niveles.

Data$provincia <- as.factor(Data$provincia)

Data$nombre <- as.factor(Data$nombre)

str(Data$nombre)
str(Data$fecha)
str(Data$provincia)

## Vamos a escoger la fecha como separador de k-tablas.

fecha<-Data$fecha  
class(fecha)   

## Eliminamos las columnas no numéricas.

Data
str(Data)
Data.num <- Data[,5:14]
Data.num <- Data[,-c(1,2,3,4,9,11,12,15,18,20)] 

str(Data.num)

## Análisis descriptivo y boxplot.

Data.summary <- summary(Data.num)
Data.summary

str(Data.summary)

Data.summary <- as.data.frame(Data.summary)

str(Data.summary)

write_xlsx(Data.summary,"C:\\Users\\Lism_\\Desktop\\TFM Data\\Cfb.xlsx")

boxplot (Data.num$altitud, data=Data.num, xlab = "altitud", ylab = "msnm")
boxplot (Data.num$tmed, data=Data.num, xlab = "Temperatura media", ylab = "ºC")
boxplot (Data.num$prec, data=Data.num, xlab = "Precipitaciones", ylab = "mm")
boxplot (Data.num$tmin, data=Data.num, xlab = "Temperatura mínima", ylab = "ºC")
boxplot (Data.num$tmax, data=Data.num, xlab = "Temperatura máxima", ylab = "ºC")
boxplot (Data.num$velmedia, data=Data.num, xlab = "Velocidad media del viento", ylab = "m/s")
boxplot (Data.num$racha, data=Data.num, xlab = "Racha máxima de viento", ylab = "m/s")
boxplot (Data.num$sol, data=Data.num, xlab = "Horas de insolación", ylab = "h")
boxplot (Data.num$presmax, data=Data.num, xlab = "Presión máxima", ylab = "hPa")
boxplot (Data.num$presmin, data=Data.num, xlab = "Presión mínima", ylab = "hPa")

## Estandarizamos nuestros datos y separamos las k-tabs según la variable fecha.

Data.within <- withinpca(Data.num,fecha, scann=FALSE)  ##Estandarizamos nuestros datos.
Data.within
Data.within$fac


Data.ktab<-ktab.within(Data.within)
Data.ktab


## Realizamos el análisis STATIS


Data.statis<-statis(Data.ktab, scann=FALSE)  
print(Data.statis)

Data.statis$C.eig

Data.statis$RV         #matriz de correlaciones entre las diferentes k-tablas (fechas).
Data.statis$RV.eig     #Valores propios
Data.statis$RV.coo     #Tabla con las puntuaciones, posiciones de cada día en el espacio reducido, la componente 1 de todos es positiva.
Data.statis$tab.names  #Nombres de cada submatriz
Data.statis$RV.tabw    #Los pesos asignados a cada submatriz

cor.plot <- corrplot(Data.statis$RV)

cor.plot2 <- corrplot(Data.statis$RV, order = c("hclust"))


Data.statis$C.li # Coordenadas de las variables según la matriz covarianza de la consenso.

Data.statis$C.Co  #a data frame with the column coordinates

Data.statis$tab.names



##################################################################



#################################################################

plot(Data.statis,plabels.boxes.draw=FALSE)

plot(Data.statis$C.li[,1:2], labels(variables))


s.corcircle(Data.statis$RV.coo, lab=Data.statis$tab.names, sub="INTERESTRUCTURA")

s.arrow(Data.statis$C.li, sub="VARIABLES EN EL ESPACIO COMPROMISO")
### s.label(Data.statis$C.li, sub="VARIABLES EN EL ESPACIO COMPROMISO") prefiero la que tiene las flechas.

### plot(Data.statis$C.Co[,1:2], col=provincia,pch=20) No debería estar haciendo nada.

############################## No tengo ni puta idea de ésta sección. ##########################

kplot(Data.statis, traj = TRUE, arrow = FALSE,plabels.boxes.draw=FALSE)
kplot(Data.statis, traj = T, arrow = FALSE)

if(adegraphicsLoaded()) {
  s.arrow(Data.statis$C.li, pgrid.text.cex = 0)
  kplot(Data.statis, traj = TRUE, arrow = FALSE, plab.cex = 0, psub.cex = 3, ppoi.cex = 3)
} else {
  s.arrow(Data.statis$C.li, cgrid = 0)
  kplot(Data.statis, traj = TRUE, arrow = FALSE, unique = TRUE, 
        clab = 0, csub = 3, cpoi = 3)
}

####################################################

if(adegraphicsLoaded()) {
  if(requireNamespace("sp", quietly = TRUE)) {
    g11 <- s.label(jv73$xy, Sp = jv73$Spatial, pori.incl = FALSE, plab.cex = 0.75, plot = FALSE)
    g12 <- s.class(jv73$xy, jv73$fac.riv, ellipseSize = 0, pellipses.axes.draw = FALSE, 
                   starSize = 0, ppoints.cex = 0, plab.cex = 1.25, plot = FALSE)
    g1 <- superpose(g11, g12, plot = TRUE)
    
    g2 <- kplot(statis(w), perm = TRUE, row.plab.cex = 0, posieig = "none")
  }
  
} else {
  s.label(jv73$xy, contour = jv73$contour, incl = FALSE, clab = 0.75)
  s.class(jv73$xy, jv73$fac.riv, add.p = TRUE, cell = 0, axese = FALSE, csta = 0, 
          cpoi = 0, clab = 1.25)
  
  kplot(statis(w), perm = TRUE, clab.r = 0, clab.c = 2, show = FALSE)
}





####### Comparación PCA del consenso vs PCA de los días 4 y 11.#########################

par(mfrow=c(1,2))

s.arrow(Data.statis$C.li, sub="VARIABLES EN EL ESPACIO COMPROMISO")

Data.0420 <- (Data.ktab$`2020-02-04`)
Data.0420
Data.0420pc <- prcomp(Data.0420)
Data.0420pc

summary(Data.0420pc)

plot(Data.0420pc$x)

Data.0420pct <- t(Data.0420pc$x)

Data.0420pct

Data.0420pctcor <- cor(Data.0420pct)

corrplot(Data.0420pctcor)

################################

Data.1120 <- t(Data.ktab$`2020-02-11`)
Data.1120
Data.1120pc <- prcomp(Data.1120)


######################### Correlaciones ######################

par(mfrow=c(1,2))

Data.cor4 <- cor(t(Data.ktab$`2020-02-04`))

corrplot(Data.cor4)

Data.cor18 <- cor(t(Data.ktab$`2020-02-18`))

corrplot(Data.cor18)

Data.cor11 <- cor(t(Data.ktab$`2020-02-11`))

corrplot(Data.cor11)

corrplot(Data.varcor)

########## Reescribir tablas de inestabilidades #############

write_xlsx(Data.ktab$`2019-09-13` ,"C:\\Users\\Lism_\\Desktop\\AnaSTATIS\\Estepainest14.xlsx")

Data.ktab$`2019-12-22`

Data.ktab$`2019-12-28`

########################################################3

coordt <- t(Data.statis$C.li)

coordt.scale <- scale(coordt)
coordt
coordt.scale

coord.cov <- cov(coordt.scale)

corrplot(coord.cov)
