paquetes <- c("ade4", "corrplot", "xlsx", "openxlsx", "readxl")
lapply(paquetes, require, character.only=TRUE)


Datosclima <- read_excel("C:/Users/Lism_/Desktop/Datosclima.xlsx")
View(Datosclima)


Data <- Datosclima   ### Importamos nuestros datos.



str(Data$fecha)   

Data$fecha <- as.factor(Data$fecha)   ### Definimos la fecha como un factor de 27 niveles.

Data$provincia <- as.factor(Data$provincia)

str(Data$fecha)

Data$fecha
fecha<-Data$fecha  
class(fecha)   ### Escogemos fecha como separador de matrices, es decir,
## vamos a hacer 27 matrices de datos, una por cada día, de dimensiones "i x j", donde i son los lugares y j las variables.



Data.num <- Data[,-c(1,2,3,4,9,11,12,15,18,20)]  ### Quitamos las variables no numéricas.

str(Data.num)



Data.within <- withinpca(Data.num,fecha, scann=FALSE)  ##Estandarizamos nuestros datos.
Data.within
Data.within$fac



Data.ktab<-ktab.within(Data.within)  ## Separamos las tablas según la variable "fecha".
Data.ktab


Data.statis<-statis(Data.ktab, scann=FALSE)  
print(Data.statis)

Data.statis$RV         #matriz de correlaciones.
Data.statis$RV.eig     #Valores propios
Data.statis$RV.coo     #Tabla con las puntuaciones
Data.statis$tab.names  #Nombres de cada submatriz
Data.statis$RV.tabw    #Los pesos asignados a cada submatriz

cor.plot <- corrplot(Data.statis$RV)


Data.statis$C.li #a data frame with the row coordinates
Data.statis$C.Co  #a data frame with the column coordinates

##################################################################

Data.varstatis <- t(Data.statis$C.li)
Data.varstatis

Data.varcor <- cor(Data.varstatis)

corrplot(Data.varcor)

Data.statis$C.eig

#################################################################

plot(Data.statis,plabels.boxes.draw=FALSE)


s.corcircle(Data.statis$RV.coo, lab=Data.statis$tab.names, sub="INTERESTRUCTURA")

s.arrow(Data.statis$C.li, sub="VARIABLES EN EL ESPACIO COMPROMISO")
s.label(Data.statis$C.li, sub="VARIABLES EN EL ESPACIO COMPROMISO")

plot(Data.statis$C.Co[,1:2], col=prov,pch=20)

####################################################

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



