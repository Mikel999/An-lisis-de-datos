library(caret)
library(mlbench)
library(ggplot2)
library(reshape2)
library(CatEncoders)
library(tidyverse)
library(caTools)
library(cowplot)
library(cluster)
library(class)
library(DDoutlier)
#######################################################################
######################### Carga de datos ##############################
#######################################################################

data<-read.csv("stars.csv", stringsAsFactors=FALSE)
#Estadisticos de los datos por columna
summary(data)

#6 tipos diferentes de estrellas
unique(data$Star.type)
starNums<-data$Star.type
#Lista de nombres del tipo de estrella
tipos<-c("Brown Dwarf", "Red Dwarf", "White Dwarf", "Main Sequence" , "SuperGiants", "HyperGiants")

#Cambiar todos los valores del tipo de estrella a nominal
for(i in 0:length(tipos)){
  #Indices que coinciden con el tipo de estrella i
  index<-which(data[,5]==i)
  #Asignar en esos indices el nombre
  data[index,5]<-tipos[i+1]
}
starsNombres<-data$Star.type
#Visualizar el encabezado de la base de datos
head(data)


#Suma de valores faltantes
missing<-sum(sapply(1:ncol(data),
                    FUN=function(r) {
                      sum(is.na(data[,r]))
                    }))
print(paste("Numero de valores faltantes:", missing))

########################################################################
###################### Plot de variables ###############################
########################################################################

#Plot entre las variables numericas excepto la clase
for (i in 1:3){
  for (j in (i+1):4){
    #Pasar a caracter la columna de clase para cojer 6 colores diferentes
    #en el parametro color
    print(ggplot(data = data) + 
            geom_point(mapping = aes(x = data[,j], y = data[,i], color = as.character(Star.type))) +
            xlab(colnames(data)[j]) + ylab(colnames(data)[i]) + 
            labs(color="Star type") + 
            ggtitle(paste("Variables ",colnames(data)[i],"y ",colnames(data)[j]))+
            theme(plot.title = element_text(hjust = 0.5)))
  }
}

#Plotear todas las variables con respecto a la clase
for (i in 1:7){
  if (i!=5){
    print(ggplot(data = data) + 
            geom_point(mapping = aes(x = data[,5], y = data[,i], color = as.character(Star.type))) +
            xlab("Star type") + ylab(colnames(data)[i]) + 
            labs(color="Star type") + 
            ggtitle(paste("Relacion entre ",colnames(data)[i],"y  Star.type (clase)"))+
            theme(plot.title = element_text(hjust = 0.5)))
  }
}

#Plotear las dos variables nominales
#Colores
ggplot(data = data) + 
        geom_bar(mapping = aes(y = data[,6], fill = Star.color)) +
        ylab(colnames(data)[6]) + 
        labs(color="Star type") + 
        ggtitle(paste("Distribucion de colores en las estrellas"))+
        theme(plot.title = element_text(hjust = 0.5))+
        coord_polar("y", start=0)
#Barplot de los colores
ggplot(data = data) + 
  geom_bar(mapping = aes(y = data[,6], fill = Star.color)) +
  ylab(colnames(data)[6]) + 
  labs(color="Star type") + 
  ggtitle(paste("Distribucion de colores en las estrellas"))+
  theme(plot.title = element_text(hjust = 0.5))

#Masa espectral
ggplot(data = data) + 
  geom_bar(mapping = aes(y = data[,7], fill = Spectral.Class)) +
  ylab(colnames(data)[7]) + 
  labs(color="Masa Espectral") + 
  ggtitle(paste("Distribucion de las masas espectrales"))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_polar("y", start=0)
#Barplot de las masas espectrales
ggplot(data = data) + 
  geom_bar(mapping = aes(y = data[,7], fill = Spectral.Class)) +
  ylab(colnames(data)[7]) + 
  labs(color="Masa Espectral") + 
  ggtitle(paste("Distribucion de las masas espectrales"))+
  theme(plot.title = element_text(hjust = 0.5))

smallData<-data[,1:4]
#Boxplots de todas las variables
df <- melt(smallData, measure.vars=1:ncol(smallData))
ggplot(data=df) + geom_boxplot(mapping = aes(x=variable, y=value, fill=variable))

colors<-c("red", "green", "blue", "purple")
for (i in 1:4){
  print(ggplot(data=smallData) + 
          geom_boxplot(mapping = aes(y=smallData[,i]), fill = colors[i], color = "black") +
          ylab("value") + 
          ggtitle(paste("Boxplot de la variable",colnames(smallData)[i])))
}

#######################################################################
################ Densidad y correlacion de variables ##################
#######################################################################

#Como las ultimas dos columnas son categoricas, pasar a numericas 
#para ver la correlacion
colorsOrder<-sort(unique(data$Star.color))
spectralOrder<-sort(unique(data$Spectral.Class))

#Crear nuevo dataset, para no modificar el original
dataNum<-data
#Pasar las columnas 6 y 7 (color y masa espectral) a numericas
#con el orden de numeros creado arriba (alfabetico)
dataNum$Star.color<-as.numeric(factor(dataNum$Star.color, colorsOrder))
dataNum$Spectral.Class<-as.numeric(factor(dataNum$Spectral.Class, spectralOrder))

#Devolver a numeros para calcular densidades y correlacion entre
#variables
dataNum[,5]<-starNums

#Juntar todos los datos
df <- melt(dataNum, measure.vars=1:ncol(dataNum))
#Plotear las densidades de cada variable
ggplot(df, aes(x=value)) + geom_line(stat="density") + facet_wrap(~variable, scales="free") + labs(x="")
#Densidades complejas, no Gaussianas.

#use="complete.obs" si hay valores faltantes
correlation.matrix <- cor(dataNum, method="kendall")
df2 <- melt(correlation.matrix)
ggplot(df2, aes(x=Var1, y=Var2, fill=abs(value))) + geom_tile() + 
  labs(x="", y="") + theme(axis.text.x=element_text(angle=45, hjust=1))


#######################################################################
##################### Principal Component Analysis ####################
#######################################################################

#Particion de la base de datos en train y test con caTools
set.seed(999) 
sample<-sample.split(dataNum$Temperature..K., SplitRatio = .80)
train<-subset(dataNum, sample == TRUE)
test<-subset(dataNum, sample == FALSE)


apply(data[,1:4], MARGIN = 2, FUN = var)

#Although principal component analysis 
#is typically performed on the covariance matrix S, it often makes 
#more intuitive sense to apply PCA to the correlation matrix. 
#Cases where using R may be preferable to S include data that 
#is measured in different units or has wide variances.

#PCA con matriz de correlaciones (scale = TRUE)
pcaT<-prcomp(train[,1:4], scale. = TRUE)
summary(pcaT)
#Desviacion estandar de las componentes
sdList<-pcaT$sdev
#Hacer al cuadrado para la varianza
sdList<-sdList^2

#Porcentaje de cada componente
porc<-sdList/sum(sdList)*100

#plot(cumsum(porc), type="b", xlab = "Componente Pca", ylab="Porcentaje acumulado")

xpos<-c(1,2,3,4)
porcD<-data.frame(Component=xpos, 
                  Varianza=
                    as.numeric(formatC(porc,digits = 2, format = "f")),
                  CumulativePercentage=
                    as.numeric(formatC(cumsum(porc),digits = 2, format = "f")))

#Grafos de proporcion de varianza y varianza acumulada de las componentes
ggplot(data=porcD,aes(x=porcD[,1],y=porcD[,2], group=1))+
  geom_line()+geom_point()+
  geom_text(aes(x = porcD[,1], 
                y = porcD[,2],
                label=porcD[,2],
                color=as.character(Component)),hjust=0.5, vjust=-1)+
  labs(color="Componente")+ ylim(0,110)+
  xlab("Componente PCA")+ ylab("Proporcion de varianza")+
  ggtitle("Proporcion de varianza")+
  theme(plot.title = element_text(hjust = 0.5))

#Porcentaje acumulado
ggplot(data=porcD,aes(x=porcD[,1],y=porcD[,3], group=1))+
  geom_line()+geom_point()+
  geom_text(aes(x = porcD[,1], 
                y = porcD[,3],
                label=porcD[,3],
                color=as.character(Component)),hjust=0.5, vjust=-1)+
  labs(color="Componente")+ ylim(0,110)+
  xlab("Componente PCA")+ ylab("Porcentaje acumulado de varianza")+
  ggtitle("Porcentaje acumulado de varianza")+
  theme(plot.title = element_text(hjust = 0.5))

#Con el metodo de kaiser, solo 1 variable
#cumple que el % sea mayor que 100/4=25
#en este caso, tambien se podria coger la 2da pca
#sigue siendo facil de dibujar e interpretar
comps<-pcaT$x[,1:2]

cor(train[,1:4], comps)

#Plot de las dos componentes mas importantes y de las relaciones
#con las variables
biplot(pcaT, main="Biplot de PC1 y PC2")

pcaTrain<-as.data.frame(pcaT$x)
testPrediction<-as.data.frame(predict(pcaT, test[,1:4]))

#Plot de la base de datos de train y las predichas
#de la parte de test. Los puntos son de train, y las estrellas de test.
ggplot() + 
  geom_point(data=train, mapping=aes(x=pcaTrain[, 1], y=pcaTrain[, 2]), color=as.character(train$Star.type+1), shape=19) + 
  xlab(colnames(pcaTrain)[1]) + ylab(colnames(pcaTrain)[2]) + 
  labs(color="Star type") + 
  geom_text(aes(x=pcaTrain[, 1], y=pcaTrain[, 2],label=row.names(train), color=as.character(train$Star.type)),hjust=0.5, vjust=-0.5)+
  ggtitle(paste("Componentes ",colnames(pcaTrain)[1],"y ",colnames(pcaTrain)[2]))+
  theme(plot.title = element_text(hjust = 0.5))+
  #Añadiendo los puntos a predecir
  geom_point(data=testPrediction, mapping=aes(x=testPrediction[, 1], y=testPrediction[, 2]), color=as.character(test$Star.type+1), shape=11) +
  geom_text(aes(x=testPrediction[, 1], y=testPrediction[, 2],label=row.names(testPrediction), color=as.character(test$Star.type)),hjust=0.5, vjust=-0.5)

#PC1 relacionado con el tamaño de las estrellas,
#cuanto menor es la estrella (mayor magnitud y menor luminosidad tiene),
#menor el valor de PC1
#Como la luminosidad esta relacionada con la temperatura y el radio
#de las estrellas, mirando el biplot se puede entender que 
#esas tres variables esten mas relacionadas (menor angulo entre ellas)
#Una estrella mas caliente que otra es mas luminosa 
#teniendo estas el mismo radio
#Por otro lado, una estrella mas grande, es mas luminosa teniendo
#estas la misma temperatura
#El PC2 es mas relacionada por lo tanto, con la temperatura 
#primariamente, y algo menos con el radio
#Estrellas como la 234 y 237 tiene una buena relacion entre alta
#temperatura y gran radio, por ello la luminosidad es maxima


#######################################################################
############################# Clustering ##############################
#######################################################################

#K-medoides

dMat<-pcaT$x[,1:2]
#Hay que normalizar los valores, puesto que son metricas diferentes
#y los valores cambian mucho
#dMat<-scale(dataNum[,1:4])
#?dist
#?daisy

#Para el clusterizado de los datos, se estan utilizando las variables
#numericas, por ello, distancia euclidea
dMat<-as.matrix(dist(dMat, method="euclidean"))

#Medida de silhouette para saber cual seria el k o numero de clusters
#optimo
#?pam
#Lista con las medias de silhouette para saber que numero de clusters es mejor.
pamList<-c()
for(k in 2:10){
  #Clusterizar en k clusters y guardar el valor del cluster
  #al que pertenece cada instancia
  clusters<-pam(dMat, k)$clustering
  #Calcular el coeficiente de silhouette de los clusters obtenidos
  sil <- silhouette(clusters, dMat)
  #Guardar la media de todos los coeficientes obtenidos
  pamList<-c(pamList, mean(sil[, 3]))
}
pamList<-data.frame(kClusters=c(2:10),
                    Coeficientes=format(pamList,digits = 2, format = "f"))

ggplot(data=pamList, aes(x=pamList[,1],y=pamList[,2], group=1))+
  geom_point()+
  geom_text(aes(x = pamList[,1], 
                y = pamList[,2],
                label=pamList[,2],
                color=as.character(kClusters)),hjust=0.5, vjust=-0.5)+
  labs(color="Número de clusters")+
  xlab("Número de clusters")+ ylab("Coeficientes de Silhouette")+
  ggtitle("Coeficientes de Silhouette para los diferentes valores de k")+
  theme(plot.title = element_text(hjust = 0.5))


#pamC<-pam(dMat, 3)$clustering

#Cluster de PCA
pamC<-pam(dMat, 6)$clustering

#Unir los indices de cluster a los datos de pca para dibujar los resultados

pamDF<-as.data.frame(cbind(pcaT$x[,1:2],pamC))

clustP<-ggplot() + 
  geom_point(data=pamDF, mapping=aes(x=pamDF[, 1], y=pamDF[, 2]), color=as.character(pamDF[,3]+1), shape=15)+
  xlab(colnames(pcaTrain)[1]) + ylab(colnames(pcaTrain)[2]) + 
  labs(color="Star type") + 
  ggtitle(paste("Clusterizacion (PAM) en componentes PC1 y PC2"))+
  theme(plot.title = element_text(hjust = 0.5))

pcaP<-ggplot() + 
  geom_point(data=train, mapping=aes(x=pcaTrain[, 1], y=pcaTrain[, 2]), color=as.character(train$Star.type+1), shape=19) + 
  xlab(colnames(pcaTrain)[1]) + ylab(colnames(pcaTrain)[2]) + 
  labs(color="Star type") + 
  ggtitle(paste("Componentes ",colnames(pcaTrain)[1],"y ",colnames(pcaTrain)[2]))+
  theme(plot.title = element_text(hjust = 0.5))

plot_grid(pcaP, clustP, labels = "AUTO")

#Tabla de clasificación
table(pamDF[,3]-1, train[,5])

########################################################################
#################### Clusterizacion jerarquica #########################
########################################################################

#?agnes ?hclust
#En este caso, escoger las variables numericas
dMat<-pcaT$x[,1:2]
#dMat<-scale(dataNum[,1:4])
#Metodo ward ?dist
hc<-agnes(dist(dMat), method="ward")
#Dendrograma del metodo ward
#?pltree
pltree(hc, hang=-1, cex=0.6,main="Dendograma",sub = "", xlab="Estrellas")
lines(x=rep(6,200), pch = 18, col = "blue", type = "l", lty = 2)
lines(x=rep(13,200), pch = 18, col = "red", type = "l", lty = 2)
#Coordenadas para los titulos de las lineas
#coords<-locator()
x1<-24.5
y1<-15.97
x2<-24.5
y2<-8.97
?text
text(x1,y1, labels ="Linea de corte 1 (k=3)", cex=0.8)
text(x2,y2, labels ="Linea de corte 2 (k=6)", cex=0.8)
#?cutree
#Se podria cortar por el punto de 3 o 6 clusters
wardC3<-cutree(hc, k=3)
wardC6<-cutree(hc, k=6)
#Guardar la particion de clusteres en dos data frames separados
ward3DF<-as.data.frame(cbind(pcaT$x[,1:2],wardC3))
ward6DF<-as.data.frame(cbind(pcaT$x[,1:2],wardC6))

wardP3<-ggplot() + 
  geom_point(data=ward3DF, mapping=aes(x=ward3DF[, 1], y=ward3DF[, 2]), color=as.character(ward3DF[,3]+1), shape=15)+
  xlab(colnames(pcaTrain)[1]) + ylab(colnames(pcaTrain)[2]) + 
  labs(color="Star type") + 
  ggtitle(paste("Clusterizacion Jerarquica (k=3) en PC1 y PC2"))+
  theme(plot.title = element_text(hjust = 0.5))

wardP6<-ggplot() + 
  geom_point(data=ward6DF, mapping=aes(x=ward6DF[, 1], y=ward6DF[, 2]), color=as.character(ward6DF[,3]+1), shape=15)+
  xlab(colnames(pcaTrain)[1]) + ylab(colnames(pcaTrain)[2]) + 
  labs(color="Star type") + 
  ggtitle(paste("Clusterizacion Jerarquica (k=6) en PC1 y PC2"))+
  theme(plot.title = element_text(hjust = 0.5))


plot_grid(wardP3, wardP6, labels = "AUTO")

table(ward3DF[,3]-1, train[,5])
table(ward6DF[,3]-1, train[,5])

########################################################################
############################# KNN ######################################
########################################################################

#?knn()

testIndex<-as.numeric(row.names(test))
error<-c()
for (i in 2:9){
  knnM<-knn(train[,1:4], test[,1:4], cl=train[,5], k=i)
  (kTable<-table(dataNum[testIndex,5], knnM, dnn = c("Star type","Knn star type")))
  error<-c(error,1-sum(diag(kTable))/sum(kTable))
}

errorD<-data.frame(ValorK=c(2:9), 
                   Error=as.numeric(formatC(error,digits = 2, format = "f")))

#Grafo de errores para diferentes valores de K
ggplot(data=errorD,aes(x=errorD[,1],y=errorD[,2], group=1))+
  geom_line()+geom_point()+
  geom_text(aes(x = errorD[,1], 
                y = errorD[,2],
                label=errorD[,2],
                color=as.character(ValorK)),hjust=0.5, vjust=-1)+
  labs(color="Valor de K")+ ylim(0.38,0.5)+
  xlab("Valor de K")+ ylab("Error del modelo")+
  ggtitle("Error del modelo Knn para diferentes valores de K")+
  theme(plot.title = element_text(hjust = 0.5))

#K=7 el error minimo
k<-errorD[which(errorD[,2]==min(errorD[,2])),1]
#Volver a crear el modelo con k=7
knnM<-knn(train[,1:4], test[,1:4], cl=train[,5], k=k)
(kTable<-table(dataNum[testIndex,5], knnM, dnn = c("Star type","Knn star type")))

accuracy<-sum(diag(kTable))/sum(kTable)*100
toString(accuracy+"%")
print(paste("El porcentaje de bien clasificados es: ",accuracy,"%",sep=""))

ggplot(data = dataNum[testIndex,]) + 
        geom_point(mapping = aes(x = dataNum[testIndex,4], y = dataNum[testIndex,1], color = as.character(Star.type))) +
        xlab(colnames(dataNum)[1]) + ylab(colnames(dataNum)[4]) + 
        labs(color="Star type") + 
        ggtitle(paste("Variables ",colnames(dataNum)[1],"y ",colnames(dataNum)[4]))+
        theme(plot.title = element_text(hjust = 0.5))+
        geom_point(mapping = aes(x = dataNum[testIndex,4], y = dataNum[testIndex,1], color = as.character(knnM), shape=15, alpha=0.8))+
        scale_shape_identity()

clases<-c(as.character(knnM),as.character(test[,5]))
knnC<-rep("Knn Predicted", length(knnM))
realC<-rep("Real class", length(knnM))
typeC<-c(knnC,realC)
position<-c(seq(1,48), seq(1,48))
alphaC<-ifelse(knnM==test[,5],1,0.8)
predicted<-data.frame(Clases=clases, Tipo=typeC, Position=position,
                      Alpha=c(alphaC, alphaC))  

ggplot(data = predicted) + 
    geom_point(mapping = aes(x = Position, 
                             y = Tipo, 
                             color = as.character(Clases),
                             alpha = Alpha)) +
    xlab("Test classes") + ylab("Type") + 
    labs(color="Star type") + 
    ggtitle("Comparación de las clases reales y las predichas por Knn")+
    theme(plot.title = element_text(hjust = 0.5))

#Resaltados los aciertos con un valor de alpha de 1 y los 
#valores que han fallado, con un valor alpha de 0.8

########################################################################
############################K-NN para las variables de pca##############
########################################################################

errorpca<-c()
for (i in 2:9){
  knnM<-knn(pcaTrain[,1:2], testPrediction[,1:2], cl=train[,5], k=i)
  (kTable<-table(dataNum[testIndex,5], knnM, dnn = c("Star type","Knn star type")))
  errorpca<-c(errorpca,1-sum(diag(kTable))/sum(kTable))
}


errorP<-data.frame(ValorK=c(2:9), 
                   Error=as.numeric(formatC(errorpca
                                            ,digits = 2, format = "f")))

#Grafo de errores para diferentes valores de K
ggplot(data=errorP,aes(x=errorP[,1],y=errorP[,2], group=1))+
  geom_line()+geom_point()+
  geom_text(aes(x = errorP[,1], 
                y = errorP[,2],
                label=errorP[,2],
                color=as.character(ValorK)),hjust=0.5, vjust=-1)+
  labs(color="Valor de K")+ ylim(0.06,0.11)+
  xlab("Valor de K")+ ylab("Error del modelo")+
  ggtitle("Error del modelo Knn para diferentes valores de K")+
  theme(plot.title = element_text(hjust = 0.5))

#K=2,5,6,7 el error minimo
#utilizar mismo k=7 para comparar con el de variables normales
k<-7
#Volver a crear el modelo con k=7
knnM<-knn(pcaTrain[,1:2], testPrediction[,1:2], cl=train[,5], k=k)
(kTable<-table(dataNum[testIndex,5], knnM, dnn = c("Star type","Knn star type")))

#Calcular porcentaje de aciertos
accuracy<-sum(diag(kTable))/sum(kTable)*100
print(paste("El porcentaje de bien clasificados es: ",accuracy,"%",sep=""))

clases<-c(as.character(knnM),as.character(test[,5]))
knnC<-rep("Knn Predicted", length(knnM))
realC<-rep("Real class", length(knnM))
typeC<-c(knnC,realC)
position<-c(seq(1,48), seq(1,48))
alphaC<-ifelse(knnM==test[,5],1,0.8)
predicted<-data.frame(Clases=clases, Tipo=typeC, Position=position,
                      Alpha=c(alphaC, alphaC))  

ggplot(data = predicted) + 
  geom_point(mapping = aes(x = Position, 
                           y = Tipo, 
                           color = as.character(Clases),
                           alpha = Alpha)) +
  xlab("Test classes") + ylab("Type") + 
  labs(color="Star type") + 
  ggtitle("Comparación de las clases reales y las predichas por Knn (PCA)")+
  theme(plot.title = element_text(hjust = 0.5))


#######################################################################
########################## Outlier Detection ##########################
#######################################################################
Stars<-dataNum
fake<-rep("", dim(data)[1])
#Pasar como parametros las columnas numericas, no la de la clase
#?LOF
Stars$outlierness<-LOF(dataNum[,1:4], k=5)
#sort(Stars$outlierness)
#print(which(Stars$outlierness>2))
#Crear columna nueva que dice si es o no un outlier dependiendo del
#valor de outlierness obtenido
Stars$outlier <-(Stars$outlierness>1.75)
Stars$OutlierColor <- ifelse(Stars$outlier, "red", "black")
hist(Stars$outlierness, main=paste("Histograma de outlierness de las estrellas"))
#Scater plot de cada tipo de estrella y su outlierness
ggplot(data = Stars) + 
        geom_point(mapping = aes(x = Stars[,8], y = Stars[,5], color = outlier, size=1)) +
        xlab("Outlierness") + ylab("Star type") + 
        labs(title=paste("Outlierness de los diferentes tipos de estrellas"), color="Outlier") + 
        theme(plot.title = element_text(hjust = 0.5))+
        #Poner texto a los que sean outliers solo
        geom_text(aes(x = Stars[,8], y = Stars[,5],
                      label=ifelse(Stars[,9]==TRUE ,row.names(Stars), fake), 
                      color=outlier),hjust=0, vjust=-1)


ggplot(data = Stars) + 
  geom_point(mapping = aes(x = Stars[,4], y = Stars[,1], color = outlier)) +
  xlab("Outlierness") + ylab("Star type") + 
  labs(title=paste("Outlierness en",colnames(Stars)[1],"y",colnames(Stars)[1]), color="Outlier") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  #Poner texto a los que sean outliers solo
  geom_text(aes(x = Stars[,4], y = Stars[,1],
                label=ifelse(Stars[,9]==TRUE ,row.names(Stars), fake), 
                color=outlier),hjust=0, vjust=-1)

