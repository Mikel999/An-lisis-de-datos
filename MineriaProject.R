library(caret)
library(mlbench)
library(ggplot2)
library(reshape2)
library(CatEncoders)
library(tidyverse)
library(DDoutlier)
library(caTools)
library(randomForest)

#######################################################################
######################### Carga de datos ##############################
#######################################################################

data<-read.csv("stars.csv", stringsAsFactors=FALSE)
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
for (i in 1:4){
  print(ggplot(data = data) + 
          geom_point(mapping = aes(x = data[,5], y = data[,i], color = as.character(Star.type))) +
          xlab("Star type") + ylab(colnames(data)[i]) + 
          labs(color="Star type") + 
          ggtitle(paste("Relacion entre ",colnames(data)[i],"y  Star.type (clase)"))+
          theme(plot.title = element_text(hjust = 0.5)))
}
#Plotear la variable de color con respecto a la clase
print(ggplot(data = data) + 
        geom_point(mapping = aes(x = data[,5], y = data[,6], color = as.character(Star.type))) +
        xlab(colnames(data)[5]) + ylab(colnames(data)[6]) + 
        labs(color="Star type") + 
        ggtitle(paste("Relacion entre ",colnames(data)[6],"y ",colnames(data)[5]))+
        theme(plot.title = element_text(hjust = 0.5)))

#Plotear las dos variables nominales
ggplot(data = data) + 
        geom_bar(mapping = aes(y = data[,6], fill = Star.color)) +
        ylab(colnames(data)[6]) + 
        labs(color="Star type") + 
        ggtitle(paste("Distribucion de colores en las estrellas"))+
        theme(plot.title = element_text(hjust = 0.5))+
        coord_polar("y", start=0)
ggplot(data = data) + 
  geom_bar(mapping = aes(y = data[,7], fill = Spectral.Class)) +
  ylab(colnames(data)[7]) + 
  labs(color="Masa Espectral") + 
  ggtitle(paste("Distribucion de las masas espectrales"))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_polar("y", start=0)

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
################### One class Outlier Detection #######################
#######################################################################
data[,5]<-starNums
#One class outlier detection para los 6 tipos de estrellas
for (i in 0:5){
  Stars<-data[data$Star.type==i,]
  #Vector de strings vacios para poner a los valores normales
  #y que no salgan en el grafico los indices
  fake<-rep("", dim(Stars)[1])
  #Pasar como parametros las columnas numericas, no la de la clase
  Stars$outlierness<-LOF(Stars[,1:4], k=3)
  #sort(outlierness)
  #print(which(Stars$outlierness>2))
  #Crear columna nueva que dice si es o no un outlier dependiendo del
  #valor de outlierness obtenido
  Stars$outlier <-(Stars$outlierness>2)
  Stars$OutlierColor <- ifelse(Stars$outlier, "red", "black")
  hist(Stars$outlierness, main=paste("Histograma de outlierness del tipo de estrella ",i))
  #Scater plot de cada tipo de estrella y su outlierness
  print(ggplot(data = Stars) + 
          geom_point(mapping = aes(x = Stars[,8], y = Stars[,5], color = outlier, size=2)) +
          xlab("Outlierness") + ylab("Star type") + 
          labs(title=paste("Outlierness del tipo de estrella ",tipos[i]), color="Outlier") + 
          theme(plot.title = element_text(hjust = 0.5))
          #Poner texto a los que sean outliers solo
          + geom_text(aes(x = Stars[,8], y = Stars[,5],
                          label=ifelse(Stars[,9]==TRUE ,row.names(Stars), fake), 
                          color=outlier),hjust=0, vjust=-1))
  
  StarsVar <- paste("Star", i, sep = "")
  assign(StarsVar, Stars)
}
Stars<-rbind(Star0, Star1, Star2, Star3, Star4, Star5)
fake<-rep("", dim(data)[1])
#Scater plot con los 5 tipos de estrella y su outlierness
print(ggplot(data = Stars) + 
        geom_point(mapping = aes(x = Stars[,5], y = Stars[,8], color = outlier)) +
        xlab("Star Type") + ylab("Outlierness") + 
        labs(color="Outlier") + ggtitle("Outlierness de cada tipo de estrella")+
        theme(plot.title = element_text(hjust = 0.5)) +
        #Poner texto a los que sean outliers solo
        geom_text(aes(x = Stars[,5], y = Stars[,8],
                       label= ifelse(Stars[,9]==TRUE ,row.names(Stars), fake), 
                       color=outlier),hjust=0, vjust=0))


#######################################################################
##################### Principal Component Analysis ####################
#######################################################################

#Particion de la base de datos en train y test con caTools
set.seed(999) 
sample = sample.split(data$Temperature..K., SplitRatio = .80)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)

#PCA
pcaT<-prcomp(train[,1:4], scale. = TRUE)
#Desviacion estandar de las componentes
pcaT$sdev

#Plot de las dos componentes mas importantes y de las relaciones
#con las variables
biplot(pcaT, main="Biplot de PC1 y PC2")

pcaTrain<-as.data.frame(pcaT$x)
testPrediction<-as.data.frame(predict(pcaT, test[,1:4]))

#Plot de la base de datos de train y las predichas
#de la parte de test. Los puntos son de train, y las estrellas de test.
ggplot() + 
  geom_point(data=train, mapping=aes(x=pcaTrain[, 1], y=pcaTrain[, 2]), color=as.character(train$Star.type), shape=19) + 
  xlab(colnames(pcaTrain)[1]) + ylab(colnames(pcaTrain)[2]) + 
  labs(color="Star type") + 
  geom_text(aes(x=pcaTrain[, 1], y=pcaTrain[, 2],label=row.names(train), color=as.character(train$Star.type)),hjust=0, vjust=0)+
  ggtitle(paste("Componentes ",colnames(pcaTrain)[1],"y ",colnames(pcaTrain)[2]))+
  theme(plot.title = element_text(hjust = 0.5))+
  #Añadiendo los puntos a predecir
  geom_point(data=testPrediction, mapping=aes(x=testPrediction[, 1], y=testPrediction[, 2]), color=as.character(test$Star.type), shape=11) +
  geom_text(aes(x=testPrediction[, 1], y=testPrediction[, 2],label=row.names(testPrediction), color=as.character(test$Star.type)),hjust=0, vjust=0)



#######################################################################
#################### Clasificacion supervisada ########################
#######################################################################
dataNum[,5]<-starsNombres
set.seed(999) 
sample = sample.split(data$Temperature..K., SplitRatio = .80)
#Utilizar los datos con las variables nominales factorizadas
train = subset(dataNum, sample == TRUE)
test  = subset(dataNum, sample == FALSE)
#train<-subset(train, select=-c(6,7))
#test<-subset(test, select=-c(6,7))
train<-subset(train, select=-6)
test<-subset(test, select=-6)
#Pasar a caracteres las classes a predecir
test[,5]<-as.character(test[,5])
#Comprobar que no es clase numerica 
is.numeric(test[,5])


#?trainControl
trainCont<-trainControl(method="boot", number=20)

#?train
multiLayerPerceptronModelBOOT <- train (Star.type ~ ., data=train,
                                        method="mlp",
                                        trControl=trainCont,
                                        preProc=c("center","scale"),
                                        metric="Accuracy",
                                        tuneLength=10)

multiLayerPerceptronModelBOOT

plot(multiLayerPerceptronModelBOOT)

#?predict

#Predecir la clase, sin probabilidades, la que mayor probabilidad tenga se asigna.
#Como devuelve numeros enteros, pasar a enteros redondeando y factorizar
predictionsMLP <- factor(predict(multiLayerPerceptronModelBOOT,test, type = "raw"))
confusionMatrix(data=predictionsMLP,factor(test$Star.type))
###############################Metodo 2################################

#?trainControl
rfTrain<-trainControl(method="optimism_boot", number=20, 
                        search="random")

#?train
randForest <- train (Star.type ~ ., data=train,
                     method = 'rf',
                     trControl=rfTrain,
                     preProc=c("center","scale"),
                     tuneLength=5)
randForest
predictionsRF <- factor(predict(randForest,test, type = "raw"))
confusionMatrix(data=predictionsRF,factor(test$Star.type))

#######################################################################
############### Comparacion de modelos ################################
#######################################################################

#?resamples
#Los dos modelos tienen que tener el mismo numero de resamples en 
#el parametro number de trainControl
resamps=resamples(list(mlp=multiLayerPerceptronModelBOOT,rf=randForest)) 
summary(resamps) 
#?xyplot
xyplot(resamps) 
diffs<-diff(resamps) 
summary(diffs)

#######################################################################
##################### Feature selection ###############################
#######################################################################
dataNum[,5]<-starNums
?safsControl
ctrl <- safsControl(functions = rfSA,
                    method = "repeatedcv",
                    repeats = 5,
                    improve = 10)
set.seed(998)
randForSA <- safs(x = dataNum[,1:4], 
            y = dataNum[,5],
            iters = 30,
            safsControl = ctrl)

randForSA
#Las 2 mejores variables: abs.magnitude y radius
plot(randForSA) + theme_bw()
