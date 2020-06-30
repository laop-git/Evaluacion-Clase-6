rm(list = ls())
setwd("C:/Users/USUARIO/Documents/Cursos Profesionales/Especialización en R para Data Science/Clase 6_R4DS/Evaluación Clase 6")
dir()

library(datasets)
library(ggplot2)
library(help="datasets")
data("iris")
#Dado que es un dataset, iris es un objeto de R por lo que puedo observar su documentación
datos <- iris
help("iris")
#Confirmando que es un dataframe
class(datos)
#####PREGUNTA 1:
### Análisis descriptivo
plot(datos)
#Este dataframe brinda las magnitudes en centímetros de las variables sepal lenght/width y petal lenght/width, para 50 flores de cada 3 especies de iris: Iris Setosa, versicolor y virginica 
#Se observa que las variables (columnas) están respectivamente denominadas
#Limpiando dataframe
datos <- na.omit(datos)
#Con str se determina que las 4 variables "X" son numéricas:sepal.lenght, sepal.width, petal.length, petal.width; y 1 cualitativa: Especie
str(datos)
#Para obtener la matriz de correlaciones entre las X que son numéricas, se quita la "y" que está en columna 5 Species
cor(datos[,-5])
library(corrplot)
corrplot(cor(datos[,-5]))
#Diagrama de pares de variables independientes X
pairs(datos[,-5])

#Estadísticos
head(datos)
tail(datos)
summary(datos)

datos[is.na(datos$Species) , ]

######Por especie:
colnames(datos)
datos$Sepal.Length

# Histograma #
hist(datos$Sepal.Length)
h_Sepal.Length <- hist(datos$Sepal.Length)
h_Sepal.Length$breaks
h_Sepal.Length$counts
range(datos$Sepal.Length)

# boxplot #

boxplot(datos$Sepal.Length)
bp_Sepal.Length <- boxplot(datos$Sepal.Length)

#Diagrama de dispersión #
#Los valores categóricos de Y toman valores de 1, 2 y 3
plot.ts(datos$Sepal.Length, datos$Species)


datos$Sepal.Width

# Histograma #
hist(datos$Sepal.Width)
h_Sepal.Width <- hist(datos$Sepal.Width)
h_Sepal.Width$breaks
h_Sepal.Width$counts
range(datos$Sepal.Width)

# boxplot #

boxplot(datos$Sepal.Width)
bp_Sepal.Width <- boxplot(datos$Sepal.Width)

#Diagrama de dispersión #

plot.ts(datos$Sepal.Width, datos$Species)

datos$Petal.Length

# Histograma #
hist(datos$Petal.Length)
h_Petal.Length <- hist(datos$Petal.Length)
h_Petal.Length$breaks
h_Petal.Length$counts
range(datos$Petal.Length)

# boxplot #

boxplot(datos$Petal.Length)
bp_Petal.Length <- boxplot(datos$Petal.Length)

#Diagrama de dispersión #
plot.ts(datos$Petal.Length, datos$Species)


datos$Petal.Width

# Histograma #
hist(datos$Petal.Width)
h_Petal.Width <- hist(datos$Petal.Width)
h_Petal.Width$breaks
h_Petal.Width$counts
range(datos$Petal.Width)

# boxplot #

boxplot(datos$Petal.Width)
bp_Petal.Width <- boxplot(datos$Petal.Width)

#Diagrama de dispersión #
plot.ts(datos$Petal.Width, datos$Species)

#En conjunto

datasepall<-select(iris,"Sepal.Length","Sepal.Width","Species")
ggplot(datasepall,aes(x=Sepal.Length,y=Sepal.Width,colour=Species))+geom_point()+ggtitle("Sepal Length vs Sepal Width")
boxplot(select(datasepall,"Sepal.Length","Sepal.Width"))



###########PREGUNTA 2################

library(rpart)
library(help="rpart")
#Construcción del árbol
mod1 <- rpart(Species ~ .,data=datos)
#Con class se observa que es una variable de clase rpart
class(mod1)
typeof(mod1)
#Dado que es una lista puedo acceder a sus campos de una manera similar como los dataframe
mod1$splits
mod1
#Con mod1, se tiene 150 puntos, donde el particionado se ha logrado utilizando las variables Petal.Lenght y Petal.Width
#Visualizando el árbol mod1 mediante prp que significa plot rpart
library(rpart.plot)
prp(mod1)
#Se procede a particioanr el dataframe 
datos_Level1<- datos[datos$Petal.Length >= 2.5,]
#Con esta primera partición, se obtienen 50 observaciones, luego particionamos sobe datos_level1
datos_level2<- datos_Level1[datos_Level1$Petal.Width < 1.8,]
#Al generar datos_level2, se nota que la cantidad de observaciones resultan ser 54
#Para corroborar la construcción del árbol, se ha utilizado la moda en datos_level2
install.packages("modeest")
library(modeest)
#Obteniendo la moda: 
mfv(datos_level2$Species)
######Predicción Dataframe Nueva Especie
colnames(datos)

NuevaEspecie <- data.frame(Sepal.Length = 6.5, Sepal.Width = 3.0, Petal.Length = 5.2, Petal.Width = 2.0)
class(datos)
install.packages("tree")
library("tree")
set.seed(1)
train <- sample(1:nrow(datos), size = nrow(datos)/2)
#Predicción:
arbol_regresion <- tree(formula = datos$Species ~ ., data = datos, subset = train, split = "deviance")
predict(object=mod1,newdata=NuevaEspecie)
dev.off()

########## Pregunta 3 #########


library(mlbench)
data(PimaIndiansDiabetes2)
datos1<-PimaIndiansDiabetes2[,-7]
datos1.1<-PimaIndiansDiabetes2[,-2]
mod2 <- rpart(diabetes ~ ., data=datos1, method='class')

data1<-data.frame(pregnant=7,glucose=90,pressure=70,triceps=23,insulin=92,age=30,mass=25)

predict(mod2,newdata = data1)

mod2.1 <- rpart(diabetes ~ ., data=datos1.1, method='class')
data2<-data.frame(pregnant=7,pedigree=0.6,pressure=70,triceps=23,insulin=92,age=30,mass=25)
predict(mod2.1,newdata = data2)
dev.off()

########## Pregunta 4 ##########
install.packages("MASS")
library(MASS)
library(primes)
data("Boston")
View(Boston)

class(Boston)
colnames(Boston)
## para los dos escenarios 
Prueba1 <-c(1 ,3, 9,12, 14,15,  32,  36,  45 , 59 , 66  ,94 , 95 ,130 ,146, 149 ,171, 188 ,
            193, 194, 209, 210 ,218 ,227, 237 ,241, 255, 277,304 ,308 ,316 ,320, 334 ,349,
            366 ,367 ,371 ,378, 393 ,401, 406 ,422, 423 ,453 ,455 ,485, 496, 505)
Prueba2 <- sample(primes::generate_primes(min=200,max = 506),size = 25)
Escenario1 <- Boston[Prueba1,-14] # dataframe de prueba 1
colnames(Escenario1)
Escenario2 <- Boston[Prueba2,-14] # dataframe de prueba 2
colnames(Escenario2)

datos3 <- Boston
datos3 <- na.omit(datos3)
####
mod3 <- rpart(Escenario1$ptratio ~ ., data = Escenario1)
plot(mod3)
text(mod3, use.n=TRUE)
mod3.pred<-predict(mod3,Escenario1)
hist(mod3.pred)
####
mod4 <- rpart(Escenario2$ptratio ~ ., data = Escenario2)
plot(mod4)
text(mod4, use.n=TRUE)
mod4.pred<-predict(mod4,Escenario2)
hist(mod4.pred)

