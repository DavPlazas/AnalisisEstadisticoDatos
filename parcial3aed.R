
#Parcial 3 - AED
# David A. Oviedo Salamanca


#1).

#cargamos el dataset wine.csv

#luego procedemos a dividir el conjunto de datos para el entrenamiento y el test (utilizando el 70% para entrenar y
# el restante 30% para probar)


set.seed(101) # nombramos una semilla para que la misma muestra sea reproducida en el futuro
# Luego seleccionamos el 70% de los datos de la muestra del total de n filas (También podemos no usarla y usar más 
# bien datos aleatorios).
sample <- sample.int(n = nrow(wine), size = floor(.70*nrow(wine)), replace = F)
train <- wine[sample, ]
test  <- wine[-sample, ]
w = train [,-1] # eliminamos la primera columna a nuestra matriz de entrenamiento

#para realizar nuestro modelo de discriminacion lineal, usamos la funcion lda que se encuentra en el paquete MASS

#usamos nuestro conjunto de entrenamiento

library(MASS)
modelo_lda <- lda(formula = Type ~ Alcohol + Malic + Ash + Alcalinity + Magnesium + Phenols + Flavanoids + Nonflavanoids
                  + Proanthocyanins + Color + Hue + Dilution + Proline,
                  data = train)

modelo_lda  

# al realizar el LDA ya podemos observar ahora el error de clasificacion, para esto utilzamos la funcion predict
# que nos ayudará a crear la matriz de confusion (ed. una tabla con los valores reales y otra con los predictivos)

predicciones <- predict(object= modelo_lda, newdata = w, method="predictive")
predicciones

table(train$Type, predicciones$class, dnn =c("Tipo de vino real","Tipo de vino predicho"))

#como se puede observar, no se han cometido errores de clasificacion, ahora calculamos el error de clasificación el
#cual en este caso debe ser del 0%

train_error <- mean(train$Type != predicciones$class) * 100
paste("Error de clasificación", train_error, "%")

#el cual efectivamente nos da 0%, o lo que es lo mismo, la precisión de clasificación (desempeño) es del 100%

#-------------------------------------------------------------------------------------------------------------------

# 2). LDA Y QDA con datos iris

#cargamos conjunto de datos
library(datasets)
data("iris")
iris

#de la misma manera que en el ejercicio anterior
set.seed(101) # nombramos una semilla para que la misma muestra sea reproducida en el futuro
# Luego seleccionamos el 60% de los datos de la muestra del total de n filas (los cuales nos serviran para el training)
sample2 <- sample.int(n = nrow(iris), size = floor(.60*nrow(iris)), replace = F)
train2 <- iris[sample2, ]
test2  <- iris[-sample2, ]
z = train2 [,-5] # eliminamos la ultima columna a nuestra matriz de entrenamiento

library(MASS)
modelo_lda2 <- lda(formula =Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                  data = train2)

modelo_lda2  

#luego de realizar el modelo lineal, procedemos a calcular la matriz de confusion mediante las funciones anteriormente
#utilizadas y descritas

predicciones2 <- predict(object= modelo_lda2, newdata = z, method="predictive")
predicciones2

#matriz de confusion
table(train2$Species, predicciones2$class, dnn =c("Especie real","Predicción Especie LDA")) 

#ahora calculamos el error 
train2_error <- mean(train2$Species != predicciones2$class) * 100
paste("Error de clasificación", train2_error, "%")

#como se puede evidenciar el error de clasificación usando LDA es del 1.11%

#Ahora, realizaremos el método de QDA, su función también se encuentra en la librería MASS de r
set.seed(123) # nombramos una semilla para que la misma muestra sea reproducida en el futuro
# Luego seleccionamos el 60% de los datos de la muestra del total de n filas (los cuales nos serviran para el training)
sample3 <- sample.int(n = nrow(iris), size = floor(.60*nrow(iris)), replace = F)
train3 <- iris[sample3, ]
test3  <- iris[-sample3, ]
l = train3 [,-5]

library(MASS)
modelo_qda <- qda(formula = Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = train2)
modelo_qda

#ahora calculamos su matriz de confusion mediante el proceso ya descrito usando 'predcit' y luego agrupandolos en 
#una tabla 
predicciones3 <- predict(object = modelo_qda, newdata = l)
predicciones3
table(train3$Species, predicciones3$class,
      dnn = c("Especie real ", "Predicción Especie QDA"))

train3_error <- mean(train3$Species != predicciones3$class) * 100
paste("trainig_error=",train3_error,"%") 

#Cómo se puede evidenciar el error es del 2.22% en este caso
#por lo que podemos concluir que 

