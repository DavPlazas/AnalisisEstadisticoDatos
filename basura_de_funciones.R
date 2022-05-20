#------------------------Ejercicio 9.10-------------------------------------------#

vector_1 <- c(1,0.505,0.569,0.602,0.621,0.603)
vector_2 <- c(0.505,1,0.422,0.467,0.482,0.450)
vector_3 <- c(0.569,0.422,1,0.926,0.877,0.878)
vector_4 <- c(0.602,0.467,0.926,1,0.874,0.894)
vector_5 <- c(0.621,0.482,0.877,0.874,1,0.937)
vector_6 <- c(0.603,0.450,0.878,0.894,0.937,1)
matriz <- rbind(vector_1, vector_2, vector_3, vector_4, vector_5, vector_6)
matriz

vector_1_L <- c(0.602,0.200)
vector_2_L <- c(0.467,0.154)
vector_3_L <- c(0.926,0.143)
vector_4_L <- c(1.000,0.000)
vector_5_L <- c(0.874,0.476)
vector_6_L <- c(0.894,0.327)
matriz_L <- rbind(vector_1_L, vector_2_L, vector_3_L, vector_4_L, vector_5_L, vector_6_L)
matriz_L

a = matriz_L%*%t(matriz_L)
a

phi = matriz - a
phi

#Las varianzas están en la diagonal principal de phi


#Las comunalidades están en la diagonal de a

#Suma de cuadrados de la primera columna de L sobre 6 (numero de var) y lo mismo para la segunda = Proporcion


Matriz_residual = matriz - a -phi
Matriz_residual

#https://towardsdatascience.com/exploratory-factor-analysis-in-r-e31b0015f224

#------------------------Ejemplo Fact Análisis y PCA-------------------------------------------#

load("abulon.RData")
abulon = abulon[,-9]

p = prcomp(abulon,scale = TRUE)
p

summary(p)

#En las 5 primeras componentes ya tenemos un acumulado del 98%.


#Las 5 primeras componentes explican todo y van variando menos al meter más componentes

install.packages("psych")
library(psych)


fit = fa(abulon,3,rotate="none", fa = "mle", covar = FALSE)
fit

#------------------------Ejemplo Var canonicas y corr canónica-------------------------------------------#


load("C:/Users/ASUS X512F/Downloads/plantulas.RData")
plantulas = dat

plantulas=plantulas[,-3]
plantulas

a = plantulas[,1:2]
a

b = plantulas[,c(3,4)]
b

can = cancor(a,b,xcenter = TRUE, ycenter = TRUE)
cancor


#------------------------Ejemplo CLASIFICACIÓN-------------------------------------------#

library(MASS)


penguins
a = lda(species ~ .,data = penguins)
a
plot(a)

b = lda(Libro1$pob ~ .,data = Libro1)
b
plot(b)

table(predict(b)$clas,Libro1$pob)

mean(Libro1[Libro1$pob=="a",]$var1)
Libro1[Libro1$pob=="b",]

install.packages("effectsize")
library(effectsize)

sd_pooled(Libro1[Libro1$pob=="a",], Libro1[Libro1$pob=="b",])

poolCov <- covW(Libro1[,1:2],Libro1[,3])
