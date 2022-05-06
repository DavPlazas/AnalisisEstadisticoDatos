#-----------------------------------------------Punto 1------------------------------------------#

install.packages("expm")
library(expm)


vector_1 <- c(100,0,0,0)
vector_2 <- c(0,1,0.95,0)
vector_3 <- c(0,0.95,1,0)
vector_4 <- c(0,0,0,100)

matriz <- rbind(vector_1, vector_2, vector_3, vector_4)
matriz

Sigma_21 = matriz[3:4,1:2]
Sigma_21

Sigma_12 = matriz[1:2,3:4]
Sigma_12

Sigma_11 = matriz[1:2,1:2]
Sigma_11_inv <- solve(Sigma_11)

Sigma_22 = matriz[3:4,3:4]
Sigma_22_inv <- solve(Sigma_22)

Sigma_11_inv_raizcuadrado<- sqrtm(Sigma_11_inv)
Sigma_11_inv_raizcuadrado

Sigma_22_inv_raizcuadrado<- sqrtm(Sigma_22_inv)
Sigma_22_inv_raizcuadrado

Matriz_u <- Sigma_11_inv_raizcuadrado%*%Sigma_12%*%Sigma_22_inv%*%Sigma_21%*%Sigma_11_inv_raizcuadrado
Matriz_u

eigenValuesU <- eigen(Matriz_u)$values
eigenVectorsU <- eigen(Matriz_u)$vectors
eigenValuesU                      
eigenVectorsU 


t(eigenVectorsU[,1])%*%Sigma_11_inv_raizcuadrado


#---------------------------Para f--------------------------------------#

Matriz_v <- Sigma_22_inv_raizcuadrado%*%Sigma_21%*%Sigma_11_inv%*%Sigma_12%*%Sigma_11_inv_raizcuadrado
Matriz_v

eigenValuesV <- eigen(Matriz_v)$values
eigenVectorsV <- eigen(Matriz_v)$vectors
eigenValuesV                      
eigenVectorsV 

t(eigenVectorsV[,1])%*%Sigma_22_inv_raizcuadrado

u1<-eigenVectorsU [,1]%*%Sigma_11_inv_raizcuadrado
v1<-eigenVectorsV[,1]%*%Sigma_22_inv_raizcuadrado

cat("\nVariantes Canonicas 1a: \n")
sprintf("u1 = %f X1(1) + %f X2(1)", u1[1] , u1[2])
sprintf("v1 = %f X3(2) + %f X4(2)", v1[1], v1[2])



#La correlación entre las variables canónicas u1 y v1 es el primer valor propio de la matriz U.

cat("\nCorrelaciones Canónicas 1b: \n")
sprintf("Cor(U1,V1) = %f", sqrt(eigenValuesU[1]))
sprintf("Cor(U2,V2) = %f", sqrt(eigenValuesU[2]))


#---------------------------------Punto 2-------------------------------------------#



vector_1 <- c(8,2,3,1)
vector_2 <- c(2,5,-1,3)
vector_3 <- c(3,-1,6,-2)
vector_4 <- c(1,3,-2,7)

matriz <- rbind(vector_1, vector_2, vector_3, vector_4)
matriz

Sigma_21 = matriz[3:4,1:2]
Sigma_21

Sigma_12 = matriz[1:2,3:4]
Sigma_12

Sigma_11 = matriz[1:2,1:2]
Sigma_11_inv <- solve(Sigma_11)

Sigma_22 = matriz[3:4,3:4]
Sigma_22_inv <- solve(Sigma_22)

Sigma_11_inv_raizcuadrado<- sqrtm(Sigma_11_inv)
Sigma_11_inv_raizcuadrado

Sigma_22_inv_raizcuadrado<- sqrtm(Sigma_22_inv)
Sigma_22_inv_raizcuadrado

Matriz_u <- Sigma_11_inv_raizcuadrado%*%Sigma_12%*%Sigma_22_inv%*%Sigma_21%*%Sigma_11_inv_raizcuadrado
Matriz_u

eigenValuesU <- eigen(Matriz_u)$values
eigenVectorsU <- eigen(Matriz_u)$vectors
eigenValuesU                      
eigenVectorsU 


t(eigenVectorsU[,1])%*%Sigma_11_inv_raizcuadrado


#---------------------------Para f--------------------------------------#

Matriz_v <- Sigma_22_inv_raizcuadrado%*%Sigma_21%*%Sigma_11_inv%*%Sigma_12%*%Sigma_11_inv_raizcuadrado
Matriz_v

eigenValuesV <- eigen(Matriz_v)$values
eigenVectorsV <- eigen(Matriz_v)$vectors
eigenValuesV                      
eigenVectorsV 

t(eigenVectorsV[,1])%*%Sigma_22_inv_raizcuadrado

u1<-eigenVectorsU [,1]%*%Sigma_11_inv_raizcuadrado
v1<-eigenVectorsV[,1]%*%Sigma_22_inv_raizcuadrado
u2<-eigenVectorsU [,2]%*%Sigma_11_inv_raizcuadrado
v2<-eigenVectorsV[,2]%*%Sigma_22_inv_raizcuadrado


cat("\nVariantes Canonicas 2a: \n")
sprintf("u1 = %f X1(1) + %f X2(1)", u1[1] , u1[2])
sprintf("v1 = %f X3(1) + %f X4(1)", v1[1], v1[2])
sprintf("u2 = %f X1(2) + %f X2(2)", u2[1] , u2[2])
sprintf("v2 = %f X3(2) + %f X4(2)", v2[1], v2[2])



#La correlación entre las variables canónicas u1 y v1 es el primer valor propio de la matriz U.

cat("\nCorrelaciones Canónicas 2b: \n")
sprintf("Cor(U1,V1) = %f", sqrt(eigenValuesU[1]))
sprintf("Cor(U2,V2) = %f", sqrt(eigenValuesU[2]))
 
#---------------------------------Punto 3------------------------------------------#



vector_1 <- c(1,0.615,-0.111,-0.266)
vector_2 <- c(0.615,1,-0.195,-0.085)
vector_3 <- c(-0.111,-0.195,1,-0.269)
vector_4 <- c(-0.266,-0.085,-0.269,1)

matriz <- rbind(vector_1, vector_2, vector_3, vector_4)
matriz

Sigma_21 = matriz[3:4,1:2]
Sigma_21

Sigma_12 = matriz[1:2,3:4]
Sigma_12

Sigma_11 = matriz[1:2,1:2]
Sigma_11_inv <- solve(Sigma_11)

Sigma_22 = matriz[3:4,3:4]
Sigma_22_inv <- solve(Sigma_22)

Sigma_11_inv_raizcuadrado<- sqrtm(Sigma_11_inv)
Sigma_11_inv_raizcuadrado

Sigma_22_inv_raizcuadrado<- sqrtm(Sigma_22_inv)
Sigma_22_inv_raizcuadrado

Matriz_u <- Sigma_11_inv_raizcuadrado%*%Sigma_12%*%Sigma_22_inv%*%Sigma_21%*%Sigma_11_inv_raizcuadrado
Matriz_u

eigenValuesU <- eigen(Matriz_u)$values
eigenVectorsU <- eigen(Matriz_u)$vectors
eigenValuesU                      
eigenVectorsU 


t(eigenVectorsU[,1])%*%Sigma_11_inv_raizcuadrado


#---------------------------Para f--------------------------------------#

Matriz_v <- Sigma_22_inv_raizcuadrado%*%Sigma_21%*%Sigma_11_inv%*%Sigma_12%*%Sigma_11_inv_raizcuadrado
Matriz_v

eigenValuesV <- eigen(Matriz_v)$values
eigenVectorsV <- eigen(Matriz_v)$vectors
eigenValuesV                      
eigenVectorsV 

t(eigenVectorsV[,1])%*%Sigma_22_inv_raizcuadrado

u1<-eigenVectorsU [,1]%*%Sigma_11_inv_raizcuadrado
v1<-eigenVectorsV[,1]%*%Sigma_22_inv_raizcuadrado
u2<-eigenVectorsU [,2]%*%Sigma_11_inv_raizcuadrado
v2<-eigenVectorsV[,2]%*%Sigma_22_inv_raizcuadrado


cat("\nVariantes Canonicas 3a: \n")
sprintf("u1 = %f X1(1) + %f X2(1)", u1[1] , u1[2])
sprintf("v1 = %f X3(1) + %f X4(1)", v1[1], v1[2])
sprintf("u2 = %f X1(2) + %f X2(2)", u2[1] , u2[2])
sprintf("v2 = %f X3(2) + %f X4(2)", v2[1], v2[2])



#La correlación entre las variables canónicas u1 y v1 es el primer valor propio de la matriz U.

cat("\nCorrelaciones Canónicas 3b: \n")
sprintf("Cor(U1,V1) = %f", sqrt(eigenValuesU[1]))
sprintf("Cor(U2,V2) = %f", sqrt(eigenValuesU[2]))

##

