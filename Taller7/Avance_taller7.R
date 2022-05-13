
#----------------------------------------Punto 1---------------------------------------------------#
vector_1_X1 <- c(3,7);
vector_2_X1 <- c(2,4);
vector_3_X1 <- c(4,7);


matriz_X1 <- rbind(vector_1_X1 , vector_2_X1 , vector_3_X1 );
matriz_X1

vector_1_X2 <- c(6,9);
vector_2_X2 <- c(5,7);
vector_3_X2 <- c(4,8);



matriz_X2 <- rbind(vector_1_X2, vector_2_X2, vector_3_X2);
matriz_X2


vector_1_X1_Mean <- c(3);
vector_2_X1_Mean <- c(6);

matriz_X1_mean <- rbind(vector_1_X1_Mean , vector_2_X1_Mean);
matriz_X1_mean

vector_1_X2_Mean <- c(5);
vector_2_X2_Mean <- c(8);

matriz_X2_mean <- rbind(vector_1_X2_Mean , vector_2_X2_Mean);
matriz_X2_mean

vector_1_Spooled<- c(1,1);
vector_2_Spooled <- c(1,2);

matriz_Spooled<- rbind(vector_1_Spooled , vector_2_Spooled);
matriz_Spooled

#------------------Punto 1a---------------------#


y_gorro = t(matriz_X1_mean - matriz_X2_mean)%*%solve(matriz_Spooled);

sprintf("y_gorro = %f X1 + %f X2", y_gorro[1] , y_gorro[2])


#------------------Punto 1b---------------------#

vector_1_X0 <- c(2);
vector_2_X0 <- c(7);

matriz_X0 <- rbind(vector_1_X0, vector_2_X0);
matriz_X0

lado_izq = t(matriz_X1_mean - matriz_X2_mean)%*%solve(matriz_Spooled)%*%matriz_X0 - 0.5*t(matriz_X1_mean - matriz_X2_mean)%*%solve(matriz_Spooled)%*%(matriz_X1_mean + matriz_X2_mean)
lado_izq

cat("\nComo 4 es mayor a cero, a X0 la clasificamos en la población pi1: \n")

#------------------Punto 1c---------------------#


m = 0.5*t(matriz_X1_mean - matriz_X2_mean)%*%solve(matriz_Spooled)%*%(matriz_X1_mean + matriz_X2_mean)
m

cat("\nCuando Y_gorro es mayor o igual a -8, a X0 la clasificamos en la población pi1, de lo controario, lo clasificamos en pi2: \n")

#------------------Punto 1d---------------------#

#Obs1#
vector_1_Obs_1 <- c(3);
vector_2_Obs_1 <- c(7);
matriz_Obs_1 <- rbind(vector_1_Obs_1, vector_2_Obs_1);
y_gorro_obs1 = t(matriz_X1_mean - matriz_X2_mean)%*%solve(matriz_Spooled)%*%matriz_Obs_1

cat("\nObservación 1: \n")
sprintf("obs1 = (%f,%f)", matriz_Obs_1[1] , matriz_Obs_1[2])
sprintf("Como %f es mayor a -8, la clasificamos en la población 1 ", y_gorro_obs1)

#Obs2#
vector_1_Obs_2 <- c(2);
vector_2_Obs_2 <- c(4);
matriz_Obs_2 <- rbind(vector_1_Obs_2, vector_2_Obs_2);
y_gorro_obs2 = t(matriz_X1_mean - matriz_X2_mean)%*%solve(matriz_Spooled)%*%matriz_Obs_2

cat("\nObservación 2: \n")
sprintf("obs2 = (%f,%f)", matriz_Obs_2[1] , matriz_Obs_2[2])
sprintf("Como %f es mayor a -8, la clasificamos en la población 1 ", y_gorro_obs2)

#Obs3#
vector_1_Obs_3 <- c(4);
vector_2_Obs_3 <- c(7);
matriz_Obs_3 <- rbind(vector_1_Obs_3, vector_2_Obs_3);
y_gorro_obs3 = t(matriz_X1_mean - matriz_X2_mean)%*%solve(matriz_Spooled)%*%matriz_Obs_3

cat("\nObservación 3: \n")
sprintf("obs3 = (%f,%f)", matriz_Obs_3[1] , matriz_Obs_3[2])
sprintf("Como %f es igual a -8, la clasificamos en la población 1 ", y_gorro_obs3)

#Obs4#
vector_1_Obs_4 <- c(6);
vector_2_Obs_4 <- c(9);
matriz_Obs_4 <- rbind(vector_1_Obs_4, vector_2_Obs_4);
y_gorro_obs4 = t(matriz_X1_mean - matriz_X2_mean)%*%solve(matriz_Spooled)%*%matriz_Obs_4

cat("\nObservación 4: \n")
sprintf("obs4 = (%f,%f)", matriz_Obs_4[1] , matriz_Obs_4[2])
sprintf("Como %f es menor a -8, la clasificamos en la población 2 ", y_gorro_obs4)

#Obs5#
vector_1_Obs_5 <- c(5);
vector_2_Obs_5 <- c(7);
matriz_Obs_5 <- rbind(vector_1_Obs_5, vector_2_Obs_5);
y_gorro_obs5 = t(matriz_X1_mean - matriz_X2_mean)%*%solve(matriz_Spooled)%*%matriz_Obs_5

cat("\nObservación 5: \n")
sprintf("obs5 = (%f,%f)", matriz_Obs_5[1] , matriz_Obs_5[2])
sprintf("Como %f es menor a -8, la clasificamos en la población 2 ", y_gorro_obs5)

#Obs6#
vector_1_Obs_6 <- c(4);
vector_2_Obs_6 <- c(8);
matriz_Obs_6 <- rbind(vector_1_Obs_6, vector_2_Obs_6);
y_gorro_obs6 = t(matriz_X1_mean - matriz_X2_mean)%*%solve(matriz_Spooled)%*%matriz_Obs_6

cat("\nObservación 6: \n")
sprintf("obs5 = (%f,%f)", matriz_Obs_6[1] , matriz_Obs_6[2])
sprintf("Como %f es igual a -8, la clasificamos en la población 2 ", y_gorro_obs6)


cat("\nMatriz de confusión: \n")
vector_1_Conf <- c(3,0);
vector_2_Conf <- c(1,2);
matriz_Conf <- rbind(vector_1_Conf, vector_2_Conf);
matriz_Conf
cat("\nDado que las 3 muestras de la población 1 se clasifican como población 1, en la primera posición de la primera fila tenemos 3 y en la segunda 0 \n")
cat("\nDado que 1 muestra de la población 2 se clasifica como población 1, y las restantes se clasifican correctamente en la población 2, en la primera posición de la segunda fila tenemos 1 y en la segunda 2 \n")
#------------------Punto 1e---------------------#

APER = (1+0)/6;
APER;

sprintf("El apparent error rate es: %f", APER)


#--------------------------------------------------------------------------------------------------#

#----------------------------------------Punto 2---------------------------------------------------#

vector_1_X1_Mean_2 <- c(-1);
vector_2_X1_Mean_2 <- c(-1);

matriz_X1_mean_2 <- rbind(vector_1_X1_Mean_2 , vector_2_X1_Mean_2);
matriz_X1_mean_2

vector_1_X2_Mean_2 <- c(2);
vector_2_X2_Mean_2 <- c(1);

matriz_X2_mean_2 <- rbind(vector_1_X2_Mean_2 , vector_2_X2_Mean_2);
matriz_X2_mean_2

vector_1_Spooled_2 <- c(7.3,-1.1);
vector_2_Spooled_2 <- c(-1.1,4.8);

matriz_Spooled_2 <- rbind(vector_1_Spooled_2 , vector_2_Spooled_2);
matriz_Spooled_2

#------------------Punto 2a---------------------#

y_gorro_2 = t(matriz_X1_mean_2 - matriz_X2_mean_2)%*%solve(matriz_Spooled_2);

sprintf("y_gorro = %f X1 + %f X2", y_gorro_2[1] , y_gorro_2[2])

#------------------Punto 2b---------------------#

vector_1_X0_2<- c(0);
vector_2_X0_2<- c(1);

matriz_X0_2 <- rbind(vector_1_X0_2, vector_2_X0_2);
matriz_X0_2

lado_izq_2 = t(matriz_X1_mean_2 - matriz_X2_mean_2)%*%solve(matriz_Spooled_2)%*%matriz_X0_2 - 0.5*t(matriz_X1_mean_2 - matriz_X2_mean_2)%*%solve(matriz_Spooled_2)%*%(matriz_X1_mean_2 + matriz_X2_mean_2)
lado_izq_2

sprintf("Como %f es menor a cero, a X0 la clasificamos en la población pi2", lado_izq_2)


