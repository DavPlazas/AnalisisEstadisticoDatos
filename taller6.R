Sigma<-matrix(c(100,0,0,0,0,1,0.95,0,0,0.95,1,0,0,0,0,100),nrow=4,ncol=4,byrow = TRUE)


Sigma11<-Sigma[1:2,1:2]
e1<-eigen(Sigma11)
v1<-e1$vectors
S11_half<-v1 %*% solve(diag(e1$values)) %*% t(v1)
Sigma12<-Sigma[1:2,3:4]
e2<-eigen(Sigma12)
v2<-e2$vectors
S12_half<-v2 %*% diag(e2$values) %*% t(v2)
Sigma21<-Sigma[3:4,1:2]
e3<-eigen(Sigma21)
v3<-e3$vectors
S21_half<-v3 %*% diag(e3$values) %*% t(v3)
Sigma22<-Sigma[3:4,3:4]
e4<-eigen(Sigma22)
v4<-e4$vectors
S22_half<-v4 %*% solve(diag(e4$values)) %*% t(v4)


U<-S11_half%*%Sigma12%*%solve(Sigma22)%*%Sigma21%*%S11_half
V<-S22_half%*%Sigma21%*%solve(Sigma11)%*%Sigma12%*%S22_half

eUvalues<-eigen(U)$values
eUvectors<-eigen(U)$vectors

fVvalues<-eigen(V)$values
fVvectors<-eigen(V)$vectors

u1<-eUvectors[,1]%*%S11_half
v1<-fVvectors[,1]%*%S22_half

cat("\nVariantes Canonicas 1a: \n")
sprintf("u1 = %f X1(1) + %f X2(1)", u1[1] , u1[2])
sprintf("v2 = %f X3(2) + %f X3(2)", v1[1], v1[2])

#https://stackoverflow.com/questions/3516008/how-to-print-r-variables-in-middle-of-string
