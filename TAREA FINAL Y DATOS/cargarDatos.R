library(foreign)
library(mvtnorm)

table <- read.dta("datos_politicos.dta") # LEEMOS LOS DATOS DE FORMATO STATA
anio <- 1986 # POR EL MOMENTO ESCOJAMOS UN SOLO A?O PUES NO SABEMOS NADA DE DATOS PANEL
data <- table[table$year==anio, ]
labels <- paste(names(data), attributes(data)$var.labels, sep=": ") # NOMBRES DE LAS VARIABLES

Y <- data$reg # INDICADORA DE SI SE ES O NO UNA DEMOCRACIA
list.depend <- c("level", "open", "g", "strikes", "govpwt", "ls", "invpwt",
                 "fertil", "lfagric", "popg", "femsec", "EnergProd") # VARIABLES ECON?MICAS EXPLICATIVAS

#analisis sin imputacion

X<- subset(data, select=list.depend) #Solo las que nos interesan
for (j in 1:ncol(X)) X[,j]<- as.numeric(X[,j])
row.names(X)<- data$name
X.comp<- X[complete.cases(X),]
nrow(X.comp) #Con Cuanto hariamos el estudio si no usaramos ipmutacion?


View(X.comp)
View(round(cor(X.comp),2))
data.full<-data.frame(Y[complete.cases(X)],X.comp)
names(data.full)[1]<-"Y"
res<-glm(Y~.,data=data.full,family="binomial")#Regresion Logit, el problema es que overfitting
guess<-round(predict(res))

##########
#Notas
#hago una regresion de la primera con las demas
#ahora de los valores arbitriarios que puse, los sustituyo por los valores actualizados despues de la regresion
#como este es un metodo para datos normales gaussianes, debo calcular la media, la varianza y calculo la logverosimilitud y repito los paseos hasta que converja
##########
#AHORA Si, Nos enfocamos en la imputacion condicional


nrows<-nrow(X)
ncols<-ncol(X)
m<-5
tol<- 1e-3
res<-list()
imputed.sets<-list()
pred.success<-numeric(m)

for(rep in 1:m){
  #bootstrap
  samp<-sample(1:nrows,nrows,replace=TRUE)
  Xb<-X[samp,]
  #Incializacion
  #paso 0
  M <- is.na(Xb) #m de missing
  sigma<-cov(Xb[complete.cases(Xb),]) #matrices de varianzas y covarianzas
  sd.vec<-sqrt(diag(sigma))
  mu <-apply(Xb[complete.cases(Xb),],2,mean)
  for(i in 1:nrows) for(j in 1:ncols) if(M[i,j]) Xb[i,j]<-rnorm(1,mu[j],sd.vec[j])
  logv<-sum(apply(Xb,1,function(row) log(dmvnorm(row,mu,sigma))))
  iter<-1
  repeat{
    #Valor Actual de la verosimilitud
    #iteraciones por variables
    for(j in 1:ncol(Xb)){
      ind<-as.matrix(Xb[,j],ncol=1)
      dep<-as.matrix(Xb[,-j])
      mod<-lm(ind~dep) #regresion lineal de la n-esima columna con todos los demas
      pred<-predict(mod)
      #for(k in 1:rows) if(M[k,j]) Xb[k,j]<-pred[k]
      Xb[M[,j],j]<-pred[M[,j]] #solo en los missing values pongo el fitted value
    }
    #nueva matrix se covarianzas
    sigma<-cov(Xb)
    mu<-apply(Xb,2,mean)
    logv[iter+1]<-sum(apply(Xb,1,function(row) log(dmvnorm(row,mu,sigma))))
    if(abs(logv[iter+1]-logv[iter]) < tol) break
    iter<-iter+1
  }
  print(paste("     - iteraciones totales: ", iter))
  imputed.sets[[rep]]<-Xb
  #GRAFICA
  plot(logv[-(1:3)],type="l",col="blue",main=paste("Bootstrap",rep))
  #Modelo
  data.full<-data.frame(Y[samp],Xb)
  names(data.full)[1]<-"Y"
  res[[rep]]<-glm(Y~.,data=data.full,family="binomial") #Rgresion logit
  guess<-round(predict(res[[rep]],type="response"))
  pred.success[rep]<-sum(guess==data.full$Y)/nrows
}
#pooling
beta.all<-matrix(0,nrow=ncols,ncol=m)
for(rep in 1:m){
  beta.all[,j]<-coef(res[[rep]])[-1]
  
}
#promedio de las betas
beta.estims<-apply(beta.all,1,mean)
#estimacion de las varianzas
beta.var.within<-numeric(ncols)
for(rep in 1:m){
  beta.var.within<-beta.var.within+(summary(res[[rep]])$coefficients[,2][-1])^2/m
}
beta.var.between <-apply(beta.all,2,var)
beta.var<-beta.var.within+(1+1/m)*beta.var.between

#z-values finales
table<-data.frame(beta=beta.estims,sd=sqrt(beta.var))