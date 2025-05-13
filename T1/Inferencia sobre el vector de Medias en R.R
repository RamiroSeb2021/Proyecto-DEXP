n = 50
x_barra = c(62,173)

V = matrix(c(25,50,50,144),ncol=2)
V_inversa = solve(V)

mu0 = c(65, 162)

estadistica=c(n*t(x_barra - mu0)%*%V_inversa%*%(x_barra-mu0))

pchisq(estadistica, 2, lower=F)

## Región de Confianza

region_confianza = function(mu, conf){
  #mu representa el vector a evaluar
  #conf el nivel de confianza
  estadistica=c(n*(x_barra - as.numeric(mu))%*%V_inversa%*%(x_barra - as.numeric(mu)))
  percentil = qchisq(conf, length(x_barra))
  if (estadistica<=percentil){
    return(1)
  } else {return(0)}
}

aleat = expand.grid(seq(50,90,length.out=500),seq(120,200,length.out=500))

x = rep(NA,nrow(aleat))

for(i in 1:nrow(aleat)){
  x[i] = region_confianza(aleat[i,],0.95)
}

aleat = cbind(aleat,x)

aleat = as.data.frame(aleat)
conf_region=aleat[aleat$x==1,]

windows()
plot(conf_region$Var1, conf_region$Var2,pch=20,col="red")


# Supongamos que V no es conocido -----------------------------------------

S = matrix(c(35,55,55,169),ncol=2)
S_inversa = solve(S)

estadistica=c(n*(n-2)/(2*(n-1))*t(x_barra - mu0)%*%S_inversa%*%(x_barra-mu0))

pf(estadistica, 2, n-2, lower=F)

## Región de Confianza

region_confianza_varianza_no_conocida = function(mu, conf){
  #mu representa el vector a evaluar
  #conf el nivel de confianza
  estadistica = c(n*(x_barra - as.numeric(mu))%*%S_inversa%*%(x_barra - as.numeric(mu)))
  estadistica= (n-length(x_barra))/(length(x_barra)*(n-1)) * estadistica
  percentil = qf(conf, length(x_barra), n-length(x_barra))
  if (estadistica<=percentil){
    return(1)
  } else {return(0)}
}

x2 = rep(NA,nrow(aleat))

for(i in 1:nrow(aleat)){
  x2[i] = region_confianza_varianza_no_conocida(aleat[i,1:2],0.95)
}

aleat = cbind(aleat,x2)

aleat = as.data.frame(aleat)
conf_region_var_no_conocida=aleat[aleat$x2==1,]

windows()
par(mfrow=c(1,2))
plot(conf_region$Var1, conf_region$Var2,pch=20,col="red",
     ylim=c(165, 180), xlim=c(60,64))
plot(conf_region_var_no_conocida$Var1, 
     conf_region_var_no_conocida$Var2,pch=20,col="blue",
     ylim=c(165, 180), xlim=c(60,64))
