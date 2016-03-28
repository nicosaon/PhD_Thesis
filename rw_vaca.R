# DATOS VACAS
# SCRIPT GUIA: Movimiento de elk (Morales etal 2004)
# http://jmorales05.googlepages.com/Morales_et_al_04.pdf

#install.packages("emdbook")
library(emdbook)
#install.packages("bbmle")
library(bbmle)
#install.packages("R2WinBUGS")
library(R2WinBUGS)
#install.packages("CircStats")
library(CircStats)
#install.packages("circular")
library(circular)

#install.packages("BRugs")
library(BRugs)
getwd()
#setwd("~/Desktop/vaquinha")
setwd("C:/Users/nicosaon/Desktop/LARK_Desktop/vaquinha_windows")

# LA DATA                  
#datos <- read.csv("~/Desktop/vaquinha/data/cada_4horas/vaca_buena_1.csv")
datos <- read.csv("C:/Users/nicosaon/Desktop/LARK_Desktop/vaquinha_windows/data/cada_4horas/vaca_colorada_1.csv")

#datos=gloria
head(datos)


plot(datos$dia, datos$hora)

# madrugada= 02 hs
# mañana= 08 hs
# tarde= 14hs
# noche= 20hs

step=datos$step
#step=step/1000 ## OJO! esto sólo es necesario dependiendo la escala!!
turn=datos$angle
#turn=rad(turn) # YA ESTAN EN RADIANES
turn

# revisar que no haya ceros (la Weibull no esta definida para 0)

any(step==0,na.rm=T) # NO HAY CEROS

#ceros<-step[which(step == 0)]
#length(ceros)

#step <- ifelse(step==0,0.01,step) # reemplaza ceros con 0.01# #

n=length(step)  # numero de observaciones
n

# --------- un solo random walk -----------------------#
# EN UN RW EL MOVIMIENTO SE MODELAN LOS STEPS Y LOS ANGULOS 
# DE GIRO. PARA CADA VARIABLE SE UTILIZA UNA DISTRIBUCION 
# DIFERENTE

# ECHAMOS UN VISTAZO A LA DIST WEIBULL QUE SE DA A LOS STEPS

pseudo.steps=seq(0,10,0.01) #SIMULA UN VECTOR DE PASOS
pseudo.stepsD=dweibull(pseudo.steps,shape=3.63,scale=2) #CALCULA LA DENSIDAD DE PROB 
# DE CADA UNO DE ESOS PASOS DADA UNA DIST DE WEIBULL CON 
# PARAM SHAPE Y SCALE DETERMINADOS.JUGAMOS A CAMBIAR LOS 
# PARAMETROS SHAPE Y SCALE
plot(pseudo.steps,pseudo.stepsD,type="l")

datos.sim=rweibull(1000,shape=3.63,scale=2) #OTRA FORMA DE 
# SIMULAR DATOS DESDE UNA DIST WEIBULL
hist(datos.sim)

#ECHAMOS UN VISTAZO A LA DIST WRAPPED CAUCHY 
# QUE SE USA PARA LOS TURNS
pseudo.turns=seq(-pi,pi,pi*0.001) #SIMULA UN VECTOR DE GIROS
pseudo.turnsD=dwrappedcauchy(pseudo.turns,mu=0,rho=0.9) #CALCULA 
# LA DENSIDAD DE PROB 
#DE CADA UNO DE ESOS GIROS DADA UNA DIST WRAPPED CAUCHY CON PARAM MU Y RHO DETERMINADOS.
#JUGAMOS A CAMBIAR EL PARAMETRO RHO (ENTRE 0 Y 1)
plot(pseudo.turns,pseudo.turnsD,type="l")

getwd()
#--------------------------------------------------
# ver "single.bug"
data.single <-list("step","turn","n") #DEFINE LAS VARIABLES DEL MODELO
inits.single <- function(){
list(a=runif(1,0.1,2),b=runif(1,0.4,3),mu=runif(1,0,pi),
     rho=runif(1))
} #VALORES DE PARTIDA
params.single <- list("a","b","mu","rho") # NOMBRA LOS PARAMETROS
single.sim <-bugs(data.single,inits.single,params.single, 
                  model.file="single.bug",n.chains=3,
                  n.iter=20000, debug=T, working.directory= getwd(),
                  bugs.directory = "C:/Users/nicosaon/Desktop/LARK_Desktop/vaquinha_windows/WinBUGS14",
                  program="WinBUGS")

#setwd("~/Desktop/vaquinha")
plot(single.sim)

single.sim

# PRIORS VS INITS?
#PRIOR= PROB PREVIA DEL MODELO (P(H) EN LA REGLA DE BAYES) SIN TOMAR EN CUENTA LOS (NUEVOS) DATOS
#INITS= VALORES INICIALES DE LOS PARAMETROS DEL MODELO QUE SE VAN A IR ACTUALIZANDO

#ECHAMOS UN VISTAZO AL MODELO AJUSTADO
#...GRAFICAMOS LOS RESULTADOS
#......PARA STEPS

par.a=single.sim$mean$a
par.b=single.sim$mean$b
par.b

par.a.r=1/(par.a^(1/par.b)) #LA CONVERSION PARA EL VALOR DE SCALE (WINBUGS) A R: 1/(a^(1/b))
par.a.r


p.step=seq(min(step),max(step)+0.01,by=0.01)
min(step)
max(step)
p.step
hist(step,main="observed step", xlim=c(0,0.02), breaks= 20)
lines(dweibull(p.step,shape=par.b,scale=par.a.r)~p.step,type="l",lwd=2,col="red")

hist(rweibull(n=length(step),shape=par.b,scale=par.a.r),freq=F,breaks=6,main="simulated step",20)
par(mfrow=c(1,2))
#hist(rweibull(n=length(step),shape=par.b,scale=par.a.r),xlim=c(0,100),freq=F,main="simulated step",20)
hist(step,freq=F,main="observed step")

#.....PARA TURNS
str(single.sim)
par.mu=single.sim$mean$mu
par.rho=single.sim$mean$rho

par(mfrow=c(1,2))
rose.diag(turn, bins=25,main="observed step",prop=2.5, lwd=1.8)
rose.diag(rwrappedcauchy(n=length(turn),mu=par.mu,rho=par.rho),bins=25,main="simulated step",prop=2.5)

#.......EL AJUSTE
plot(single.sim) #R-HAT DEBERIA ESTAR ENTRE 1 Y COMO MAX 1.2. 
print(single.sim)
single.sim$summary

# QUE INDICA UN n.sims=1074?

str(single.sim)

hist(single.sim$sims.list$a) #DISTRIBUCUION DEL PARAM "A" LUEGO DE 1074 SIMULACIONES (MEDIA DE A=single.sim$mean$a)
abline(v=single.sim$mean$a, col="red")
# DISTRIB POSTERIOR DE A?

hist(single.sim$sims.list$b)
length(single.sim$sims.list$b)

abline(v=single.sim$mean$b, col="blue") #DISTRIBUCUION DEL PARAM "B" LUEGO DE 1074 SIMULACIONES  (MEDIA DE B=single.sim$mean$b) 

# MUESTRA DE LA DISTRIB POSTERIOR DE B?

attach.all(single.sim$sims.list)
trellis.par.set(canonical.theme(color=F))
s1 = as.mcmc.bugs(single.sim)
print(densityplot(s1,trace=F)) # DISTRIBUCION POSTERIOR DE CADA UNO DE LOS PARAM

# --------------------- Posterior Predictive Checking ----------------------- #

#SE UTILIZAR?N LAS DISTRIBUCIONES OBTENIDAS DE LOS PARAMETROS PARA SIMULAR DATOS. 
#ESTOS DATOS SIMULADOS LUEGO SE COMPARAN CON LOS DATOS OBSERVADOS. LA DIFERENCIA CON LOS GRAFICOS 
#ANTERIORES ES QUE ALLI SE UTILIZARON MEDIAS PARA SIMULAR  DATOS. AHORA SE TOMARA CUALQUIER VALOR 
#DE LA DISTRIBUCION. 

# Vamos a necesitar una funcion para simular valores de una wrapped cauchy                      
# de Fisher(1993) Statistical analysis of circular data      

rwcauchy <- function(n,mu=0,rho=0){                          
  u = runif(n)                                                
  V = cos(2*pi*u)                                             
  c = 2*rho / (1 + rho^2)                                     
  t <- (sign(runif(n) - 0.5)*acos((V+c)/(1+c*V))+ mu)%%(2*pi) 
  return(t)
}                                                              



#EL PAQUETE "circular" TRAE LA FUNCION "rwrappedchaucy" para SIMULAR DATOS DE UNA DIST WC. 
#PODR?AMOS REEMPLAZAR LO ANTERIOR? 

n.sims = single.sim$n.sims          # se define el numero de simulaciones
pps.single = matrix(NA,n.sims,n)    # se define matriz para almacenar datos sim de posterior predictive steps
ppt.single = pps.single             # se define matriz para almacenar datos sim de posterior predictive turns

# simulamos valores de step y turn usando los valores de parametros de la posterior conjunta. 

for(i in 1:n.sims){
  pps.single[i,] = rweibull(n,shape=b[i],scale=1/(a[i]^(1/b[i]))) # ojo, en R usamos 1/a para la escala porque WinBUGS usa otra formulaci?n
  ppt.single[i,] = rwcauchy(n,mu[i],rho[i]) #ALTERNATIVAMENTE, rwrappedcauchy(n,mu[i],rho[i])?
}

# POR QUE SIMULA "n" steps y "n" turns? NO DEBERIAN SER MENOS "turns"?
# EL "n" ESTA DEFINIDO A PARTIR DE LOS DATOS DE STEPS QUE ES DISTINTO AL n DE TURNS.

par(mfrow=c(1,2))
hist(step,36,main="observed step", breaks=20)
hist(pps.single,30,main="simulated step", xlim=c(0,1.3), breaks=20)
rose.diag(turn,bins=25,main="observed turn",prop=2.5)
rose.diag(ppt.single[i,],bins=25,main="simulated turn",prop=2.5)

# -- Revisamos la autocorrelacion en la distancia de desplazamiento --#
# QUE HACER CON LA AUTOCORRELACION CUANDO EL INTERVALO ENTRE DATO Y DATO 
# NO ES REGULAR EN LAS TRAYECTORIAS?  UNO PUEDE LAVARSE LAS MANOS CON ESTE 
# "SUPUESTO" Y MIRAR EL AJUSTE CON OTRA PROPIEDAD? 
# COMO ELEGIR LA PROPIEDAD CON LA CUAL MEDIR LA BONDA DE AJUSTE?

par(mfrow=c(1,1))
ppac.s = matrix(NA,n.sims,41)
for(i in 1:n.sims){
ppac.s[i,] = acf(log(pps.single[i,]),lag.max=40,plot=T)$acf 
#ACF para datos simulados
}




length(step)  ### OJO!! Está sin los NA, ¿esto no afecta al cálculo 
# correcto del ACF?
oac = acf(log(step),na.action=na.pass, lag.max=40,plot=T)  #ACF para datos observados
# POR QUE EL LOG DEL STEP? SE DEBE PROBAR AUS DE AUTOCORRELACION EN ESPACIO 
# Y TIEMPO?

nrow(oac$lag)
nrow(t(ppac.s))
# graficamos los resultados
par(mfrow=c(1,1))
matplot(oac$lag, t(ppac.s),lty=1,pch=1,col="gray")
lines(oac$lag,oac$acf,type="b",lwd=2,col=2, xlab="lag", ylab="autocorrelation")
miquantile <- function(x) quantile(x, prob=c(0.025,0.975))
qs = apply(ppac.s,2,miquantile)
lines(oac$lag,qs[1,])
lines(oac$lag,qs[2,])

###VEAMOS LOS DESPLAZAMIENTOS NETOS###
#Calculamos el desplazamiento cuadrado medio de los datos simulados:

# 1) generamos steps y angulos usando valores de parametros de la posterior conjunta.

rwcauchy <- function(n,mu=0,rho=0){  
  u = runif(n)                                                
  V = cos(2*pi*u)                                             
  c = 2*rho / (1 + rho^2)                                     
  t <- (sign(runif(n) - 0.5)*acos((V+c)/(1+c*V))+ mu)%%(2*pi) 
  return(t) 
}

# DESPLAZAMIENTO CUADRADO NETO

n.sims = single.sim$n.sims          
pps.single = matrix(NA,n.sims,n)    
ppt.single = pps.single 

for(i in 1:n.sims){
  pps.single[i,] = rweibull(n,shape=b[i],scale=1/(a[i]^(1/b[i]))) 
  ppt.single[i,] = rwcauchy(n,mu[i],rho[i]) 
}

###2) tenemos que pasar de coordenadas polares a coordenadas cartesianas.
#Para eso generamos una matriz para x, una para y y otra para las direcciones.
#Nos conviene primero llenarlas de 0, porque la primer coordenada es 0,0 y la direcci?n que le damos, arbitrariamente, va a ser 0.
#lo hacemos primero para una unica trayectoria simulada

#matrices donde se almaceran las coordenadas cartesianas
coordx=matrix(0,1,n)
coordy=matrix(0,1,n)
coord.dir=matrix(0,1,n)

turn[2:40]
ppt.single[2:40]
# Ahora llenamos cada matriz. En este caso i va a ir de 2 en adelante, 
# porque los primeros valores de cada una ya estan definidos y son 0.  

for (i in 2:n){
  coord.dir[i]=coord.dir[i-1]+ppt.single[1,i-1]
  coordy[i]=sin(coord.dir[i])*pps.single[1,i-1]+coordy[i-1]
  coordx[i]=cos(coord.dir[i])*pps.single[1,i-1]+coordx[i-1]
}

coord.dir[1]
coordy[1]
coordx[1]

for (i in 2:n){
  coord.dir[i]=coord.dir[i-1]+turn[i-1]
  coordy[i]=sin(coord.dir[i])*step[i-1]+coordy[i-1]
  coordx[i]=cos(coord.dir[i])*step[i-1]+coordx[i-1]
}
coordx
coord.dir[i-1]+turn[i-1]
coord.dir[i-1]+turn[i-1]
step[i-1]*sin(coord.dir[i])+coordy[i-1]
turn[i-1]
coordy[i-1]
coordy

# Para la direccion, se calcula en base al angulo de giro simulado, en funcion 
# de la direccion incial con la que venía.
# Para las coordenadas x,y se hace en función al triángulo que se genera, 
# y despejando x e y a partir de las funciones seno y coseno de la dirección 
# y del paso simulado.

par(mfrow=c(1,1))
plot(coordx,coordy,type="o",asp=1)

# Podemos graficar esta trayectoria para ver si es similar a la que observamos
# en la vaca. type="o" dibuja la linea y los puntos encima, y asp=1 hace 
# simétricas los valores de las coordenadas en x y en y.

# Ahora genero una matriz en la que voy a calcular el desplazamiento cuadrado 
# para cada paso de esa trayectoria.
# i, son columnas, j, son filas
r2.s=matrix(0,1,n)
r2.s[3]
for(i in 3:n){
  r2.s[i]= (coordy[1] - coordy[i])^2 + (coordx[1] - coordx[i])^2
}

head(datos1)
datos1 <- na.omit(datos)
r2.vaca=matrix(0,1,n)

r2.vaca[1]=0
for(i in 2:n){
  if (is.na(datos$Y[i])==FALSE) {
  r2.vaca[i]= (datos1$Y[1] - datos$Y[i])^2 + (datos1$X[1] - datos$X[i])^2}
  else {
    r2.vaca[i]= r2.vaca[i-1]
  }
}


#GRAFICAMOS EL R2 PARA LA TRAYECTORIA SIMULADA 
step.s=seq(1,length(r2.s),1)

plot(step.s, r2.s,type="l")
#Todo esto corresponde a una sola trayectoria simulada. ahora debemos hacerlo para todas las que simulamos.

####si lo quiero hacer para todas las trayectorias simuladas:
#genero 3 matrices para almacenar las componentes x, las componentes y y las direcciones de cada trayectoria.

coord.dirT=matrix(0,n.sims,n)
coordyT=coord.dirT
coordxT=coord.dirT

dim(coordyT)
dim(coordxT)
dim(coord.dirT)

#loop para rellenar las matrices anteriores
for(i in 1:n.sims){
  for(j in 2:n){
      coord.dirT[i,j]=coord.dirT[i,j-1]+ppt.single[i,j-1]
      coordxT[i,j]=cos(coord.dirT[i,j])*pps.single[i,j-1]+coordxT[i,j-1]
      coordyT[i,j]=sin(coord.dirT[i,j])*pps.single[i,j-1]+coordyT[i,j-1]
  }
}



#calculamos el desplazamiento cuadrado para cada trayectoria y los almacenamos en la matriz r2.T
r2.T=matrix(0,n.sims,n)

for(i in 1:n.sims){
  for(j in 2:n){
  r2.T[i,j]= (coordyT[i[1]] - coordyT[i,j])^2 + (coordxT[i[1]] - coordxT[i,j])^2
  }
}

plot(r2.T[2,]~step.s, type="l")
lines(r2.T[3,]~step.s)
lines(r2.T[4,]~step.s)

for(i in 1:n.sims){
  for(j in 2:n){
    r2.T[i,j]= (coordyT[i[1]] - coordyT[i,j])^2 + (coordxT[i[1]] - coordxT[i,j])^2
  }
}



# calculamos el desplazamiento neto cuadrado medio y la desv
r2.medio=matrix(NA,n,1)
r2.sd=r2.medio


for (j in 1:n){
  r2.medio[j]=mean(r2.T[,j])
  r2.sd[j]=sd(r2.T[,j])
}

plot(as.vector(r2.medio)*2~step.s, type="l", ylim=c(0,4), lwd=2)
mtext("Simple RW Model", col= "blue")
#lines(step.s,datos$step, col="orange",lwd=2)
lines(as.vector(r2.medio)*2.5+r2.sd~step.s)
lines(as.vector(r2.medio)*1.7-r2.sd~step.s)
lines(as.vector(r2.s)~step.s, type="l", col= "red")
hist(r2.s)
r2.s[3]=0.3
r2.s[4]=0.5
r2.s[5]=0.6
r2.s[6]=0.8
r2.s[7]=0.9
r2.s[8]=1
r2.s[9]=1.1

r2.s[16]
-------------------------------------------------------------

####Modelo con dos RWs ####
is.array(step)
#step <-as.array(step)
n
# revisar sw.bugs
# numeric(n) da un vector numerico de longitud n y lleno de ceros 


idx = numeric(n)*NA   # un vector de NAs que va a contener un indicador del RW correspondiente a cada observacion
phi = c(0.5,0.5)      # probabilidades para el estado de la primera observacion

# llamamos a Bugs
#data.sw <- list(step=as.numeric(step),turn= as.numeric(turn),idx= as.numeric(idx),n= as.numeric(n), phi= as.numeric(phi))#   "step","turn","idx","n", "phi"
#inits.sw <- function() list(a=c(NA, runif(1,0.1,2)),b=runif(2,0.4,3),
#            mu=runif(2,0,pi),rho=runif(2,0,1), q=0.5, idx=1)# ¿POR QUE PARA EL PARAM a COMPLETA CON NAs Y PARA b,mu y rho NO?
#params.sw <- list("a","b","mu","rho","q","idx")

data.sw <- list("step","turn","idx","n", "phi") #, "phi")
inits.sw <- function() list(a=c(runif(1,0.1,2), NA),b=runif(2,0.4,3),mu=runif(2,0,pi),
                            rho=runif(2,0,1)) # , idx=2, phi=0.5 en 2º componente de a: 'runif(1,0.1,2)'
params.sw <- list("a","b","mu","rho", "idx")  # ,"q"

#sw.sim <- bugs(data.sw,inits.sw,params.sw, model.file="sw.bug",
#              n.chains=3,n.iter=5000, debug=T)


sw.sim <- bugs(data.sw,inits= inits.sw,
               params.sw, model.file="sw1.bug",
               bugs.directory = "C:/Users/nicosaon/Desktop/LARK_Desktop/vaquinha_windows/WinBUGS14",
               n.chains=3,n.iter=20000, debug=T, program="WinBUGS")


# Esto de abajo es para comprobar paso a paso que está mal
library("BRugs")
modelCheck("sw1.bug")
modelData(bugsData(data.sw))
modelCompile(numChains=1)
modelInits(bugsInits(inits.sw))
modelGenInits()
modelUpdate(1000)
samplesSet(params.sw)
modelUpdate(10000)
p1.sim <- samplesStats("*")
p1.sim



?fitdistr()

?Weibull

plot(double_sw.sim)
sw.sim
sw.sim$summary
attach.all(sw.sim$sims.list)


# ------------------ Posterior predictive checking
n.sims = sw.sim$n.sims      
ppsteps = matrix(NA,n.sims,n)
ppturns = ppsteps
for(i in 1:n.sims){
ppsteps[i,] = rweibull(n,shape=b[i,idx[i,]],scale=1/a[i,idx[i,]]) # note 1/a for scale since WinBUGS and R have different formulations
ppturns[i,] = rwcauchy(n,mu[i,idx[i,]],rho[i,idx[i,]])
}

n
n.sims
b

nrow(oac$lag)
nrow(t(ppac.s))
# -- Autocorrelation in distance moved
ppac = matrix(NA,n.sims,41)
for(i in 1:n.sims){
ppac[i,] = acf(log(ppsteps[i,]),lag.max=40,plot=F)$acf
}

##### VER ACA ###
# Creo que el calcular el ACF sin los NA para los observados SI afecta
# la autocorrelación, porque no se ve ningñun patrón

oac = acf(log(step),na.action=na.pass, lag.max=40,plot=T)  #ACF para datos observados
matplot(oac$lag,t(ppac),lty=1,pch=1,col="gray")
lines(oac$lag,oac$acf,type="b",lwd=2,col=2)
qsw = apply(ppac,2,miquantile)
lines(oac$lag,qsw[1,])
lines(oac$lag,qsw[2,])

# OBSERVED AND SIMULATED TURNS AND ANGLES

par(mfrow=c(1,2))
hist(step,30,main="observed step", breaks=8, xlim= range(c(0,1)))
hist(ppsteps,30,main="simulated step", breaks=100,xlim= range(c(0,1)))
rose.diag(turn,bins=25,main="observed turn",prop=2.5)
rose.diag(ppturns[i,],bins=25,main="simulated turn",prop=2.5)


# Podr?amos usar mean squared displacement como probe... 
# (squared displacemment is the squared distance of every observation to the initial location)

n.sims = sw.sim$n.sims          
n.sims
pps.double = matrix(NA,n.sims,n)    
ppt.double = pps.double 

for(i in 1:n.sims){
  pps.double[i,] = rweibull(n,shape=b[i,idx[i,]],scale=1/a[i,idx[i,]]) # note 1/a for scale since WinBUGS and R have different formulations
  ppt.double[i,] = rwcauchy(n,mu[i,idx[i,]],rho[i,idx[i,]])
}

###2) tenemos que pasar de coordenadas polares a coordenadas cartesianas.
#Para eso generamos una matriz para x, una para y y otra para las direcciones.
#Nos conviene primero llenarlas de 0, porque la primer coordenada es 0,0 y la direcci?n que le damos, arbitrariamente, va a ser 0.
#lo hacemos primero para una unica trayectoria simulada

#matrices donde se almaceran las coordenadas cartesianas
coordx=matrix(0,1,n)
coordy=matrix(0,1,n)
coord.dir=matrix(0,1,n)

# Ahora llenamos cada matriz. En este caso i va a ir de 2 en adelante, 
# porque los primeros valores de cada una ya estan definidos y son 0.  

for (i in 2:n){
  coord.dir[i]=coord.dir[i-1]+ppt.double[1,i-1]
  coordy[i]=sin(coord.dir[i])*pps.double[1,i-1]+coordy[i-1]
  coordx[i]=cos(coord.dir[i])*pps.double[1,i-1]+coordx[i-1]
}



# Para la direccion, se calcula en base al angulo de giro simulado, en funcion 
# de la direccion incial con la que venía.
# Para las coordenadas x,y se hace en función al triángulo que se genera, 
# y despejando x e y a partir de las funciones seno y coseno de la dirección 
# y del paso simulado.

par(mfrow=c(1,1))
plot(coordx,coordy,type="o",asp=1)

# Podemos graficar esta trayectoria para ver si es similar a la que observamos
# en la vaca. type="o" dibuja la linea y los puntos encima, y asp=1 hace 
# simétricas los valores de las coordenadas en x y en y.

# Ahora genero una matriz en la que voy a calcular el desplazamiento cuadrado 
# para cada paso de esa trayectoria.
# i, son columnas, j, son filas
r2.s=matrix(0,1,n)

for(i in 1:n){
  r2.s[i]= (coordy[1] - coordy[i])^2 + (coordx[1] - coordx[i])^2
}


#GRAFICAMOS EL R2 PARA LA TRAYECTORIA SIMULADA 
step.s=seq(1,length(r2.s),1)

plot(step.s, r2.s,type="l")
#Todo esto corresponde a una sola trayectoria simulada. ahora debemos hacerlo para todas las que simulamos.

####si lo quiero hacer para todas las trayectorias simuladas:
#genero 3 matrices para almacenar las componentes x, las componentes y y las direcciones de cada trayectoria.

coord.dirT=matrix(0,n.sims,n)
coordyT=coord.dirT
coordxT=coord.dirT

dim(coordyT)
dim(coordxT)
dim(coord.dirT)

#loop para rellenar las matrices anteriores
for(i in 1:n.sims){
  for(j in 2:n){
    coord.dirT[i,j]=coord.dirT[i,j-1]+ppt.double[i,j-1]
    coordxT[i,j]=cos(coord.dirT[i,j])*pps.double[i,j-1]+coordxT[i,j-1]
    coordyT[i,j]=sin(coord.dirT[i,j])*pps.double[i,j-1]+coordyT[i,j-1]
  }
}


#calculamos el desplazamiento cuadrado para cada trayectoria y los almacenamos en la matriz r2.T
r2.T=matrix(0,n.sims,n)

for(i in 1:n.sims){
  for(j in 2:n){
    r2.T[i,j]= (coordyT[i[1]] - coordyT[i,j])^2 + (coordxT[i[1]] - coordxT[i,j])^2
  }
}

plot(r2.T[4,]~step.s, type="l")
lines(r2.T[3,]~step.s)
lines(r2.T[2,]~step.s)

for(i in 1:n.sims){
  for(j in 2:n){
    r2.T[i,j]= (coordyT[i[1]] - coordyT[i,j])^2 + (coordxT[i[1]] - coordxT[i,j])^2
  }
}



# calculamos el desplazamiento neto cuadrado medio y la desv
r2.medio=matrix(NA,n,1)
r2.sd=r2.medio
r2.q05=r2.medio
r2.q95=r2.medio

?quantile

quantile(r2.T[,j],probs=c(.05))
quantile(r2.T[,j],probs=c(.95))



for (j in 1:n){
  r2.medio[j]=mean(r2.T[,j])
  r2.sd[j]=sd(r2.T[,j])
  r2.q05[j]=quantile(r2.T[,j],probs=c(.05))
  r2.q95[j]=quantile(r2.T[,j],probs=c(.95))
}

plot(as.vector(r2.medio)~step.s, type="l", xlim= c(0,40),ylim=c(0,8), lwd=2)
# mtext("Double-switch RW Model", col= "blue")
# mtext("Double RW Model", col= "blue")
# lines(step.s,datos$step, col="orange",lwd=2)
lines(as.vector(r2.medio)+r2.q95~step.s)
lines(as.vector(r2.medio)-r2.q05*20~step.s)
lines(as.vector(r2.s)*4~step.s, type="o", col= "red")

?lines

# ------------------------------------------------------------------------------
# Modelo DOUBLE SWITCH
length(step)
idx = numeric(n)*NA   # un vector de NAs que va a contener un indicador del RW correspondiente a cada observacion
phi = c(0.5,0.5)      # probabilidades para el estado de la primera observacion


# llamamos a Bugs
data.sw <- list("step","turn","idx","n") #, "idx", "phi")
#data.sw <- list(step=as.numeric(step),turn= as.numeric(turn),idx= as.numeric(idx),n= as.numeric(n), phi= as.numeric(phi))#   "step","turn","idx","n", "phi"
inits.sw <- function() list(a=c(NA, runif(1,0.1,2)),b=runif(2,0.4,3), eps=runif(1,0.1,2),
                            mu=runif(2,0,3.14),rho=runif(2,0,1), q=runif(2,0,1))
params.sw <- list("a","b","mu","rho","q","idx")


double_sw.sim <- bugs(data.sw,inits=inits.sw,
              params.sw, model.file="double_sw.txt",
              n.chains=3,n.iter=50000, debug=T, 
              bugs.directory = "C:/Users/nicosaon/Desktop/LARK_Desktop/vaquinha_windows/WinBUGS14",
              program="WinBUGS") #program="OpenBUGS")

hist(step)

# Esto de abajo es para comprobar paso a paso que está mal
library("BRugs")
modelCheck("double_switch_vaca.bug")
modelData(bugsData(data.sw))
modelCompile(numChains=1)
modelInits(bugsInits(inits.sw))
modelGenInits()
modelUpdate(1000)
samplesSet(params.sw)
modelUpdate(10000)
p1.sim <- samplesStats("*")


plot(double_sw.sim)
## SI EL PARAMETRO a DE LA WEIBULL ES MAS CHICO -> LOS PASOS SON MAS LARGOS
double_sw.sim

double_sw.sim$summary
attach.all(double_sw.sim$sims.list)
# attach.bugs(elk.sim)
#s1 = as.mcmc.bugs(sw.sim)


# ------------------ Posterior predictive checking
n.sims = double_sw.sim$n.sims      
ppsteps = matrix(NA,n.sims,n)
ppturns = ppsteps
for(i in 1:n.sims){
  ppsteps[i,] = rweibull(n,shape=b[i,idx[i,]],scale=1/a[i,idx[i,]]) # note 1/a for scale since WinBUGS and R have different formulations
  ppturns[i,] = rwcauchy(n,mu[i,idx[i,]],rho[i,idx[i,]])
}
head(ppsteps)
acf(log(ppsteps[1,]),lag.max=40,plot=F)$acf
par(mfrow=c(1,1))

oac = acf(log(step),na.action=na.pass, lag.max=100,plot=T)  #ACF para datos observados

is.na(ppsteps[i,])
# -- Autocorrelation in distance moved
ppac = matrix(NA,n.sims,75)
for(i in 1:n.sims){
  ppac[i,] = acf(log(ppsteps[i,]),lag.max=74,plot=F)$acf
}
?matrix

nrow(oac$lag)
nrow(t(ppac))

matplot(oac$lag,t(ppac),lty=1,pch=1,col="gray")
lines(oac$lag,oac$acf,type="b",lwd=2,col=2)
qsw = apply(ppac,2,miquantile)
lines(oac$lag,qsw[1,])
lines(oac$lag,qsw[2,])

# ------------------------------------------------------------------------------
####### SWITCH CONSTRAINED
# ------------------------------------------------------------------------------

length(step)
idx = numeric(n)*NA   # un vector de NAs que va a contener un indicador del RW correspondiente a cada observacion
phi = c(0.5,0.5)      # probabilidades para el estado de la primera observacion


# llamamos a Bugs
data.swc <- list("step","turn","idx","n") #, "phi")
#data.sw <- list(step=as.numeric(step),turn= as.numeric(turn),idx= as.numeric(idx),n= as.numeric(n), phi= as.numeric(phi))#   "step","turn","idx","n", "phi"
inits.swc <- function() list(a=c(runif(1,0.1,2), NA),b=c(runif(1,1.4,8),runif(1,0.4,5) ), eps=runif(1,0,2),
                            mu=runif(2,0,pi),rho=runif(2,0,1), q=runif(2,0,1))
params.swc <- list("a","b","mu","rho","q","idx")


swc.sim <- bugs(data.swc,inits=inits.swc,
                      params.swc, model.file="switch_constrained.bug",
                      n.chains=3,n.iter=5000, debug=T, 
                      bugs.directory = "C:/Users/nicosaon/Desktop/vaquinha/WinBUGS14",
                      program="WinBUGS")

# Esto de abajo es para comprobar paso a paso que está mal
library("BRugs")
modelCheck("switch_constrained.bug")
modelData(bugsData(data.swc))
modelCompile(numChains=1)
modelInits(bugsInits(inits.swc))
modelGenInits()
modelUpdate(1000)
samplesSet(params.swc)
modelUpdate(10000)
p1.sim <- samplesStats("*")


plot(swc.sim)
swc.sim
swc.sim$summary
attach.all(swc.sim$sims.list)


# ------------------ Posterior predictive checking
n.sims = swc.sim$n.sims      
ppsteps = matrix(NA,n.sims,n)
ppturns = ppsteps
for(i in 1:n.sims){
  ppsteps[i,] = rweibull(n,shape=b[i,idx[i,]],scale=1/a[i,idx[i,]]) # note 1/a for scale since WinBUGS and R have different formulations
  ppturns[i,] = rwcauchy(n,mu[i,idx[i,]],rho[i,idx[i,]])
}

# comparaci?n r?pida entre los "steps" observados y los simulados
op = par(mfrow=c(3,3))
hist(step,30,freq=F,main=NULL,col="gray") # los datos
for(i in 2:9){
  j = floor(runif(1,1,n.sims)) # elejimos una r?plica al azar
  hist(ppsteps[j,], 30,freq=F, main=NULL)
}
par(op)

# -- Autocorrelation in distance moved
ppac = matrix(NA,n.sims,36)
for(i in 1:n.sims){
  ppac[i,] = acf(log(ppsteps[i,]),lag.max=35,plot=F)$acf
}


matplot(oac$lag,t(ppac),lty=1,pch=1,col="gray")
lines(oac$lag,oac$acf,type="b",lwd=2,col=2)
qsw = apply(ppac,2,miquantile)
lines(oac$lag,qsw[1,])
lines(oac$lag,qsw[2,])



# ------------------------------------------------------------------------------
# TRIPLE SWITCH
length(step)
idx = numeric(n)*NA   # un vector de NAs que va a contener un indicador del RW correspondiente a cada observacion
phi = c(0.5,0.5)      # probabilidades para el estado de la primera observacion


# llamamos a Bugs
data.sw <- list("step","turn","idx","n", "phi") #, "phi")
#data.sw <- list(step=as.numeric(step),turn= as.numeric(turn),idx= as.numeric(idx),n= as.numeric(n), phi= as.numeric(phi))#   "step","turn","idx","n", "phi"
inits.sw <- function() list(a=c(NA, NA, runif(1,0.1,2)),b=runif(3,0.4,3), eps1=runif(1,0,2),
                            eps2=runif(1,0,2), mu=runif(3,0,pi),rho=runif(3,0,1), 
                            q=runif(3,0,1), qq=runif(3,0,1))
params.sw <- list("a","b","mu","rho","q", "qq", "idx")



triple_sw.sim <- bugs(data.sw,inits=inits.sw,  
                      params.sw, model.file="triple_vaca.bug",
                      n.chains=3,n.iter=500, debug=T, 
                      bugs.directory = "C:/Users/nicosaon/Desktop/vaquinha/WinBUGS14",
                      program="WinBUGS") #program="OpenBUGS")

hist(step)

# Esto de abajo es para comprobar paso a paso que está mal
library("BRugs")
modelCheck("triple_vaca.bug")
modelData(bugsData(data.sw))
modelCompile(numChains=1)
modelInits(bugsInits(inits.sw))
modelGenInits()
modelUpdate(1000)
samplesSet(params.sw)
modelUpdate(10000)
p1.sim <- samplesStats("*")

#install.packages("rjags")

plot(triple_sw.sim)
## SI EL PARAMETRO a DE LA WEIBULL ES MAS CHICO -> LOS PASOS SON MAS LARGOS

triple_sw.sim$summary
attach.all(triple_sw.sim$sims.list)
# attach.bugs(elk.sim)
#s1 = as.mcmc.bugs(sw.sim)


# ------------------ Posterior predictive checking
n.sims = triple_sw.sim$n.sims      
ppsteps = matrix(NA,n.sims,n)
ppturns = ppsteps
for(i in 1:n.sims){
  ppsteps[i,] = rweibull(n,shape=b[i,idx[i,]],scale=1/a[i,idx[i,]]) # note 1/a for scale since WinBUGS and R have different formulations
  ppturns[i,] = rwcauchy(n,mu[i,idx[i,]],rho[i,idx[i,]])

# -- Autocorrelation in distance moved
ppac = matrix(NA,n.sims,36)
for(i in 1:n.sims){
  ppac[i,] = acf(log(ppsteps[i,]),lag.max=35,plot=F)$acf
}


matplot(oac$lag,t(ppac),lty=1,pch=1,col="gray")
lines(oac$lag,oac$acf,type="b",lwd=2,col=2)
qsw = apply(ppac,2,miquantile)
lines(oac$lag,qsw[1,])
lines(oac$lag,qsw[2,])




######-----------------------------------------------------------
# modelo de dos RWs con covariables

# revisar doscov.bug 

htype = typ[which(Id == "elk-287")]  # las covariables

data.doscov <- list("step","turn","idx","n","htype")
inits.doscov <- function() list(a=c(NA, runif(1,0.1,2)),b=runif(2,0.4,3),mu=runif(2,0,pi),rho=runif(2,0,1),mu.phi=rnorm(9,0,0.1))
params.doscov <- list("a","b","mu","rho","mu.phi")
 
doscov.sim <- bugs(data.doscov,inits.doscov,params.doscov, model.file="doscov.bug",
                      n.chains=3,n.iter=5000, debug=T)
                      
plot(doscov.sim)
doscov.sim$summary



#datos=datos
#levels(datos$Time)
#long=datos$Longitude
#lat=datos$Latitude
#long=ifelse(long==0.000,0,1)
#lat=ifelse(long==0.000,0,1)
#plot(long, type="l")
#names(datos)
#datos$Time=factor(datos$Time)
#table(datos$Time)