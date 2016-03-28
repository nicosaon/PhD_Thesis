##################################################################################
# FIELD SIGNS CHAPTER
##################################################################################

# Loading the data ####

# LARK:
data <- read.delim("C:/Users/nicosaon/Desktop/field_signs/parcelas_compilado_provisorio.csv")

# MSI
data <- read.delim("~/Desktop/Master_Data_MSI/PARCELAS_master_data/parcelas_2015.csv")

# Workstation:
data <- read.csv("~/Desktop/field_signs/parcelas_compilado_provisorio.csv")
data <- read.csv("~/Desktop/field_signs/parcelas_compilado_2.csv") ## This has UTM coordinates
##################################################################################
# MAPPING THE FIELD PLOTS ####
x <- data$Longitud
y <- data$Latitud
plot(x,y, asp= 1)
# Throw out or fix wrong values...
which(y < -41.5)
data[331,]
y = y [-66]
y = y [-67]
which(y < -41.5)
x = x [-66]
x = x [-67]
which(x > -71.45)
data[271,]

# Delete an empty data row
data <- data[-36,]
# Plot the data
data$x <- data$Longitud
data$y <- data$Latitud
plot(x,y, asp= 1)
str(data)
str(data$Año)
# Plot the sampling plots coloured by year...
attach(data); 
plot(data$x, data$y, col=c("red","blue","green","orange")[as.factor(data$Año)], 
     xlab="Longitud", ylab="Latitud",asp= 1, pch=20, cex=2); 
legend(x="topleft", legend = levels(as.factor(data$Año)), col=c("red","blue","green","orange"), pch=20, pt.cex=2)
detach(data)
##################################################################################
# Convert Lat/Long to UTM ##########
require("PBSmapping") 
str(data)
data[32]
colnames(data)[31] <- "X"
colnames(data)[32] <- "Y"

is.na(data$X)
str(data)
data[,31:32]
# If empty places, put a 1 in each NA cell before transform
input <- commandArgs(TRUE)[1]
coord <- data[,31:32]
attr(coord, "projection") <- "LL"
str(coord)
attr(coord, "zone") <- 19
coord.utm <- convUL(na.omit(coord))
head(coord.utm)
coord.utm$X <- coord.utm$X*1000
coord.utm$Y <- coord.utm$Y*1000
head(coord.utm)
plot(coord.utm$X, coord.utm$Y, asp=1)
# OKEY!! :)
# Add the UTM coordinates to the data set
data$X.utm <-coord.utm$X
data$Y.utm <-coord.utm$Y

getwd()
write.csv(data[,c(1,7,31:34)], file= "parcelas_location.csv")
write.csv(data, file= "parcelas_compilado_2.csv")

##################################################################################
# Calculate distances between plots ####

# Calculate distance in kilometers between two points
earth.dist <- function (long1, lat1, long2, lat2)
{
  a1 <- lat1
  a2 <- long1 
  b1 <- lat2 
  b2 <- long2
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 57.04189
  d <- R * c
  return(d)
}

# attach(data)
earth.dist(X.utm[1], Y.utm[1],  X.utm[2], Y.utm[2])
# Hay 100.5 metros entre estos puntos

##################################################################################
# Joining forests types ####
str(data)
levels(data$Ambiente)

lev <- with(data, levels(Ambiente))
lev
lev[lev == "lenga_bosque"] <- "forest"
lev[lev == "mix_forest"] <- "forest"
lev[lev == "nire_bosque"] <- "forest"

data <- within(data, levels(Ambiente) <- lev)
levels(data$Ambiente)

data[data$Ambiente=="alto_andino",]
##################################################################################
## Standardizing variables ####

hist(data$Altura)
hist(data$Pendiente)
sum(data$ramoneo==3)
sum(data$ramoneo==0)
table(data$ramoneo)
barplot(table(as.factor(data$ramoneo)), xlab="Grado de ramoneo", ylab="Número de ocurrencias", las=1)
data$ramoneo[is.na(data$ramoneo)] <- 0
hist(data$bostas)
data$bostas[is.na(data$bostas)] <- 0
hist(data$bostas, prob=T)

data$Roca[is.na(data$Roca)] <- 0
data$Pasto[is.na(data$Pasto)] <- 0
data$Suelo.desnudo[is.na(data$Suelo.desnudo)] <- 0
data$Distancia.a.picada[is.na(data$Distancia.a.picada)] <- 0
str(data)
hist(data$Roca) 
hist(data$Pasto) 
hist(data$Suelo.desnudo)
hist(data$Distancia.a.picada)


### ESTA LINEA ES AD-HOC HASTA QUE REVISE EL GRADO RAMONEO DE 2012ª!!!
data <- subset(data, Año!=2012)
### AGREGAR GRADO DE RAMONEO A 2012 Y ELIMINAR ESTE SUBSET!!


# Standardize elevation and slope
n <-length(data$Altura)
for(i in 1:n) {
  data$std_elevation[i] <- (data$Altura[i] - mean(data$Altura))/sd(data$Altura)
} 
which(is.na(data$Altura))
which(is.na(data$Pendiente))
str(data$Pendiente)
for(i in 1:n) {
  data$std_slope[i] <- (data$Pendiente[i] - mean(data$Pendiente))/sd(data$Pendiente)
} 
# Generating and standardize square elevation and slope
data$sqr.Altura <- data$Altura^2
data$sqr.Pendiente <-data$Pendiente^2
for(i in 1:n) {
  data$std_sqr.elevation[i] <- (data$sqr.Altura[i] - mean(data$sqr.Altura))/sd(data$sqr.Altura)} 
for(i in 1:n) {
  data$std_sqr.slope[i] <- (data$sqr.Pendiente[i] - mean(data$sqr.Pendiente))/sd(data$sqr.Pendiente)} 

# Standardize grass and rock cover, bare soil, trunk presence, and distance to trails
for(i in 1:n) {
  data$std_rocks[i] <- (data$Roca[i] - mean(data$Roca))/sd(data$Roca)} 
for(i in 1:n) {
  data$std_grass[i] <- (data$Pasto[i] - mean(data$Pasto))/sd(data$Pasto)} 
for(i in 1:n) {
  data$std_soil[i] <- (data$Suelo.desnudo[i] - mean(data$Suelo.desnudo))/sd(data$Suelo.desnudo)}
for(i in 1:n) {
  data$std_distance[i] <- (data$Distancia.a.picada[i] - mean(data$Distancia.a.picada))/sd(data$Distancia.a.picada)} 

# Save subseted data 
write.csv(data, file= "parcelas_compilado_3.csv")
data <- read.csv("~/Desktop/field_signs/parcelas_compilado_3.csv") ## This has UTM coordinates
##################################################################################
#### MODELLING #####
##################################################################################

# We will fit two models:
# A poisson model for foraging and a Zero Inflated Poisson for fences counts

# POISSON In BUGS language
# for(i in 1:9){
#   ramoneo[i] ~ dbin(p[i], 4)
#   logit(p[i]) <- alpha[habitat_type[i]] + beta * elevation[i]
# }

str(data)
which(is.na(data$ramoneo))
which(is.na(data$bostas))

hist(data$bostas)
hist(na.omit(data$Roca), xlim=c(0,60))

# Poisson model for foraging (not adequate, just an approach )
primer_GR <- glm(ramoneo ~ Ambiente -1 + std_elevation + std_slope, 
              family = poisson, data=data)
summary(primer_GR)


primer_B <- glm(bostas ~ Ambiente -1 + std_elevation + std_slope,
              family = poisson, data=data)
summary(primer_B)

# Agrego variables cuadráticas
sqr_GR <- glm(ramoneo ~ Ambiente -1 + std_elevation + std_slope 
                 + std_sqr.elevation + std_sqr.slope , 
                 family = poisson, data=data)
summary(sqr_GR)


sqr_B <- glm(bostas ~ Ambiente -1 + std_elevation + std_slope 
                + std_sqr.elevation + std_sqr.slope , 
                family = poisson, data=data)
summary(sqr_B)


# Saco la pendiente al cuadrado que no aporta mucho
sqr.elev_GR <- glm(ramoneo ~ Ambiente -1 + std_elevation + std_slope + std_sqr.elevation, 
              family = poisson, data=data)
summary(sqr.elev_GR)


sqr.elev_B <- glm(bostas ~ Ambiente -1 + std_elevation + std_slope + std_sqr.elevation , 
             family = poisson, data=data)
summary(sqr.elev_B)

# Hago unos con todas las variables...
total_GR <- glm(ramoneo ~ Ambiente -1 + std_elevation + std_slope + std_grass 
              + std_soil + std_rocks + std_distance + std_sqr.elevation, 
              family = poisson, data=data)
summary(total_GR)


total_B <- glm(bostas ~ Ambiente -1 + std_elevation + std_slope + std_grass 
             + std_soil + std_rocks + std_distance + std_sqr.elevation,
             family = poisson, data=data)
summary(total_B)


AIC(primer_GR, sqr_GR, sqr.elev_GR, total_GR)
AIC(primer_B, sqr_B,  sqr.elev_B, total_B)


# Multinomial Model for foraging ####
#install.packages("nnet")
require(nnet)
ml$prog2 <- relevel(ml$prog, ref = "academic")
test <- multinom(prog2 ~ ses + write, data = ml)


n <- 1000
df1 <- data.frame(x1=runif(n,0,100),
                  x2=runif(n,0,100))
df1 <- transform(df1,
                 y=1+ifelse(100 - x1 - x2 + rnorm(n,sd=10) < 0, 0,
                            ifelse(100 - 2*x2 + rnorm(n,sd=10) < 0, 1, 2)),
                 set="Original")
head(df1)
str(df1)
df$col <- as.factor(df$y)

plot(df$x1, df$x2, asp=1, col=c("red","blue","green")[as.factor(df1$y)], 
     pch=20, cex=1.5) 
legend(x="topleft", legend = levels(as.factor(df1$y)), title="Grado de ramoneo",
       col=c("red","blue","green"), pch=20, pt.cex=1.5, cex=0.7)



# PLOT THE FORAGING LEVEL IN STUDY AREA ####
plot(data$Longitud, data$Latitud, asp=1, col=c("black","red","blue","green")[as.factor(data$ramoneo)], 
     pch=20, cex=1.5) 
legend(x="topleft", legend = levels(as.factor(data$ramoneo)), title="Grado de ramoneo",
       col=c("black", "red","blue","green"), pch=20, pt.cex=1.5, cex=0.8)


mod <- multinom(y ~ x1 + x2, df1)
summary(mod)
predict(mod)
predict(mod,df1,"probs")


str(data)

mod1 <- multinom(as.factor(ramoneo) ~ std_elevation + std_slope, data)
summary(mod1)
predict(mod1)
predict(mod1,data,"probs")


# Fitting the model as in Gelman
install.packages("arm")
require(arm)
fit1 <- bayespolr(as.factor(ramoneo) ~ std_slope, data)
summary(fit1)
??bayespolr
names(data)

fit2 <- bayespolr(as.factor(ramoneo) ~ Ambiente + std_slope, data)
summary(fit2)
coefplot(fit2)


fit3 <- bayespolr(as.factor(ramoneo) ~ Ambiente + std_slope + std_elevation +
                  std_rocks + std_grass + std_soil + std_distance  , data)
summary(fit3)
coefplot(fit3)
varnames = c("andino", "bosque", "mallín", "matorral", "quemado", "pendiente",
             "altitud2", "rocas", "forraje", "suelo", "sendero", "0|1","1|2","2|3")

coefplot(fit3, varnames=varnames)

### PARCELAS CON DATOS DE COMPOSICION PARCELAS ####
# Cargo datos que tienen riqueza de spp. y proporcion de plantas consumidas:
data_spp <- read.csv("~/Desktop/Workstation_Ecotono_SEPT2015/field_signs/parcelas_compilado_3POSTA.csv")

require(arm)

tail(data_spp [1:345,])
# Los corto porque tiene como un millon de filas vacías parece....
data_1 <- data_spp 
data_1 <- data_spp [1:345,]


# Veo los datos de riqueza específica de plantas:
names(data_1)
max(na.omit(data_1$proporcion))
hist(data_1$proporcion)
# par(mar=c(5,4,4,2))
hist(data_1$proporcion)

hist(data_1$Riqueza)
hist(data_1$Total_ramoneadas)

x.stand <- scale(data_1$x)
y.stand <- scale(data_1$y)

# Hago el modelo con las variables de vegetacion:

# Riqueza
fit4 <- bayespolr(as.factor(Grado.ramoneo) ~ Ambiente + std_slope + std_elevation +
                    std_rocks + std_grass + std_soil + std_distance + li +
                    x.stand + y.stand, data_1)
summary(fit4)
coefplot(fit4)
longnames <-names(fit4$coefficients)
sd.vec<-(summary(fit4)$coefficients)[1:14,2] # Desvios 
coefplot(fit4$coefficients, sd.vec, varnames=longnames)

hist(data_1$Grado.ramoneo)
table(data_1$Grado.ramoneo)
sum(table(data_1$Grado.ramoneo))
100-72*100/344

# Proporcion ramoneadas
fit5 <- bayespolr(as.factor(Grado.ramoneo) ~ Ambiente + std_slope + std_elevation +
                    std_rocks + std_grass + std_soil + std_distance + proporcion + li, data_1)
summary(fit5)
coefplot(fit5)

#Cobertura
fit6 <- bayespolr(as.factor(Grado.ramoneo) ~ Ambiente + std_slope + std_elevation +
                    std_rocks + std_grass + std_soil + std_distance + Cobertura.TOTAL , data_1)
summary(fit6)
coefplot(fit6)
longnames <-names(fit6$coefficients)
sd.vec<-(summary(fit6)$coefficients)[1:12,2] # Desvios 
coefplot(fit6$coefficients, sd.vec, varnames=longnames)


names(data)

#Total ramoneadas
fit7 <- bayespolr(as.factor(Grado.ramoneo) ~ Ambiente + std_slope + std_elevation +
                    std_rocks + std_grass + std_soil + std_distance + Total_ramoneadas , data_1)

str(summary(fit7)$coefficients)

class(summary(fit7)$coefficients)

(summary(fit7)$coefficients)[,1] # Coeficientes
(summary(fit7)$coefficients)[,2] # Desvios 
(summary(fit7)$coefficients)[,3]

coefplot(fit7)
str(fit7$coefficients)
longnames <-names(fit7$coefficients)
sd.vec<-c(0.9114,0.5466,0.9378,0.6231,0.6132,0.2063,0.1757,0.2489,0.1697,0.1873,0.1796,   0.1001 )

class((summary(fit7)$coefficients)[,2])
(summary(fit7)$coefficients)[1:12,2]

sd.vec<-(summary(fit7)$coefficients)[1:12,2] # Desvios 
coefplot(fit7$coefficients, sd.vec, varnames=longnames)

names(fit7)
?coefplot
fit7$zeta
fit7$deviance
fit7$fitted.values
fit7$lev
fit7$terms
fit7$df.residual
fit7$n
fit7$Hessian
fit7$xlevels
head(fit7$model)


#Altura del forraje
fit8 <- bayespolr(as.factor(Grado.ramoneo) ~ Ambiente + std_slope + std_elevation +
                    std_rocks + std_grass + std_soil + std_distance + Forraje..altura. , data_1)
summary(fit8)
coefplot(fit8)


# MODELO GRADO DE RAMONEO CON TODAS LAS VARIABLES VEGETACION

fit9 <- bayespolr(as.factor(Grado.ramoneo) ~ Ambiente + 
                    std_slope + std_elevation +
                    std_rocks + std_grass + std_soil + std_distance + #x.stand + y.stand +#proporcion + 
                    Cobertura.TOTAL + Riqueza + li, data_1)

summary(fit9)
coefplot(fit9)

longnames <-names(fit9$coefficients)
longnames <-c("altoandino","bosque","mallin","matorral", "quemado",
              "pendiente","altitud", 
              "% roca","% gramineas","% suelo desnudo", "distancia", #"spp ramoneadas", 
              #"componente x", "componente y", 
              "cobertura",
              "riqueza", "composición" #, "01", "12", "23"
              )


length(names(fit9$coefficients))
sd.vec<-(summary(fit9)$coefficients)[1:length(names(fit9$coefficients)),2] # Desvios 
coefplot(fit9$coefficients, sd.vec, varnames=longnames, main="Coeficientes estimados", lwd=4)



### DISENO DE MUESTERO ESPACIAL -> EQUAL STRATIFIED STRATEGY (Hirzel & Guisan 2002) #####
head(data_1)
class(data_1$Ambiente)
plot(data_1$Ambiente, cex.axis=1)
par(mar=c(5,4,4,2))

str(data_1$Ambiente)
levels(data_1$Ambiente)
table(data_1$Ambiente)
str(table(data_1$Ambiente))

class(table(data_1$Ambiente)[1])
table(data_1$Ambiente)[]

### VAMOS A ARMAR UNA TABLA DE PROPORCION DE AMBIENTES MUESTREADOS
sampl.design <-matrix(ncol=5, nrow=2)

colnames<-c("bosque", "mallin", "quemado", "matorral","altoandino")
colnames(sampl.design) <- colnames

rownames(sampl.design) <- c("prop.disp", "prop.muestra")
sampl.design

disp <- vector(mode="numeric",length=5)
class(disp)
disp
muestra <- vector(mode="numeric",length=5)

# Completamos el vector de hectareas disponibles por ambiente 

disp <- c(3600, 200, 1200, 1400, 1800)
barplot(disp)
sum(disp)
barplot(disp/sum(disp), col="lightgrey")

# Completamos la tabla de bosque muestreado
table(data_1$Ambiente)
muestra[1]<-table(data_1$Ambiente)[3]
muestra
muestra[2]<-table(data_1$Ambiente)[4]
muestra[3]<-table(data_1$Ambiente)[6] # el quemado es el 3ro en la tabla de disponibilidad
muestra[4]<-table(data_1$Ambiente)[5]
muestra[5]<-table(data_1$Ambiente)[1]+table(data_1$Ambiente)[2]
muestra
barplot(muestra/sum(muestra), density=3, add=T)
?barplot

sampl.design[1,] <-disp/sum(disp)
sampl.design[2,] <-muestra/sum(muestra)
sampl.design

barplot(sampl.design[1,], beside=T, col="lightgrey")
barplot(sampl.design[2,], density=3, add=T,beside=T, col="brown")


### PACKAGE CITATIONS in R #########
citation(package = "base", lib.loc = NULL) 


# Function to predict multinomial logit choice model outcomes
# model = nnet class multinomial model
# newdata = data frame containing new values to predict
predictMNL <- function(model, newdata) {
  
  # Only works for neural network models
  if (is.element("nnet",class(model))) {
    # Calculate the individual and cumulative probabilities
    probs <- predict(model,newdata,"probs")
    cum.probs <- t(apply(probs,1,cumsum))
    
    # Draw random values
    vals <- runif(nrow(newdata))
    
    # Join cumulative probabilities and random draws
    tmp <- cbind(cum.probs,vals)
    
    # For each row, get choice index.
    k <- ncol(probs)
    ids <- 1 + apply(tmp,1,function(x) length(which(x[1:k] < x[k+1])))
    
    # Return the values
    return(ids)
  }
}

# Make up some new data
n <- 200
df <- data.frame(x1=runif(n,0,100),
                 x2=runif(n,0,100),
                 set="Model")
y2 <- predictMNL(model=mod,newdata=df)
df2 <- cbind(df,y=y2)

str(df2)
plot(df2, df)



# Evaluate Collinearity
install.packages("Hmisc")
library(Hmisc)
str(data)
rcorr(data[38:47], type="pearson")

rcorr(as.matrix(data[38:47])) 

# install.packages("car")
library(car)
vif(primer_GR) # variance inflation factors
sqrt(vif(primer_GR)) > 2 # problem?

# Evaluate Nonlinearity
# component + residual plot
crPlots(primer_GR)
# Ceres plots
ceresPlots(primer_GR)


crPlots(total_GR)
# Ceres plots
ceresPlots(tota_GR)

##################################################################################
# ZERO-INFLATED POISSON

# ZIP in R
#install.packages("pscl")
library(pscl)
data<-data_spp

ZIP_bostas <- zeroinfl(bostas ~ Ambiente -1  + std_elevation + std_slope + std_grass 
               + std_soil + std_rocks + std_distance + std_sqr.elevation  | 1, 
               dist = "poisson", data=data)
summary(ZIP_bostas)

# Meto un par de variables de vegetacion..
ZIP_bostas_veg <- zeroinfl(bostas ~ Ambiente -1  + std_elevation + std_slope + std_grass 
                       + std_soil + std_rocks + std_distance + std_sqr.elevation  
                        + Cobertura.TOTAL +li | 1, 
                       dist = "poisson", data=data)
summary(ZIP_bostas_veg)

data$li

?cor
cor(na.omit(data$Riqueza), na.omit(data$proporcion), na.omit(data$Cobertura.TOTAL))
names(data)


ZIP_bostas_sin_amb <- zeroinfl(bostas ~ std_elevation + std_slope + std_grass 
                       + std_soil + std_rocks + std_distance + std_sqr.elevation | 1, 
                       dist = "poisson", data=data)
summary(ZIP_bostas_sin_amb)



# ZIP in BUGS 

data$ambiente.dummy <- as.numeric(data$Ambiente)
str(data$ambiente.dummy)

levels(data$Ambiente)
data$ambiente.dummy[1:30]
data$Ambiente[1:30]
### NOT EXECUTABLE CODE ####
# DUMMY VARIABLE FOR HABITATS 
1- ABIERTO
2- ALTO-ANDINO
3- FOREST
4- MALLIN
5- MATORRAL 
6- QUEMADO
##############################  
# Define model
sink("ZIP_3.txt")
cat("        model {

# Priors
    psi ~ dunif(0,1)
    alpha ~ dnorm(0,0.001)
    beta ~ dnorm(0,0.001)
    gamma ~ dnorm(0,0.001)
    delta ~ dnorm(0,0.001)
    epsilon ~ dnorm(0,0.001)
    theta ~ dnorm(0,0.001)
    eta ~ dnorm(0,0.001)
#    iota ~ dnorm(0,0.001)
    kappa ~ dnorm(0,0.001)

    # Likelihood
    for (i in 1:n) {
    w[i] ~ dbern(psi)
    C[i] ~ dpois(eff.lambda[i])
    eff.lambda[i] <- w[i] * lambda[i] + 0.00001
    log(lambda[i]) <- alpha + beta *elev[i] + gamma*slope[i] + delta*grass[i] + epsilon*soil[i] + theta*rocks[i] + eta*distance [i] +kappa*ambiente[i]
#+ iota*sqr.elev[i]
    }   
R.lpsi <- logit(1-psi)
}
    
    ", 
    fill=T
    )

sink()


# Bundle data
win.data <- list(C=data$bostas, elev=data$std_elevation,slope=data$std_slope, 
                 grass=data$std_grass, soil=data$std_soil, rocks=data$std_rocks,
                 distance=data$std_distance, sqr.elev= data$std_sqr.elevation,
                 n= nrow(data), ambiente=data$ambiente.dummy)


# Inits function
#?rlnorm
inits <- function(){ list(alpha=rlnorm(1), beta=rlnorm(1), gamma=rlnorm(1), delta=rlnorm(1),
                          epsilon=rlnorm(1), theta=rlnorm(1), eta=rlnorm(1), iota=rlnorm(1),
                          kappa=rlnorm(1),
                          w=rep(1, nrow(data)))}


# Parameters to estimate
# params <- c("lambda","alpha", "beta", "w", "psi", "R.lpsi")
# OBS: lambda es el estimador de conteo para cada sitio y w la suitability de cada uno
# si queremos ver sólo los parámetros de las covariables....
params <- c("alpha", "beta", "gamma", "delta", "epsilon", "theta", "eta", #"iota",
            "kappa",
            "psi", "R.lpsi")


# MCMC settings (need fairly long chains)
nc <- 3 # Number of chains
ni <- 10000 # Number of draws from posterior per chain
nb <- 1000 # Number of draws to discard as burn in
nt <- 2 # Thinning rate

# Cargamos la librería para trabajar con JAGS
#install.packages("jagsUI")
library(jagsUI)


# Corremos el MCMC en JAGS...
# SIN TIPOS DE HABITAT
out1 <- jags(data=win.data, inits=inits, parameters.to.save=params, "ZIP_1.txt", 
             n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
# CON TIPOS DE HABITAT
out2 <- jags(data=win.data, inits=inits, parameters.to.save=params, "ZIP_2.txt", 
             n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
# CON TIPOS DE HABITAT Y SIN SQR.ELEV
out3 <- jags(data=win.data, inits=inits, parameters.to.save=params, "ZIP_3.txt", 
             n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)


# Resumen de resultados
print(out1, digits=2)
print(out2, digits=2)


# ...o en WinBUGS
out <- bugs(data=win.data, inits, parameters.to.save=params, 
                       model.file="model.txt",n.chains=nc, n.iter=ni, 
                       n.burnin=nb,n.thin=nt,n.sims = 1000, debug=TRUE, 
                       DIC=TRUE, digits=2, bugs.directory="C:/Program Files/WinBUGS14/")

           




##################################################################################
# Partially interactive models ###### REVISAR A ESTO LE FALTA BASTANTE!!
attach(data)
DMO0 <- model.matrix(~Ambiente*std_slope-1-Ambiente)
head(DMO0)
summary(fm4 <- lm(ramoneo ~ DMO0-1))
coef(fm4)[6]

plot(std_slope, ramoneo, xlab="slope", xlim=c(-2,4), ylim=c(0,3.8))
abline(coef(fm4)[1],coef(fm4)[4], col="red", lwd=2)
abline(coef(fm4)[1],coef(fm4)[5], col="green", lwd=2)
abline(coef(fm4)[1],coef(fm4)[2], col="green", lwd=2)
abline(coef(fm4)[1],coef(fm4)[3], col="green", lwd=2)
abline(coef(fm4)[1],coef(fm4)[6], col="green", lwd=2)
##################################################################################


#### ABUNDANCE ESTIMATION FROM PELLETS COUNTS


# Determine sample sizes (spatial and temporal replication)
R <- 200
T <- 3
# Create structure to contain counts
y <- array(dim = c(R, T))
# Sample abundance from a Poisson(lambda = 2)
N <- rpois(n = R, lambda = 2)
# Sample counts from a Binomial(N, p = 0.5)
for (j in 1:T){
  y[,j] <- rbinom(n = R, size = N, prob = 0.5)
}
class(y)

# Look at realization of biological and observation processes
cbind(N, y)

# Specify model in BUGS language
sink("model_pellets.txt")
cat("
    model {

# Priors
    lambda ~ dgamma(0.005, 0.005)
    # lambda ~ dunif(0, 10)
    p ~ dunif(0, 1)
    
# Likelihood
    # Biological model for true abundance
    for (i in 1:R) {
    N[i] ~ dpois(lambda)

# Observation model for replicated counts
    for (j in 1:T) {
    y[i,j] ~ dbin(p, N[i])
    } #j
    } #i

# Derived quantities
totalN <- sum (N[])
    }
    ",fill = TRUE)
sink()

# Bundle data
win.data <- list(y = y, R = nrow(y), T = ncol(y))
# Initial values
Nst <- apply(y, 1, max) + 1 
inits <- function() list(N = Nst)
# Parameters monitored
params <- c("lambda", "p", "totalN")
# MCMC settings
ni <- 1200
nt <- 2
nb <- 200
nc <- 3


out <- jags(data=win.data, inits=inits, parameters.to.save=params, "model_pellets.txt", 
             n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)



# Resumen de resultados
print(out, digits=2)

data <- read.csv("~/Desktop/field_signs/N_mixture/revisited_data.csv")

str(data)

y <- data[9:10]
head(y)
class(y)
y <- as.matrix(y)
# Model in JAGS with a lot iterations...

# MCMC settings
ni <- 100000
nt <- 4
nb <- 5000
nc <- 3


out.huge <- jags(data=win.data, inits=inits, parameters.to.save=params, "model_pellets.txt", 
            n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
print(out.huge, digits=2)

plot(out.huge)





sink("model_cows_from_pellets.txt")
cat("
    model {
    
    # Priors
    lambda ~ dgamma(0.005, 0.005)
    # lambda ~ dunif(0, 10)
    p ~ dunif(0, 1)

mean.rate ~ dunif(0, 0.03) # Put a sensible value of defecation rate
#mean.rate ~ dnorm(0.01, 100)

# See Bagshaw (2002) for number of defecations per day in study plots 
# at landscape level within the home-range -> 0.1

sigma.proc ~ dunif(0.00007, 0.016)
tau.rate <- pow(sigma.proc, -2)
fecal.rate ~ dnorm(mean.rate, tau.rate)    

    # Likelihood

    # Biological model for true abundance of pellets
    for (i in 1:R) {
    N[i] ~ dpois(lambda)

        # State process of cows! not fences
#        for (t in 1:T){
#        fecal.rate[t] ~ dnorm(mean.rate, tau.rate)
#        N.est[t] <- N[i] * fecal.rate
#        }
    
############# AN ALTERNATIVE ########
# Estimated abundance as a derived quantity
    N.est[i] <- N[i] * fecal.rate
## Fecal rate as a constant?
####################################

    # Observation model for replicated counts
    for (j in 1:T) {
    y[i,j] ~ dbin(p, N[i])
    } #j
    } #i
    
    # Derived quantities
    totalN <- sum (N.est[])
    }
    ",fill = TRUE)
sink()

# Cargo el paquete de jags..
library(jagsUI)

# Estos datos tienen sólo las bostas nuevas
data<- read.csv("~/Desktop/Workstation_Ecotono_SEPT2015/field_signs/N_mixture/revisited_data_nuevas.csv")

# Y estos sólo las nuevas nuevas (SIN LAS ESTADO INTERMEDIO)
data<- read.csv("~/Desktop/Workstation_Ecotono_SEPT2015/field_signs/N_mixture/revisited_data_nuevas_2.csv")
y <- data[9:10]
head(y)
class(y)
y <- as.matrix(y)

#Bundle data
win.data <- list(y = y, R = nrow(y), T = ncol(y))
# Initial values
Nst <- apply(y, 1, max) + 1 
inits <- function() list(N = Nst)
# Parameters monitored
params <- c("lambda", "p", "totalN")
# MCMC settings
ni <- 1200
nt <- 2
nb <- 200
nc <- 3

setwd("/home/pc")
getwd()

out.state.space <- jags(data=win.data, inits=inits, parameters.to.save=params, 
                        "model_cows_from_pellets_AS.txt", 
                        n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)


# OJO !!!! MIRAR ESTE MODELO DE "_AS" PORQUE SE HABIA BORRADO LOS DE bernoulli (PSI)
# EU ESTA EN EL ANEXO DE LA TESIS 

totalN <- out.state.space$mean$totalN

# Resumen de resultados
print(out.state.space, digits=2)

area.total<- 500 # hectareas

vacas.por.ha <-function(area.total, totalN){
  vacas <- totalN/area.total
  return(vacas)
}
  
vacas.por.ha(area.total, totalN)

# 12.5 m2 -> 0.0125 Ha



### SENSITIVITY ANALYSIS ####

## Opción gráfica: Es la que vamos a usar en lugar de un análisis formal. 
# La idea es ver cuanto varía la variable de respuesta (Nro total de vacas)
# ante la variación en distintos parámetros del modelo.

# Tenemos que probar distintas variaciones en los parámetros dentro del rango
# y distintas opciones para graficarlo. Dos fáciles de interpretar son los scatterplots
# y los spider diagrams... 

## Vemos que onda el spider diagram de este paquete...
install.packages("psych")
library(psych)
spider(y=1,x=2:9,data=Thurstone) #same plot as a spider plot
head(Thurstone)
spider(y=1,x=2:9,data=Thurstone,connect=FALSE) #a radar plot
## NO NOS SIRVE!



### Veamos ahora en el paquete 'vegan'
install.packages("vegan")
library(vegan)
data(dune)
data(dune.env)
mod <- cca(dune ~ Management, dune.env)
attach(dune.env)
## pass non-graphical arguments without warnings
plot(mod, type="n", scaling = 3)
## Catch the invisible result of ordihull...
pl <- ordihull(mod, Management, scaling = 3, label = TRUE)
## ... and find centres and areas of the hulls
summary(pl)
ordispider(mod, col="red", scaling = 3)
plot(mod, type = "p", display="sites")
ordicluster(mod, hclust(vegdist(dune)), prune=3, col = "blue")
plot(mod, type="n", display = "sites")
text(mod, display="sites", labels = as.character(Management))
pl <- ordiellipse(mod, Management, kind="se", conf=0.95, lwd=2, col="blue")
### Esto tampoco NADA QUE VER!!!!!! 
### Vamos a hacerlo a mano me pa......



## A ver este paquete de gráficos 'plotly' está zarpado me parece..
install.packages("dplyr")
install.packages("plotly")
detach("package:psych", unload=TRUE)
library(dplyr)
library(plotly)

p <- ggplot2::mpg %>% group_by(class) %>%
  summarise(mn = mean(hwy), sd = 1.96 * sd(hwy)) %>%
  arrange(desc(mn)) %>%
  plot_ly(x = class, y = mn, error_y = list(array = sd),
          mode = "markers", name = "Highway") %>%
  layout(yaxis = list(title = "Miles Per Gallon"))
p
### NO FUNCIONA ESTE PAQUETE, tiene unos gráficos impresionantes, pero no 
### salen en esta compu..... :( 



### Listo... ANALISIS DE SENSIBILIDAD A MANOOO ####

# Recordar que necesitamos definida el área y la función de vacas/Ha.
area.total<- 500 # hectareas
vacas.por.ha <-function(area.total, totalN){
  vacas <- totalN/area.total
  return(vacas)}

### En esta especie de análisis de sensibilidad lo que vamos a hacer
### es ver cual es la sensibilidad del modelo a distintos valores previos 
### en los parámetros, para ver cual afecta más al nro. total de ganado

AS <- matrix(NA, nrow= 5, ncol=4)
rownames(AS) <- c("-100", "-50", "0", "50", "100")
colnames(AS) <- c("lambda", "p", "tasa", "sigma")
AS <-as.data.frame(AS)

vacas.por.ha(area.total, totalN) -> AS$sigma[3]

# MCMC settings
ni <- 50000
nt <- 2
nb <- 1000
nc <- 3

### STATE-SPACE BOSTAS SENSITIVITY ####
### Lo que vamos a hacer es correr el análisis 5 veces para cada parámetro
### modificando el archivo BUGS desde el editor de texto y almacenando los valores
### de vacas totales en la tabla anterior
out.state.space <- jags(data=win.data, inits=inits, parameters.to.save=params, 
                        "model_cows_from_pellets_AS.txt", 
                        n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
print(out.state.space, digits=2)
totalN <- out.state.space$mean$totalN
round(vacas.por.ha(area.total, totalN),5)



### Acá va un tandem de tasa completa.......
out.state.space1 <- jags(data=win.data, inits=inits, parameters.to.save=params, 
                        "model_cows_from_pellets_AS1.txt", 
                        n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
print(out.state.space1, digits=2)
totalN <- out.state.space1$mean$totalN
round(vacas.por.ha(area.total, totalN),5)

out.state.space2 <- jags(data=win.data, inits=inits, parameters.to.save=params, 
                        "model_cows_from_pellets_AS2.txt", 
                        n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
print(out.state.space2, digits=2)
totalN <- out.state.space2$mean$totalN
round(vacas.por.ha(area.total, totalN),5)

out.state.space3 <- jags(data=win.data, inits=inits, parameters.to.save=params, 
                        "model_cows_from_pellets_AS3.txt", 
                        n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
print(out.state.space3, digits=2)
totalN <- out.state.space3$mean$totalN
round(vacas.por.ha(area.total, totalN),5)

out.state.space4 <- jags(data=win.data, inits=inits, parameters.to.save=params, 
                        "model_cows_from_pellets_AS4.txt", 
                        n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
print(out.state.space4, digits=2)
totalN <- out.state.space4$mean$totalN
round(vacas.por.ha(area.total, totalN),5)

out.state.space5 <- jags(data=win.data, inits=inits, parameters.to.save=params, 
                        "model_cows_from_pellets_AS5.txt", 
                        n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
print(out.state.space5, digits=2)
totalN <- out.state.space5$mean$totalN
round(vacas.por.ha(area.total, totalN),5)

out.state.space6 <- jags(data=win.data, inits=inits, parameters.to.save=params, 
                        "model_cows_from_pellets_AS6.txt", 
                        n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
print(out.state.space6, digits=2)
totalN <- out.state.space6$mean$totalN
round(vacas.por.ha(area.total, totalN),5)

out.state.space7 <- jags(data=win.data, inits=inits, parameters.to.save=params, 
                        "model_cows_from_pellets_AS7.txt", 
                        n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
print(out.state.space7, digits=2)
totalN <- out.state.space7$mean$totalN
round(vacas.por.ha(area.total, totalN),5)

out.state.space8 <- jags(data=win.data, inits=inits, parameters.to.save=params, 
                        "model_cows_from_pellets_AS8.txt", 
                        n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
print(out.state.space8, digits=2)
totalN <- out.state.space8$mean$totalN
round(vacas.por.ha(area.total, totalN),5)

out.state.space9 <- jags(data=win.data, inits=inits, parameters.to.save=params, 
                        "model_cows_from_pellets_AS9.txt", 
                        n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
print(out.state.space9, digits=2)
totalN <- out.state.space9$mean$totalN
round(vacas.por.ha(area.total, totalN),5)

out.state.space10 <- jags(data=win.data, inits=inits, parameters.to.save=params, 
                        "model_cows_from_pellets_AS10.txt", 
                        n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
print(out.state.space10, digits=2)
totalN <- out.state.space10$mean$totalN
round(vacas.por.ha(area.total, totalN),5)





# GRAFICO ANALISIS DE SENSIBILIDAD POR DIAS ####
sens <- read.csv("~/Desktop/Workstation_Ecotono_SEPT2015/field_signs/N_mixture/sensibility_analysis.csv")
str(sens)
summary(sens)

par(mar=c(5,4,4,2))

plot(sens[,2], sens[,12], pch=15, ylim=c(0.001, 0.048), xlim=c(0.05,0.24), lty=6, 
     xlab="Tasa de deposición (fecas/ind.día.ha)", 
     ylab="Densidad de ganado estimada (ind/ha)",
     font.lab=2, cex.lab=1.2)
points(sens[,2],sens[,3], pch=21)
points(sens[,2],sens[,4], pch=16)
points(sens[,2],sens[,5], pch=22)
points(sens[,2],sens[,6], pch=17)
points(sens[,2],sens[,7], pch=23)
points(sens[,2],sens[,8], pch=18)
points(sens[,2],sens[,9], pch=24)
points(sens[,2],sens[,10], pch=19)
points(sens[,2],sens[,11], pch=25)


## ADD FITTED LINES TO THE PLOT

?lsfit

abline(lsfit(sens[,2],sens[,12]), lty=2, col="darkgrey", lwd=1.8)
abline(lsfit(sens[,2],sens[,3]), lty=2, col="darkgrey", lwd=1.8)
abline(lsfit(sens[,2],sens[,4]), lty=2, col="darkgrey", lwd=1.8)
abline(lsfit(sens[,2],sens[,5]), lty=2, col="darkgrey", lwd=1.8)
abline(lsfit(sens[,2],sens[,6]), lty=2, col="darkgrey", lwd=1.8)
abline(lsfit(sens[,2],sens[,7]), lty=2, col="darkgrey", lwd=1.8)
abline(lsfit(sens[,2],sens[,8]), lty=2, col="darkgrey", lwd=1.8)
abline(lsfit(sens[,2],sens[,9]), lty=2, col="darkgrey", lwd=1.8)
abline(lsfit(sens[,2],sens[,10]), lty=2, col="darkgrey", lwd=1.8)
abline(lsfit(sens[,2],sens[,11]), lty=2, col="darkgrey", lwd=1.8)

### ADD THE NUMBER OF DEPOSITION DAYS

text(0.235, 0.003, "1",font=4, cex=1.2, col="brown",adj = c(0,0))
text(0.235, 0.008, "2",font=4, cex=1.2, col="brown",adj = c(0,0))
text(0.235, 0.012, "3",font=4, cex=1.2, col="brown",adj = c(0,0))
text(0.235, 0.016, "4",font=4, cex=1.2, col="brown",adj = c(0,0))
text(0.235, 0.0195, "5",font=4, cex=1.2, col="brown",adj = c(0,0))
text(0.235, 0.0245, "6",font=4, cex=1.2, col="brown",adj = c(0,0))
text(0.235, 0.0305, "7",font=4, cex=1.2, col="brown",adj = c(0,0))
text(0.235, 0.0335, "8",font=4, cex=1.2, col="brown",adj = c(0,0))
text(0.235, 0.037, "9",font=4, cex=1.2, col="brown",adj = c(0,0))
text(0.235, 0.047, "10",font=4, cex=1.2, col="brown",adj = c(0,0))

#

#




######## SPATIAL AUTOCORRELATION ##########

install.packages("ape")
library(ape)

head(data)
data$bostas <- data$T1+data$T2
data.dists <- as.matrix(dist(cbind(data$Longitud, data$Latitud)))

data.dists.inv <- 1/data.dists
diag(data.dists.inv) <- 0
data.dists.inv[1:5, 1:5]
# Moran.I(data$bostas, data.dists.inv)

data.dists.bin <- (data.dists > 0 ) # & data.dists <= 600
Moran.I(data$bostas, data.dists.bin)

# No podemos rechazar la H0 de que hay zero correlation

# Hacemos un VARIOGRAMA
install.packages("geoR") 
library(geoR) 

head(data[,3:4])
dists <- dist(data[,3:4])
summary(dists)

str(data)
breaks = seq(0, 1.5, l = 11)
breaks
breaks = seq(0, 0.15, l = 20)
v1 <- variog(coords = data[,3:4], data = data[,29], breaks = breaks)
str(v1)
v1.summary <- cbind(c(1:19), v1$v, v1$n)
colnames(v1.summary) <- c("lag", "semi-variance", "# of pairs")

v1.summary

plot(v1, type = "b", main = "Variogram: pellet counts") 











