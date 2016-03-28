# Script para graficar las trayectorias de todos los 
# animales que tengo con GPS hasta ahora


# 2013
# Graficamos primero este bicho porque tiene la 
# trayectoria más amplia

x <- read.table("C:/Users/nicosaon/Desktop/Winter_Desk/CAMPO_2014_2015/CAMPO_INVIERNO/COLLARES_INVIERNO/vaca_vila.csv", 
                header=T, sep= ",")

plot(x$Latitude,x$Longitude, type="o", col="yellow", 
      xlim= c(-41.425,-41.366), ylim=c(-71.587,-71.53),
      xlab="Latitude", ylab="Longitud", asp=1)
# El 'asp' es para que los ejes estén en la misma escala
# 4 meses de datos


# 2012
colorada_2012 <- read.csv("~/LARK_Desktop/vaquinha_windows/data/set_completo/posicionesGPS_COLORADA.txt")
buena_2012 <- read.csv("~/LARK_Desktop/vaquinha_windows/data/set_completo/posicionesGPS_BUENA.txt")
linda_2012 <- read.csv("~/LARK_Desktop/vaquinha_windows/data/set_completo/posicionesGPS_LINDA.txt")

# Hay un punto de colorada y uno de linda que tienen 
# un error en la posición del punto
colorada_2012[256,1] <--41.407
linda_2012[61,2] <- -71.542

lines(colorada_2012$lat, colorada_2012$long, type= "o", col="yellowgreen")
lines(buena_2012$lat, buena_2012$long, type= "o", col="blue")
lines(linda_2012$lat, linda_2012$long, type= "o", col="salmon")


# AÑO 2014

# Un mes de datos cada uno
bicho3 <- read.csv("C:/Users/nicosaon/Desktop/Winter_Desk/CAMPO_INVIERNO/COLLARES_INVIERNO/GPS_3_inverno/07_08_14_hasta_06_09_14.csv")
bicho1 <- read.csv("C:/Users/nicosaon/Desktop/Winter_Desk/CAMPO_INVIERNO/COLLARES_INVIERNO/GPS_1_inverno/07_08_14_to_05_09_14_gps1.csv")
bicho2 <- read.csv("C:/Users/nicosaon/Desktop/Winter_Desk/CAMPO_INVIERNO/COLLARES_INVIERNO/GPS_2_inverno/07_08_14_hasta_05_09_14.csv")
bicho5 <- read.csv("C:/Users/nicosaon/Desktop/Winter_Desk/CAMPO_INVIERNO/COLLARES_INVIERNO/GPS_5_inverno/07_08_14_hasta_11_09_14_gps5.csv")


# Estos de acá abajo son dos días de datos nomás...
bicho6 <- read.csv("C:/Users/nicosaon/Desktop/Winter_Desk/CAMPO_INVIERNO/COLLARES_INVIERNO/GPS_6_inverno/07_08_14_hasta_09_08_14.csv")
bicho4 <- read.csv("C:/Users/nicosaon/Desktop/Winter_Desk/CAMPO_INVIERNO/COLLARES_INVIERNO/GPS_SN_inverno/07_08_14_hasta_13_08_14_SN.csv")

# Datos de bichos colectados durante el voluntariado

VOL_1 <- read.csv("C:/Users/nicosaon/Desktop/Winter_Desk/CAMPO_2014_2015/CAMPO_INVIERNO/COLLARES_INVIERNO/GPS_VOL_2015_Doradilla/doradilla_primavera.csv")

lines(bicho5$Latitude, bicho5$Longitude, type= "o", col="springgreen2")
lines(bicho3$Latitude, bicho3$Longitude, type= "o")
lines(bicho1$Latitude, bicho1$Longitude, type= "o", col="peru")
lines(bicho2$Latitude, bicho2$Longitude, type= "o", col="red")


lines(bicho4$Latitude, bicho4$Longitude, type= "o", col="aquamarine4")
lines(bicho6$Latitude, bicho6$Longitude, type= "o", col="violetred")

lines(VOL_1$Latitude, VOL_1$Longitude, type= "o", col="red", asp=1)

lines(x$Longitude, x$Latitude, type= "o", col="yellow", asp=1)

plot(VOL_1$Longitude, VOL_1$Latitude,type="o", col="red", 
     #xlim= c(-41.425,-41.366), ylim=c(-71.587,-71.53),
     xlab="Latitude", ylab="Longitud", asp=1)

colours()




