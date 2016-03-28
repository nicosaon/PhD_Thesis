# MAPPING THE FIELD PLOTS



data <- read.delim("~/Desktop/Master_Data_MSI/PARCELAS_master_data/parcelas_2015.csv")

x <- data$Longitud
y <- data$Latitud

plot(x,y, asp= 1, xlim=c(-71.7, -71.4))




