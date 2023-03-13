# Script used to preprocess data from GPUS.csv
######### LIBRARIES ############
install.packages("tidyr")
install.packages('dplyr')
library(tidyr)
library(dplyr)

######### METHODS ############
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

fillWithMode <- function(dd, column) {
  dd[is.na(dd[c(column)]), c(column)] <- getmode(dd$column)
}
path <- "C:/Users/Cris/Desktop/UNI/MD/MD/practica1/GPUS.csv"

gpus <- read.csv(path, header = T, sep = ",")
attach(gpus)


#Missing data treatment

#Detect
names(gpus)

# missing values come in diferent shape, we set them all as "NA"
gpus[gpus == ''] <- NA
gpus[gpus == '\n-'] <- NA
gpus[gpus == '\n'] <- NA
gpus[gpus == '-'] <- NA
gpus[gpus == '\n- '] <- NA



######### ARCHITECTURE ############
# few missing values in Architecture column so, we decided to remove rows that haven't a value to Architecture feature
dim(gpus[gpus[c("Architecture")] == 0,])
gpus <- gpus[!is.na(gpus[,1]),]


######### PORTS CONNECTIONS ############
# Imputation to 0 to ports connection that have a NA value
gpus["DisplayPort_Connection"][is.na(gpus["DisplayPort_Connection"])] <- 0
gpus[is.na(gpus[c("HDMI_Connection")]), c("HDMI_Connection")] <- 0
gpus[is.na(gpus[c("VGA_Connection")]), c("VGA_Connection")] <- 0
gpus[is.na(gpus[c("DVI_Connection")]), c("DVI_Connection")] <- 0


######### INTEGRATED I DEDICATED ############
# We remove instances of GPUS that has an NA value in dedicated column
gpus <- gpus[!is.na(gpus[,c("Dedicated")]),]
# Transformation of columns Dedicated and Integrated to boolean types
gpus$Dedicated <- (gpus$Dedicated == "Yes")
gpus$Integrated <- (gpus$Integrated == "Yes")


######### BEST_RESOLUTION ############
# We split the column of Best Resolution into two: Best_Resolution_W and Best_Resolution_H
gpus <- separate(data=gpus, col=Best_Resolution, into = c("Best_Resolution_X", "Best_Resolution_Y"), sep=" x ")
gpus[is.na(gpus[c("Best_Resolution_X")]), c("Best_Resolution_X")] <- -1
gpus[is.na(gpus[c("Best_Resolution_Y")]), c("Best_Resolution_Y")] <- -1
gpus$Best_Resolution_X <- as.numeric(gpus$Best_Resolution_X)
gpus$Best_Resolution_Y <- as.numeric(gpus$Best_Resolution_Y)
# els na els substituirem per la moda de la columna
gpus[-1 == (gpus[c("Best_Resolution_X")]), c("Best_Resolution_X")] <- median(gpus$Best_Resolution_X)
gpus[-1 == (gpus[c("Best_Resolution_Y")]), c("Best_Resolution_Y")] <- median(gpus$Best_Resolution_X)

######### BOOST_CLOCK ############
sum(is.na(gpus$Boost_Clock)) # There's 1893 missing values --> delete column
gpus <- select(gpus, -c("Boost_Clock"))

######### CORE_SPEED ############
sum(is.na(gpus$Core_Speed))
gpus$Core_Speed <- gsub(' MHz','', gpus$Core_Speed)
gpus$Core_Speed <- as.numeric(gpus$Core_Speed)

aux <- gpus[!is.na(gpus[,c("Core_Speed")]),]
gpus[is.na(gpus[c("Core_Speed")]), c("Core_Speed")] <- getmode(aux$Core_Speed)


######### DIRECT_X ############
sum(is.na(gpus$Direct_X))
gpus[is.na(gpus$Direct_X), c("Direct_X")] <- "Unknown"


######### MAX_POWER  ############
sum(is.na(gpus$Max_Power)) #574 nulls
gpus$Max_Power <- gsub(' Watts','', gpus$Max_Power)
gpus$Max_Power <- as.numeric(gpus$Max_Power)

aux <- gpus[!is.na(gpus[,c("Max_Power")]),]
gpus[is.na(gpus[c("Max_Power")]), c("Max_Power")] <- getmode(aux$Max_Power)


######### MEMORY  ############
sum(is.na(gpus$Memory)) #376 nulls
gpus$Memory <- gsub(' MB','', gpus$Memory)
gpus$Memory <- as.numeric(gpus$Memory)

aux <- gpus[!is.na(gpus[,c("Memory")]),]
gpus[is.na(gpus[c("Memory")]), c("Memory")] <- getmode(aux$Memory)


######### MANUFACTURER  ############
sum(is.na(gpus$Manufacturer))
length(unique(gpus$Manufacturer))


######### MEMORY_BRANDWIDTH ############
sum(is.na(gpus$Memory_Bandwidth)) #82 nulls
aux <- gpus
gpus$Memory_Bandwidth <- gsub('GB/sec','', gpus$Memory_Bandwidth)
gpus$Memory_Bandwidth <- as.numeric(gpus$Memory_Bandwidth)

# Crear un vector de ejemplo
tamanos <- c("18.00MB", "2.50GB", "120.00MB", "500GB", "1.25GB")
aux$Memory_Bandwidth <- convertir_a_GB(aux$Memory_Bandwidth)

# Función para convertir tamaños de archivo a GB
convertir_a_GB <- function(tamanos) {
  # Eliminar el texto "MB" o "GB" y convertir a números
  numeros <- as.numeric(gsub("GB/sec", "", tamanos))
  numeros <- as.numeric(gsub("MB/sec", "", numeros))
  #aux2$Memory_Bandwidth <- gsub('GB/sec','', aux2$Memory_Bandwidth)
  #aux2$Memory_Bandwidth <- gsub('MB/sec','', aux2$Memory_Bandwidth)
  # Multiplicar por 1024 si es MB, dejar como está si es GB
  tamanos_en_GB <- ifelse(grepl("MB", tamanos, fixed=TRUE), numeros/1024, numeros)
  # Redondear a 2 decimales
  tamanos_en_GB <- round(tamanos_en_GB, 2)
  return(tamanos_en_GB)
}

# Aplicar la función al vector de ejemplo
convertir_a_GB(tamanos)


aux <- dd[-1 != (dd[,c("Memory_Bandwidth")]),]
dd[-1 == (dd[c("Memory_Bandwidth")]), c("Memory_Bandwidth")] <- getmode(aux$Memory_Bandwidth)


######### MEMORY_BUS ############
sum(is.na(gpus$Memory_Bus)) #30 nulls
gpus$Memory_Bus <- gsub(' Bit','', gpus$Memory_Bus)
gpus$Memory_Bus <- as.numeric(gpus$Memory_Bus)

aux <- gpus[!is.na(gpus[,c("Memory_Bus")]),]
gpus[is.na(gpus[c("Memory_Bus")]), c("Memory_Bus")] <- getmode(aux$Memory_Bus)


######### MEMORY_SPEED ############
sum(is.na(gpus$Memory_Speed)) #73 nulls
gpus$Memory_Speed <- gsub(' MHz','', gpus$Memory_Speed)
gpus$Memory_Speed <- as.numeric(gpus$Memory_Speed)

aux <- gpus[!is.na(gpus[,c("Memory_Speed")]),]
gpus[is.na(gpus[c("Memory_Speed")]), c("Memory_Speed")] <- getmode(aux$Memory_Speed)


######### MEMORY_TYPE ############
sum(is.na(dd$Memory_Type)) #23 nulls
dd[is.na(dd[c("Memory_Type")]), c("Memory_Type")] <- "Undefined"


######### NOTEBOOK_GPU ############
sum(is.na(dd$Notebook_GPU)) #0 nulls
dd$Notebook_GPU <- (dd$Notebook_GPU == "Yes")


######### OPEN_GL ############
sum(is.na(dd$Open_GL)) #22 nulls
#Mirar si inferim, si posem 0 o que fem


######### PIXEL_RATE ############
sum(is.na(dd$Pixel_Rate)) #480 nulls
dd$Pixel_Rate <- gsub(' GPixel/s','', dd$Pixel_Rate)
dd$Pixel_Rate <- as.numeric(dd$Pixel_Rate)

aux <- dd[!is.na(dd[,c("Pixel_Rate")]),]
dd[is.na(dd[c("Pixel_Rate")]), c("Pixel_Rate")] <- getmode(aux$Pixel_Rate) #mirar que fem

write.table(gpus, file = "credscoClean.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

