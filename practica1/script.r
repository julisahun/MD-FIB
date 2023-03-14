# Script used to preprocess data from GPUS.csv

################################# ENVIRONMENT ##################################

######### LIBRARIES ############
install.packages("tidyr")
install.packages('dplyr')
install.packages("naniar")  
install.packages("impute")  
install.packages("mice")  
library(tidyr)
library(dplyr)
library(naniar)
library(impute)
library(mice)

######### METHODS ############
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

fillWithMode <- function(gpus, column) {
  gpus[is.na(gpus[c(column)]), c(column)] <- getmode(gpus$column)
}

######### IMPORT DATA ############
path <- "/Users/mariamontalvofalcon/Desktop/MD-FIB/practica1/GPUS.csv"
gpus <- read.csv(path, header = T, sep = ",")
attach(gpus)
names(gpus)

############################ MISSING DATA TREATMENT ############################


# missing values come in difFerent shape, we set them all as "NA"
gpus[gpus == ''] <- NA
gpus[gpus == '\n-'] <- NA
gpus[gpus == '\n'] <- NA
gpus[gpus == '-'] <- NA
gpus[gpus == '\n- '] <- NA

# Visualisation of missing data
colSums(is.na(gpus)) / nrow(gpus)  
vis_miss(gpus)

########################### FEATURE DELETION ###########################

# Deleted columns: Boost_Clock, DisplayPort_Connection and Release_Price 
# because high percentage of NA

######### BOOST_CLOCK ############
sum(is.na(gpus$Boost_Clock)) # There's 1960 missing values
gpus <- select(gpus, -c("Boost_Clock"))

######### DISPLAYPORT_CONNECTION ############
sum(is.na(gpus$DisplayPort_Connection)) # There's 2549 missing values
gpus <- select(gpus, -c("DisplayPort_Connection"))

######### RELEASE_PRICE ############
sum(is.na(gpus$Release_Price)) # There's 2850 missing values --> delete column
gpus <- select(gpus, -c("Release_Price"))

########################################################################

######### ARCHITECTURE ############
sum(is.na(gpus$Architecture))
gpus[is.na(gpus$Architecture), c("Architecture")] <- "Unknown"


######### PORTS CONNECTIONS ############
# Imputation to 0 to ports connection that have a NA value
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
# We split the column of Best Resolution into two: 
#Best_Resolution_X and Best_Resolution_Y
gpus <- separate(data=gpus, col=Best_Resolution, into = c("Best_Resolution_X", "Best_Resolution_Y"), sep=" x ")
gpus[is.na(gpus[c("Best_Resolution_X")]), c("Best_Resolution_X")] <- -1
gpus[is.na(gpus[c("Best_Resolution_Y")]), c("Best_Resolution_Y")] <- -1
gpus$Best_Resolution_X <- as.numeric(gpus$Best_Resolution_X)
gpus$Best_Resolution_Y <- as.numeric(gpus$Best_Resolution_Y)
# els na els substituirem per la moda de la columna
gpus[-1 == (gpus[c("Best_Resolution_X")]), c("Best_Resolution_X")] <- median(gpus$Best_Resolution_X)
gpus[-1 == (gpus[c("Best_Resolution_Y")]), c("Best_Resolution_Y")] <- median(gpus$Best_Resolution_X)


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


aux <- gpus[-1 != (gpus[,c("Memory_Bandwidth")]),]
gpus[-1 == (gpus[c("Memory_Bandwidth")]), c("Memory_Bandwidth")] <- getmode(aux$Memory_Bandwidth)


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
sum(is.na(gpus$Memory_Type)) #23 nulls
gpus[is.na(gpus[c("Memory_Type")]), c("Memory_Type")] <- "Undefined"


######### NOTEBOOK_GPU ############
sum(is.na(gpus$Notebook_GPU)) #0 nulls
gpus$Notebook_GPU <- (gpus$Notebook_GPU == "Yes")


######### OPEN_GL ############
sum(is.na(gpus$Open_GL)) #22 nulls
#Mirar si inferim, si posem 0 o que fem


######### PIXEL_RATE ############
sum(is.na(gpus$Pixel_Rate)) #480 nulls
gpus$Pixel_Rate <- gsub(' GPixel/s','', gpus$Pixel_Rate)
gpus$Pixel_Rate <- as.numeric(gpus$Pixel_Rate)

aux <- gpus[!is.na(gpus[,c("Pixel_Rate")]),]
gpus[is.na(gpus[c("Pixel_Rate")]), c("Pixel_Rate")] <- getmode(aux$Pixel_Rate) #mirar que fem

######### PROCESS ############
sum(is.na(gpus$Process)) #402 nulls
gpus$Process <- gsub('nm','', gpus$Process)
gpus$Process <- as.numeric(gpus$Process)

aux <- gpus[!is.na(gpus[,c("Process")]),]
gpus[is.na(gpus[c("Process")]), c("Process")] <- getmode(aux$Process) #mirar que fem

######### ROPs ############
sum(is.na(gpus$ROPs)) #475 nulls
# te multiplicacio. Veure que fem

######### RELEASE_DATE ############
sum(is.na(gpus$Release_Date)) #0 nulls
gpus$Release_Date <- gsub(' ','', gpus$Release_Date)
dates <- as.Date(gpus$Release_Date, "%d-%b-%Y")
#gpus$Release_Date <- as.Date(gpus$Release_Date, "%d-%b-%Y") # no funciona. Ho posa a NA


######### SLI_CROSSFIRE ############
sum(is.na(gpus$SLI_Crossfire)) # 0 nulls
gpus$SLI_Crossfire <- (gpus$SLI_Crossfire == "Yes")


######### SHADER ############
sum(is.na(gpus$Shader)) # 89 nulls
aux <- gpus[!is.na(gpus[,c("Shader")]),]
gpus[is.na(gpus[c("Shader")]), c("Shader")] <- getmode(aux$Shader)

######### TMUs ############
sum(is.na(gpus$TMUs)) # 475 nulls
aux <- gpus[!is.na(gpus[,c("TMUs")]),]
gpus[is.na(gpus[c("TMUs")]), c("TMUs")] <- getmode(aux$TMUs) #mirar que fem

######### TEXTURE_RATE ############
sum(is.na(gpus$Texture_Rate)) # 480 nulls
gpus$Texture_Rate <- gsub(' GTexel/s','', gpus$Texture_Rate)
gpus$Texture_Rate <- as.numeric(gpus$Texture_Rate)

aux <- gpus[!is.na(gpus[,c("Texture_Rate")]),]
gpus[is.na(gpus[c("Texture_Rate")]), c("Texture_Rate")] <- getmode(aux$Texture_Rate) #mirar que fem


######### REALESE_DATE ############
# replace "Unknown Release Date" with NA
gpus$Release_Date <- ifelse(grepl("Unknown Release Date", gpus$Release_Date), NA, gpus$Release_Date)
imputed_values <- mice(gpus, method = "pmm", m = 5)
imputed_data <- complete(imputed_values)
imputed_data

# sum(grepl("Unknown Release Date", gpus$Release_Date))
# gpus$Release_Date <- ifelse(grepl("Unknown Release Date", gpus$Release_Date), NA, gpus$Release_Date)
# imputedValues <- knn(gpus[, c("Architecture", "Release_Date")], gpus[, c("Architecture", "Release_Date")], gpus$Release_Date, k = 3)
# gpus$Release_Date <- ifelse(is.na(gpus$Release_Date), imputedValues, gpus$Release_Date)



# SAVING THE DATASET PREPROCESSED
write.table(gpus, file = "preprocessed_GPUs.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
