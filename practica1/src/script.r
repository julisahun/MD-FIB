# Script used to preprocess data from GPUS.csv

################################# ENVIRONMENT ##################################

######### LIBRARIES ############
library(tidyr)
library(dplyr)
library(naniar)
library(readr)
library(class)

######### METHODS ############
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

######### IMPORT DATA ############
path <- "/Users/mariamontalvofalcon/Desktop/MD-FIB/practica1/data/GPUS.csv"
gpus <- read.csv(path, header = T, sep = ",")
attach(gpus)
names(gpus)

############################ MISSING DATA TREATMENT ############################


# missing values come in different shape, we set them all as "NA"
gpus[gpus == ''] <- NA
gpus[gpus == '\n-'] <- NA
gpus[gpus == '\n'] <- NA
gpus[gpus == '-'] <- NA
gpus[gpus == '\n- '] <- NA

# Visualisation of missing data
aux <- gpus["Yes"==(gpus[,c("Dedicated")]),]
vis_miss(aux)

colSums(is.na(gpus)) / nrow(gpus)  
vis_miss(gpus)


########################### FEATURE DELETION ###########################

# Deleted columns: Boost_Clock, DisplayPort_Connection and Release_Price 
# because high percentage of NA

#----------- BOOST_CLOCK -----------
sum(is.na(gpus$Boost_Clock)) # There's 1960 missing values
gpus <- select(gpus, -c("Boost_Clock"))

#----------- DISPLAYPORT_CONNECTION -----------
sum(is.na(gpus$DisplayPort_Connection)) # There's 2549 missing values
gpus <- select(gpus, -c("DisplayPort_Connection"))

#----------- RELEASE_PRICE -----------
sum(is.na(gpus$Release_Price)) # There's 2850 missing values --> delete column
gpus <- select(gpus, -c("Release_Price"))

########################################################################

########################## FEATURE IMPUTATION ##########################

#----------- ARCHITECTURE -----------
sum(is.na(gpus$Architecture))
gpus[is.na(gpus$Architecture), c("Architecture")] <- "Unknown"


#----------- INTEGRATED I DEDICATED -----------
# Transformation of columns Dedicated and Integrated to boolean types
gpus$Dedicated <- (gpus$Dedicated == "Yes")
gpus$Integrated <- (gpus$Integrated == "Yes")

summary(gpus$Dedicated)
summary(gpus$Integrated)
barplot(table(gpus$Dedicated),main="Bar plot Dedicated Variable",
        xlab = "# of ports", ylab ="# of instances",las=1)
barplot(table(gpus$Integrated),main="Bar plot Integrated Variable",
        xlab = "# of ports", ylab ="# of instances",las=1)

# We remove instances of GPUS that has an NA value in dedicated column
gpus <- gpus[!is.na(gpus[,c("Dedicated")]),]


summary(gpus$Dedicated)
summary(gpus$Integrated)


#----------- BEST_RESOLUTION -----------
# We split the column of Best Resolution into two: 
#Best_Resolution_X and Best_Resolution_Y
gpus <- separate(data=gpus, col=Best_Resolution, into = c("Best_Resolution_X", "Best_Resolution_Y"), sep=" x ")
gpus$Best_Resolution_X <- as.numeric(gpus$Best_Resolution_X)
gpus$Best_Resolution_Y <- as.numeric(gpus$Best_Resolution_Y)
# els na els substituirem per la moda de la columna

aux <- gpus[!is.na(gpus[,c("Best_Resolution_X")]),]
gpus[is.na(gpus[c("Best_Resolution_X")]), c("Best_Resolution_X")] <- getmode(aux$Best_Resolution_X)
aux <- gpus[!is.na(gpus[,c("Best_Resolution_Y")]),]
gpus[is.na(gpus[c("Best_Resolution_Y")]), c("Best_Resolution_Y")] <- getmode(aux$Best_Resolution_Y)


#----------- CORE_SPEED -----------
sum(is.na(gpus$Core_Speed))
gpus$Core_Speed <- gsub(' MHz','', gpus$Core_Speed)
gpus$Core_Speed <- as.numeric(gpus$Core_Speed)

aux <- gpus[!is.na(gpus[,c("Core_Speed")]),]
gpus[is.na(gpus[c("Core_Speed")]), c("Core_Speed")] <- mean(aux$Core_Speed)


#----------- DIRECT_X -----------
sum(is.na(gpus$Direct_X))
gpus[is.na(gpus$Direct_X), c("Direct_X")] <- "Unknown"


#----------- MAX_POWER  -----------
sum(is.na(gpus$Max_Power)) #574 nulls
gpus$Max_Power <- gsub(' Watts','', gpus$Max_Power)
gpus$Max_Power <- as.numeric(gpus$Max_Power)

aux <- gpus[!is.na(gpus[,c("Max_Power")]),]
gpus[is.na(gpus[c("Max_Power")]), c("Max_Power")] <- getmode(aux$Max_Power)


#----------- MEMORY -----------
sum(is.na(gpus$Memory)) #376 nulls
gpus$Memory <- gsub(' MB','', gpus$Memory)
gpus$Memory <- as.numeric(gpus$Memory)

aux <- gpus[!is.na(gpus[,c("Memory")]),]
gpus[is.na(gpus[c("Memory")]), c("Memory")] <- getmode(aux$Memory)


#----------- MANUFACTURER -----------
sum(is.na(gpus$Manufacturer))
length(unique(gpus$Manufacturer))

#----------- L2_CACHE -----------
sum(is.na(gpus$L2_Cache)) #0 nulls
aux <- separate(data=gpus, col=L2_Cache, into = c("L2_Cache_Num", "L2_Cache_Mult"), sep="KB")
aux[""==(aux[,c("L2_Cache_Mult")]), c("L2_Cache_Mult")] <-"1"
aux$L2_Cache_Mult <- gsub('\\(','',gsub('\\)','',gsub('x','', aux$L2_Cache_Mult)))
aux$L2_Cache_Mult <- as.numeric(aux$L2_Cache_Mult)
aux$L2_Cache_Num <- as.numeric(aux$L2_Cache_Num)
gpus$L2_Cache <- aux$L2_Cache_Num * aux$L2_Cache_Mult

boxplot(table(gpus$L2_Cache), # Datos
        horizontal = FALSE, # Horizontal o vertical
        lwd = 2, # Lines width
        main = "Boxplot L2 Cache", # Título
        notch = FALSE, # Añade intervalos de confianza para la mediana
        border = "black",  # Color del borde del boxplot
        outpch = 25,       # Símbolo para los outliers
        outbg = "red",   # Color de los datos atípicos
        whisklty = 2,      # Tipo de línea para los bigotes
        lty = 1) # Tipo de línea (caja y mediana)
stripchart(gpus$L2_Cache, method = "jitter", pch = 19, add = TRUE, col = "blue")

#----------- MEMORY_BANDWIDTH -----------
sum(is.na(gpus$Memory_Bandwidth)) #82 nulls
gpus$Memory_Bandwidth <- gsub('GB/sec','', gpus$Memory_Bandwidth)
gpus$Memory_Bandwidth <- as.numeric(gpus$Memory_Bandwidth)

aux <- gpus[!is.na(gpus[,c("Memory_Bandwidth")]),]
gpus[is.na (gpus[c("Memory_Bandwidth")]), c("Memory_Bandwidth")] <- getmode(aux$Memory_Bandwidth) 


#----------- MEMORY_BUS -----------
sum(is.na(gpus$Memory_Bus)) #30 nulls
gpus$Memory_Bus <- gsub(' Bit','', gpus$Memory_Bus)
gpus$Memory_Bus <- as.numeric(gpus$Memory_Bus)

aux <- gpus[!is.na(gpus[,c("Memory_Bus")]),]
gpus[is.na(gpus[c("Memory_Bus")]), c("Memory_Bus")] <- getmode(aux$Memory_Bus)


#----------- MEMORY_SPEED -----------
sum(is.na(gpus$Memory_Speed)) #73 nulls
gpus$Memory_Speed <- gsub(' MHz','', gpus$Memory_Speed)
gpus$Memory_Speed <- as.numeric(gpus$Memory_Speed)

aux <- gpus[!is.na(gpus[,c("Memory_Speed")]),]
gpus[is.na(gpus[c("Memory_Speed")]), c("Memory_Speed")] <- mean(aux$Memory_Speed)


#----------- MEMORY_TYPE -----------
sum(is.na(gpus$Memory_Type)) #23 nulls
gpus[is.na(gpus[c("Memory_Type")]), c("Memory_Type")] <- "Unknown"


#----------- NOTEBOOK_GPU -----------
sum(is.na(gpus$Notebook_GPU)) #0 nulls
gpus$Notebook_GPU <- (gpus$Notebook_GPU == "Yes")


#----------- OPEN_GL -----------
sum(is.na(gpus$Open_GL)) #36 nulls
gpus[is.na(gpus[c("Open_GL")]), c("Open_GL")] <- "Unknown"


#----------- PSU -----------
sum(is.na(gpus$PSU)) #1174 nulls
gpus <- separate(data=gpus, col=PSU, into = c("PSU_Watt", "PSU_Aamps"), sep=" & ") 
#WATTS
sum(is.na(gpus$PSU_Watt)) #1174 nulls
gpus$PSU_Watt <- gsub(' ','',gsub('Watt','', gpus$PSU_Watt))
gpus$PSU_Watt <- as.numeric(gpus$PSU_Watt)
aux <- gpus[!is.na(gpus[,c("PSU_Watt")]),]
gpus[is.na(gpus[c("PSU_Watt")]), c("PSU_Watt")] <- getmode(aux$PSU_Watt) 
#AMPS
sum(is.na(gpus$PSU_Aamps)) #1508 nulls
gpus$PSU_Aamps <- gsub(' ','',gsub('Amps','', gpus$PSU_Aamps))
gpus$PSU_Aamps <- as.numeric(gpus$PSU_Aamps)
aux <- gpus[!is.na(gpus[,c("PSU_Aamps")]),]
gpus[is.na(gpus[c("PSU_Aamps")]), c("PSU_Aamps")] <- getmode(aux$PSU_Aamps)


#----------- PIXEL_RATE -----------
sum(is.na(gpus$Pixel_Rate)) #480 nulls
gpus$Pixel_Rate <- gsub(' GPixel/s','', gpus$Pixel_Rate)
gpus$Pixel_Rate <- as.numeric(gpus$Pixel_Rate)

aux <- gpus[!is.na(gpus[,c("Pixel_Rate")]),]
gpus[is.na(gpus[c("Pixel_Rate")]), c("Pixel_Rate")] <- getmode(aux$Pixel_Rate)

#----------- POWER_CONNECTOR -----------
sum(is.na(gpus$Power_Connector)) # 709 nulls
gpus[is.na(gpus[c("Power_Connector")]), c("Power_Connector")] <- "Unknown"

#----------- PROCESS -----------
sum(is.na(gpus$Process)) #461 nulls
gpus$Process <- gsub('nm','', gpus$Process)
gpus$Process <- as.numeric(gpus$Process)

aux <- gpus[!is.na(gpus[,c("Process")]),]
gpus[is.na(gpus[c("Process")]), c("Process")] <- getmode(aux$Process)

#----------- ROPs -----------
sum(is.na(gpus$ROPs)) #475 nulls
aux <- separate(data=gpus, col=ROPs, into = c("ROPs_Num", "ROPs_Mult"), sep=" ")
aux[is.na(aux[,c("ROPs_Mult")]), c("ROPs_Mult")] <-"1"
aux$ROPs_Mult <- gsub('\\(','',gsub('\\)','',gsub('x','', aux$ROPs_Mult)))
aux$ROPs_Mult <- as.numeric(aux$ROPs_Mult)
aux$ROPs_Num <- as.numeric(aux$ROPs_Num)
gpus$ROPs <- aux$ROPs_Num * aux$ROPs_Mult

aux <- gpus[!is.na(gpus[,c("ROPs")]),]
gpus[is.na(gpus[c("ROPs")]), c("ROPs")] <- getmode(aux$ROPs) 




#----------- RESOLUTION_WXH -----------
# We split the column of Best Resolution into two: 
#Resolution_W and Resolution_H
gpus <- separate(data=gpus, col=Resolution_WxH, into = c("Resolution_W", "Resolution_H"), sep="x")
gpus$Resolution_W <- as.numeric(gpus$Resolution_W)
gpus$Resolution_H <- as.numeric(gpus$Resolution_H)
# els na els substituirem per la moda de la columna

aux <- gpus[!is.na(gpus[,c("Resolution_W")]),]
gpus[is.na(gpus[c("Resolution_W")]), c("Resolution_W")] <- getmode(aux$Resolution_W)
aux <- gpus[!is.na(gpus[,c("Resolution_H")]),]
gpus[is.na(gpus[c("Resolution_H")]), c("Resolution_H")] <- getmode(aux$Resolution_H)


#----------- SLI_CROSSFIRE -----------
sum(is.na(gpus$SLI_Crossfire)) # 0 nulls
gpus$SLI_Crossfire <- (gpus$SLI_Crossfire == "Yes")


#----------- SHADER -----------
sum(is.na(gpus$Shader)) # 98 nulls
gpus[is.na(gpus[c("Shader")]), c("Shader")] <- "Unknown"

#----------- TMUs -----------
sum(is.na(gpus$TMUs)) # 475 nulls
aux <- gpus[!is.na(gpus[,c("TMUs")]),]
gpus[is.na(gpus[c("TMUs")]), c("TMUs")] <- getmode(aux$TMUs) #mirar que fem

#----------- TEXTURE_RATE -----------
sum(is.na(gpus$Texture_Rate)) # 480 nulls
gpus$Texture_Rate <- gsub(' GTexel/s','', gpus$Texture_Rate)
gpus$Texture_Rate <- as.numeric(gpus$Texture_Rate)

aux <- gpus[!is.na(gpus[,c("Texture_Rate")]),]
gpus[is.na(gpus[c("Texture_Rate")]), c("Texture_Rate")] <- getmode(aux$Texture_Rate) #mirar que fem

#----------- REALESE_DATE -----------
# replace "Unknown Release Date" with NA
gpus$Release_Date <- ifelse(grepl("Unknown Release Date", gpus$Release_Date), NA, gpus$Release_Date)
gpus$Release_Date <- gsub('-','/',gsub(' ','',gsub('Dec','12',gsub('Nov','11',gsub('Oct','10',gsub('Sep','09',gsub('Aug','08',gsub('Jul','07',gsub('Jun','06',gsub('May','05',gsub('Apr','04',gsub('Mar','03', gsub('Feb','02',gsub('Jan','01',gpus$Release_Date))))))))))))))
tmp <- parse_datetime(gpus$Release_Date, format="%d/%m/%Y")
gpus$Release_Date <- as.Date(tmp)
aux <- gpus[!is.na(gpus[,c("Release_Date")]),]
gpus[is.na(gpus[c("Release_Date")]), c("Release_Date")] <- getmode(aux$Release_Date)


#----------- PORTS CONNECTIONS -----------
sum(is.na(gpus$HDMI_Connection))
sum(is.na(gpus$DVI_Connection))
sum(is.na(gpus$VGA_Connection))

summary(gpus$HDMI_Connection)
summary(gpus$DVI_Connection)
summary(gpus$VGA_Connection)


barplot(table(gpus$HDMI_Connection),main="Bar plot HDMI Connection Variable",
        xlab = "# of ports", ylab ="# of instances",las=1)
barplot(table(gpus$DVI_Connection),main="Bar plot DVI Connection Variable",
        xlab = "# of ports", ylab ="# of instances",las=1)
barplot(table(gpus$VGA_Connection),main="Bar plot VGA Connection Variable",
        xlab = "# of ports", ylab ="# of instances",las=1)


uncompleteVars<-c(5,8,34)
fullVariables<-c(2, 3, 4, 10, 12, 13, 14, 15, 16, 21, 22, 23, 25, 28, 29, 32, 33)
aux<-gpus[,fullVariables]
dim(aux)
names(aux)

for (k in uncompleteVars){
  aux1 <- aux[!is.na(gpus[,k]),]
  dim(aux1) 
  aux2 <- aux[is.na(gpus[,k]),]
  dim(aux2)
  
  RefValues<- gpus[!is.na(gpus[,k]),k]
  knn.values = knn(aux1,aux2,RefValues)   
  
  gpus[is.na(gpus[,k]),k] = as.numeric(as.character(knn.values))
  fullVariables<-c(fullVariables, k)
  aux<-gpus[,fullVariables]
}

sum(is.na(gpus$HDMI_Connection))
sum(is.na(gpus$DVI_Connection))
sum(is.na(gpus$VGA_Connection))
barplot(table(gpus$HDMI_Connection),main="Bar plot HDMI Connection Variable",
        xlab = "# of ports", ylab ="# of instances",las=1)
barplot(table(gpus$DVI_Connection),main="Bar plot DVI Connection Variable",
        xlab = "# of ports", ylab ="# of instances",las=1)
barplot(table(gpus$VGA_Connection),main="Bar plot VGA Connection Variable",
        xlab = "# of ports", ylab ="# of instances",las=1)


summary(gpus$HDMI_Connection)
summary(gpus$DVI_Connection)
summary(gpus$VGA_Connection)


########################################################################

# SAVING THE DATASET PREPROCESSED
sum(is.na(gpus))
write.table(gpus, file = "preprocessed_GPUs.csv", sep = ",", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
