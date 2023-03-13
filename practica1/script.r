# Script used to preprocess data from GPUS.csv
######### LIBRARIES ############
install.packages("tidyr")
library(tidyr)

######### METHODS ############
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#dd <- read.csv("./GPUS.csv", header = T, sep = ",")
dd <- read.csv("F:/FIB/optatives/MD-FIB/practica1/GPUS.csv", header = T, sep = ",")
attach(dd)

#Missing data treatment

#Detect
names(dd)

# missing values come in diferent shape, we set them all as "NA"
dd[dd == ''] <- NA
dd[dd == '\n-'] <- NA
dd[dd == '\n'] <- NA
dd[dd == '-'] <- NA
dd[dd == '\n- '] <- NA

# Metainfo: Missing data represented by 0 in qualitative variables
# DEALING WITH MISSINGS: Detect
# poques files missing de Architecture, les eliminem

######### ARCHITECTURE ############
# We decided to remove rows that haven't a value to Architecture feature
dim(dd[dd[c("Architecture")] == 0,])
dd <- dd[!is.na(dd[,1]),]


######### PORTS CONNECTIONS ############
# Imputation to 0 to ports connection that have a NA value
dd["DisplayPort_Connection"][is.na(dd["DisplayPort_Connection"])] <- 0
dd[is.na(dd[c("HDMI_Connection")]), c("HDMI_Connection")] <- 0
dd[is.na(dd[c("VGA_Connection")]), c("VGA_Connection")] <- 0
dd[is.na(dd[c("DVI_Connection")]), c("DVI_Connection")] <- 0


######### INTEGRATED I DEDICATED ############
# We remove instances of GPUS that has an NA value in dedicated column
dd <- dd[!is.na(dd[,c("Dedicated")]),]
# Transformation of columns Dedicated and Integrated to boolean types
dd$Dedicated <- (dd$Dedicated == "Yes")
dd$Integrated <- (dd$Integrated == "Yes")


######### BEST_RESOLUTION ############
# We split the column of Best Resolution into two: Best_Resolution_W and Best_Resolution_H
dd <- separate(data=dd, col=Best_Resolution, into = c("Best_Resolution_X", "Best_Resolution_Y"), sep=" x ")

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!PROPOSTA!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#dd[is.na(dd[c("Best_Resolution")]), c("Best_Resolution")] <- getmode(dd$Best_Resolution)

dd[is.na(dd[c("Best_Resolution_X")]), c("Best_Resolution_X")] <- -1
dd[is.na(dd[c("Best_Resolution_Y")]), c("Best_Resolution_Y")] <- -1
dd$Best_Resolution_X <- as.numeric(dd$Best_Resolution_X)
dd$Best_Resolution_Y <- as.numeric(dd$Best_Resolution_Y)
# els na els substituirem per la moda de la columna
dd[-1 == (dd[c("Best_Resolution_X")]), c("Best_Resolution_X")] <- median(dd$Best_Resolution_X)
dd[-1 == (dd[c("Best_Resolution_Y")]), c("Best_Resolution_Y")] <- median(dd$Best_Resolution_X)

######### BOOST_CLOCK ############
dd[is.na(dd[c("Boost_Clock")]), c("Boost_Clock")] <- -1
dd$Boost_Clock <- gsub(' MHz','', dd$Boost_Clock)
dd$Boost_Clock <- as.numeric(dd$Boost_Clock)

sum(-1 == dd$Boost_Clock) # Hi ha 1893 valors de boost_clock a -1. Borrem columna?
dd[-1 == (dd[c("Boost_Clock")]), c("Boost_Clock")] <- NA
# dd[-1 == (dd[c("Boost_Clock")]), c("Boost_Clock")] <- median(dd$Boost_Clock)

######### CORE_SPEED ############


# canviem els valors que son NA per la moda de la columna
dd[is.na(dd[c("Core_Speed")]), c("Core_Speed")] <- -1
dd$Core_Speed <- gsub(' MHz','', dd$Core_Speed)
dd$Core_Speed <- as.numeric(dd$Core_Speed)
sum(-1 == dd$Core_Speed) # Hi ha 867 valors de core_speed a -1.

aux <- dd[-1 != (dd[,c("Core_Speed")]),]
dd[-1 == (dd[c("Core_Speed")]), c("Core_Speed")] <- getmode(aux$Core_Speed)


######### MANUFACTURER  ############
sum(is.na(dd$Manufacturer))
length(unique(dd$Manufacturer))

######### MAX_POWER  ############
sum(is.na(dd$Max_Power)) #574 nulls
dd[is.na(dd[c("Max_Power")]), c("Max_Power")] <- -1
dd$Max_Power <- gsub(' Watts','', dd$Max_Power)
dd$Max_Power <- as.numeric(dd$Max_Power)

aux <- dd[-1 != (dd[,c("Max_Power")]),]
dd[-1 == (dd[c("Max_Power")]), c("Max_Power")] <- getmode(aux$Max_Power)

######### MEMORY  ############
sum(is.na(dd$Memory)) #376 nulls
dd[is.na(dd[c("Memory")]), c("Memory")] <- -1
dd$Memory <- gsub(' MB','', dd$Memory)
dd$Memory <- as.numeric(dd$Memory)

aux <- dd[-1 != (dd[,c("Memory")]),]
dd[-1 == (dd[c("Memory")]), c("Memory")] <- getmode(aux$Memory)


######### MEMORY_BRANDWIDTH ############
sum(is.na(dd$Memory_Bandwidth)) #82 nulls
dd[is.na(dd[c("Memory_Bandwidth")]), c("Memory_Bandwidth")] <- -1
dd$Memory_Bandwidth <- gsub('GB/sec','', dd$Memory_Bandwidth)
dd$Memory_Bandwidth <- as.numeric(dd$Memory_Bandwidth)

aux <- dd[-1 != (dd[,c("Memory_Bandwidth")]),]
dd[-1 == (dd[c("Memory_Bandwidth")]), c("Memory_Bandwidth")] <- getmode(aux$Memory_Bandwidth)

######### MEMORY_BUS ############
sum(is.na(dd$Memory_Bus)) #30 nulls
dd$Memory_Bus <- gsub(' Bit','', dd$Memory_Bus)
dd$Memory_Bus <- as.numeric(dd$Memory_Bus)

aux <- dd[!is.na(dd[,c("Memory_Bus")]),]
dd[is.na(dd[c("Memory_Bus")]), c("Memory_Bus")] <- getmode(aux$Memory_Bus)

######### MEMORY_SPEED ############
sum(is.na(dd$Memory_Speed)) #73 nulls
dd$Memory_Speed <- gsub(' MHz','', dd$Memory_Speed)
dd$Memory_Speed <- as.numeric(dd$Memory_Speed)

aux <- dd[!is.na(dd[,c("Memory_Speed")]),]
dd[is.na(dd[c("Memory_Speed")]), c("Memory_Speed")] <- getmode(aux$Memory_Speed)

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

######### Process ############
sum(is.na(dd$Process)) #402 nulls
dd$Process <- gsub('nm','', dd$Process)
dd$Process <- as.numeric(dd$Process)

aux <- dd[!is.na(dd[,c("Process")]),]
dd[is.na(dd[c("Process")]), c("Process")] <- getmode(aux$Process) #mirar que fem

######### ROPs ############
sum(is.na(dd$ROPs)) #475 nulls
# te multiplicacio. Veure que fem

######### Release_Date ############
sum(is.na(dd$Release_Date)) #0 nulls
dd$Release_Date <- gsub(' ','', dd$Release_Date)
dates <- as.Date(dd$Release_Date, "%d-%b-%Y")
#dd$Release_Date <- as.Date(dd$Release_Date, "%d-%b-%Y") # no funciona. Ho posa a NA

######### Release_Price ############
sum(is.na(dd$Release_Price)) #2783 nulls. 
dd$Release_Price <- NULL #Ens carreguem la columna?

######### SLI_Crossfire ############
sum(is.na(dd$SLI_Crossfire)) # 0 nulls
dd$SLI_Crossfire <- (dd$SLI_Crossfire == "Yes")

######### Shader ############
sum(is.na(dd$Shader)) # 89 nulls
aux <- dd[!is.na(dd[,c("Shader")]),]
dd[is.na(dd[c("Shader")]), c("Shader")] <- getmode(aux$Shader)

######### TMUs ############
sum(is.na(dd$TMUs)) # 475 nulls
aux <- dd[!is.na(dd[,c("TMUs")]),]
dd[is.na(dd[c("TMUs")]), c("TMUs")] <- getmode(aux$TMUs) #mirar que fem

######### Texture_Rate ############
sum(is.na(dd$Texture_Rate)) # 480 nulls
dd$Texture_Rate <- gsub(' GTexel/s','', dd$Texture_Rate)
dd$Texture_Rate <- as.numeric(dd$Texture_Rate)

aux <- dd[!is.na(dd[,c("Texture_Rate")]),]
dd[is.na(dd[c("Texture_Rate")]), c("Texture_Rate")] <- getmode(aux$Texture_Rate) #mirar que fem

#saving the dataframe in an external file
write.table(dd, file = "credscoClean.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

