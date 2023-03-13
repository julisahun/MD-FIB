# Script used to preprocess data from GPUS.csv
######### LIBRARIES ############
install.packages("tidyr")
library(tidyr)

######### METHODS ############
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

fillWithMode <- function(dd, column) {
  dd[is.na(dd[c(column)]), c(column)] <- getmode(dd$column)
}
path <- "/home/juli/Documentos/MD/practica1/GPUS.csv"

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

# Metainfo: Missing data represented by 0 in qualitative variables
# DEALING WITH MISSINGS: Detect
# poques files missing de Architecture, les eliminem

######### ARCHITECTURE ############
# We decided to remove rows that haven't a value to Architecture feature
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

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!PROPOSTA!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
gpus[is.na(gpus[c("Best_Resolution")]), c("Best_Resolution")] <- getmode(gpus$Best_Resolution)

gpus[is.na(gpus[c("Best_Resolution_X")]), c("Best_Resolution_X")] <- -1
gpus[is.na(gpus[c("Best_Resolution_Y")]), c("Best_Resolution_Y")] <- -1
gpus$Best_Resolution_X <- as.numeric(gpus$Best_Resolution_X)
gpus$Best_Resolution_Y <- as.numeric(gpus$Best_Resolution_Y)
# els na els substituirem per la moda de la columna
gpus[-1 == (gpus[c("Best_Resolution_X")]), c("Best_Resolution_X")] <- median(gpus$Best_Resolution_X)
gpus[-1 == (gpus[c("Best_Resolution_Y")]), c("Best_Resolution_Y")] <- median(gpus$Best_Resolution_X)

######### BOOST_CLOCK ############
gpus[is.na(gpus[c("Boost_Clock")]), c("Boost_Clock")] <- -1
gpus$Boost_Clock <- gsub(' MHz','', gpus$Boost_Clock)
gpus$Boost_Clock <- as.numeric(gpus$Boost_Clock)

sum(-1 == gpus$Boost_Clock) # Hi ha 1893 valors de boost_clock a -1. Borrem columna?
gpus[-1 == (gpus[c("Boost_Clock")]), c("Boost_Clock")] <- NA
# gpus[-1 == (gpus[c("Boost_Clock")]), c("Boost_Clock")] <- median(gpus$Boost_Clock)

######### CORE_SPEED ############


# canviem els valors que son NA per la moda de la columna
gpus[is.na(gpus[c("Core_Speed")]), c("Core_Speed")] <- -1
gpus$Core_Speed <- gsub(' MHz','', gpus$Core_Speed)
gpus$Core_Speed <- as.numeric(gpus$Core_Speed)
sum(-1 == gpus$Core_Speed) # Hi ha 867 valors de core_speed a -1.

aux <- gpus[-1 != (gpus[,c("Core_Speed")]),]
gpus[-1 == (gpus[c("Core_Speed")]), c("Core_Speed")] <- getmode(aux$Core_Speed)


######### ######### CORE_SPEED ############ ############
sum(is.na(gpus$Direct_X))
gpus[is.na(gpus$Direct_X), c("Direct_X")] <- "Unknown"






write.table(gpus, file = "credscoClean.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

