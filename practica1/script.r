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
#dd <- read.csv("C:/Users/Cris/Desktop/UNI/MD/MD/practica1/GPUS.csv", header = T, sep = ",")
attach(dd)

dd<-GPUS

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
<<<<<<< Updated upstream
=======
# We decided to remove rows that haven't a value to Architecture feature
>>>>>>> Stashed changes
dim(dd[dd[c("Architecture")] == 0,])
dd <- dd[!is.na(dd[,1]),]


######### PORTS CONNECTIONS ############
<<<<<<< Updated upstream
#posem a 0 els NA dels ports
=======
# Imputation to 0 to ports connection that have a NA value
>>>>>>> Stashed changes
dd["DisplayPort_Connection"][is.na(dd["DisplayPort_Connection"])] <- 0
dd[is.na(dd[c("HDMI_Connection")]), c("HDMI_Connection")] <- 0
dd[is.na(dd[c("VGA_Connection")]), c("VGA_Connection")] <- 0
dd[is.na(dd[c("DVI_Connection")]), c("DVI_Connection")] <- 0

<<<<<<< Updated upstream
# si la grafica te algun dels ports i el valor de dedicated es na aleshores vol dir que es dedicada, posem valor Yes, 
# dd[is.na(dd[c("Dedicated")]) & (dd[c("HDMI_Connection")] > 0 | dd[c("DisplayPort_Connection")] > 0 | dd[c("DVI_Connection")] > 0 | dd[c("VGA_Connection")] > 0), c("Dedicated")] <- "Yes"
# si te tots els ports a 0 llavors es integrada
# dd[is.na(dd[c("Dedicated")]), c("Dedicated")] <- "No"

######### INTEGRATED I DEDICATED ############
# ens carreguem les rows que tenen na a la columna de dedicated (un total de 13 files)
dd <- dd[!is.na(dd[,c("Dedicated")]),]
# assignem a dedicated i integrated el resultat de comparar el valor de dedicated amb Yes, per tant convertim a boolean
dd$Dedicated <- (dd$Dedicated == "Yes")
dd$Integrated <- (dd$Integrated == "Yes")


# vaya nos podemos cargar las filas que no tengan DP, HDMI, DVI o VGA??
# dim(dd[dd[c("HDMI_Connection")] == 0 & dd[c("DisplayPort_Connection")] == 0 & dd[c("DVI_Connection")] == 0 & dd[c("VGA_Connection")] == 0,])
# dim(dd[dd[c("HDMI_Connection")] > 0 | dd[c("DisplayPort_Connection")] > 0 | dd[c("DVI_Connection")] > 0 | dd[c("VGA_Connection")] == 0,])

######### BEST_RESOLUTION ############
install.packages("tidyr")
library(tidyr)
# dividim la columna de Best Resolution en 2, una que diu les x i l'altra les y
dd <- separate(data=dd, col=Best_Resolution, into = c("Best_Resolution_X", "Best_Resolution_Y"), sep=" x ")

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
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# canviem els valors que son NA per la moda de la columna
dd[is.na(dd[c("Core_Speed")]), c("Core_Speed")] <- -1
dd$Core_Speed <- gsub(' MHz','', dd$Core_Speed)
dd$Core_Speed <- as.numeric(dd$Core_Speed)
sum(-1 == dd$Core_Speed) # Hi ha 867 valors de core_speed a -1.

aux <- dd[-1 != (dd[,c("Core_Speed")]),]
dd[-1 == (dd[c("Core_Speed")]), c("Core_Speed")] <- getmode(aux$Core_Speed)

=======

######### INTEGRATED I DEDICATED ############
# We remove instances of GPUS that has an NA value in dedicated column
dd <- dd[!is.na(dd[,c("Dedicated")]),]
# Transformation of columns Dedicated and Integrated to boolean types
dd$Dedicated <- (dd$Dedicated == "Yes")
dd$Integrated <- (dd$Integrated == "Yes")


######### BEST_RESOLUTION ############
# We split the column of Best Resolution into two: Best_Resolution_W and Best_Resolution_H
#dd <- separate(data=dd, col=Best_Resolution, into = c("Best_Resolution_X", "Best_Resolution_Y"), sep=" x ")

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!PROPOSTA!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
dd[is.na(dd[c("Best_Resolution")]), c("Best_Resolution")] <- getmode(dd$Best_Resolution)

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

>>>>>>> Stashed changes

table(is.na(DVI_Connection))
table(DVI_Connection == 0)

table(Patrimonio == 99999999)

table(Cargas.patrimoniales == 99999999)

table(Antiguedad.Trabajo == 0)

table(Ingresos == 0)

# The numerical vars have too much missing data. 
# Corresponding rows cannot be deleted
# Missing data treatment required

#
# Recode missing data to NA, including '0' in Incomes

#Ingresos[Ingresos == 99999999 | Ingresos == 0] <- NA (No ho fem mai en general)
Ingresos[Ingresos == 99999999] <- NA
#els estructurals posar la condicio justa i substituir pel valor que correspongui
Patrimonio[Patrimonio == 99999999] <- NA
Cargas.patrimoniales[Cargas.patrimoniales == 99999999] <- NA


#How to test randomness of missings? Test de little (ens el saltem)

# WARNING: NOW dd[,10] Ingressos DOESNT HAVE THE SAME CONTENT

hist(dd[,10])
hist(Ingresos)
summary(dd[,10])
sd(dd[,10])
summary(Ingresos)
sd(dd[,10])
sd(Ingresos, na.rm=TRUE)


dd[,10]<-Ingresos
hist(dd[,10])

summary(dd[,10])
summary(Ingresos)


#start substituting the structural missing values.
#with remaining, impute: Knn, MIMMI, MICE (multiple imputation, only if you know well)

# IMPUTATION By THE 1NN

library(class)

# FOR EVERY INDIVIDUAL WITH MISSING Incomes LOOK FOR THE MOST SIMILAR INDIVIDUAL 
# wrt REMAINING VARIABLES AND COPY THE VALUE OF INGRESSOS ON THE FIRST 
#For more robustness average the values of k-NN in general (with small k)


# For a single variable:
# Build an artificial matrix with the full numerical variables

fullVariables<-c(2,4,5,9,13,14)
aux<-dd[,fullvariables]
dim(aux)
names(aux)

# divide in rows that had missing incomes or not on the target variable to be imputed
aux1 <- aux[!is.na(Ingresos),]
dim(aux1)
aux2 <- aux[is.na(Ingresos),]
dim(aux2)

#Find nns for aux2
knn.ing = knn(aux1,aux2,Ingresos[!is.na(Ingresos)])   

#CARE: neither aux1 nor aux2 can contain NAs


#CARE: knn.ing is generated as a factor. 
#Be sure to retrieve the correct values

IngresosOriginal<-Ingresos
Ingresos[is.na(Ingresos)] <- as.numeric(levels(knn.ing))


hist(IngresosOriginal)
summary(IngresosOriginal)

hist(Ingresos)
summary(Ingresos)

#For several Variables: 

#built indexes of numerical variables that require inputation

#Cargas.patrimoniales (12), patrimoni (11), Ingressos (10)
uncompleteVars<-c(10,11,12)

#better if you sort them by increasing number of missing values

fullVariables<-c(2,4,5,9,13,14)
aux<-dd[,fullVariables]
dim(aux)
names(aux)

for (k in uncompleteVars){
  aux1 <- aux[!is.na(dd[,k]),]
  dim(aux1) 
  aux2 <- aux[is.na(dd[,k]),]
  dim(aux2)

  RefValues<- dd[!is.na(dd[,k]),k]
  #Find nns for aux2
  knn.values = knn(aux1,aux2,RefValues)   

  #CARE: neither aux1 nor aux2 can contain NAs


  #CARE: knn.ing is generated as a factor. 
  #Be sure to retrieve the correct values

  dd[is.na(dd[,k]),k] = as.numeric(as.character(knn.values))
  fullVariables<-c(fullVariables, k)
  aux<-dd[,fullVariables]
}

dim(dd)
summary(dd)

#knowledge-based Inputation 

#MIMMI method?





#check for outliers
#how?


#Transformations? In General avoid


# Creation of new derived VARIABLES: ?FEATURE EXTRACTION?

# RATIO OF FINANCEMENT 

Rati_fin = 100*Importe.solicitado/Precio.del.bien.financiado

hist(Rati_fin)

# CAPACITY TO SAVE

Estalvi <- (Ingresos-Gastos-(Cargas.patrimoniales/100))/(Importe.solicitado/Plazo)

hist(Estalvi)




# SAVING THE TRANSFORMATIONS IN A INTERNAL R FILE

save.image("credsco_bin")

names(dd)
dd[,15]<-Estalvi
dd[,16]<-Rati_fin
names(dd)[15]<-"Estalvi"
colnames(dd)[16]<-"RatiFin"


#saving the dataframe in an external file
write.table(dd, file = "credscoClean.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

