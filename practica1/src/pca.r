# install.packages("corrr")
# install.packages("ggcorrplot")
# install.packages("FactoMineR")
# install.packages("factoextra")
# install.packages("rgl")
library(corrr)
library(ggplot2)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(rgl)

## read data set
setwd("D:/uni/4r/MD-FIB/practica1")
gpus <- read.csv(data/preprocessed_GPUs.csv, header = T, sep = ",")

## cast boolean features (true, false) to numeric (1, 0)
numericBools <- gpus
cols <- sapply(gpus, is.logical)
numericBools[,cols] <- lapply(gpus[,cols], as.numeric)

## extract only the numerical featues
cols <- unlist(lapply(numericBools, is.numeric))
numericGpu <- numericBools[,cols]
nFeatures = dim(numericGpu)[2]

## normalise data
normalizedGpus <- scale(numericGpu)

## display correlation matrix
corr_matrix <- cor(normalizedGpus)
ggcorrplot(corr_matrix)

## first pca analisis to 
pcaValues <- prcomp(normalizedGpus, scale=TRUE)
summary(pcaValues)

## display barplots with the standard deviation of each feature and its acumulated
barplot(pcaValues$sdev)
barplot(100*cumsum(pcaValues$sdev[1:nFeatures]^2)/nFeatures)

## extract the first 6 components (up tp 80% of variance)
psi = pcaValues$x[,1:6]

## display data for 2 and 3 components of PCA
plot(psi[,1],psi[,2])
plot3d(psi[,1],psi[,2],psi[,3])

## correlation between original data and PCA data
Phi = cor(normalizedGpus,psi)
ggcorrplot(Phi)

## not sure what this is but i think its the relevancy of each feature in PCA
X<-Phi[,1]
Y<-Phi[,2]

plot(Psi[,1],Psi[,2],type="n",xlim=c(min(X,0),max(X,0)), ylim=c(-1,1))
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
text(X,Y,labels=etiq,col="darkblue", cex=0.7)


## Display PCA data with manufacturer labels (not very interesting)
varcat=factor(gpus$Manufacturer)
plot(Psi[,1],Psi[,2],col=varcat)
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("bottomleft",levels(factor(varcat)),pch=1,col=c(1,2), cex=0.6)


## Display PCA data with screen resolution labels (interesting)
varcat=factor(gpus$Resolution_W)
plot(Psi[,1],Psi[,2],col=varcat)
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("bottomleft",levels(factor(varcat)),pch=1,col=c(1,2), cex=0.6)
