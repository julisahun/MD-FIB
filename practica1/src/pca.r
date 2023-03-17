install.packages("corrr")
install.packages("ggcorrplot")
install.packages("FactoMineR")
install.packages("factoextra")
library(corrr)
library(ggplot2)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)

path <- "D:/uni/4r/MD-FIB/practica1/data/preprocessed_GPUs.csv"
gpus <- read.csv(path, header = T, sep = ",")
numericBools <- gpus
cols <- sapply(gpus, is.logical)
numericBools[,cols] <- lapply(gpus[,cols], as.numeric)

cols <- unlist(lapply(numericBools, is.numeric))
numericGpu <- numericBools[,cols]

normalizedGpus <- scale(numericGpu)

corr_matrix <- cor(normalizedGpus)
ggcorrplot(corr_matrix)

data.pca <- princomp(corr_matrix)
summary(data.pca)

fviz_eig(data.pca, addlabels = TRUE)