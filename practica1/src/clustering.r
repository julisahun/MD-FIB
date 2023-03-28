library(cluster)
library(factoextra)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
gpus <- read.csv("../data/preprocessed_GPUs.csv", header = T, sep = ",")

set.seed(1)

#create plot of number of clusters vs total within sum of squares
names(gpus)
dim(gpus)
summary(gpus)
attach(gpus)
sum(gpus$Manufacturer == 'ATI')
sum(gpus$Manufacturer == 'Nvidia')
sum(gpus$Manufacturer == 'Intel')
sum(gpus$Manufacturer == 'AMD')
gpus$Manufacturer = as.numeric(unclass(as.factor(gpus$Manufacturer)))
sum(gpus$Manufacturer == 1) # -> AMD
sum(gpus$Manufacturer == 2) # -> ATI
sum(gpus$Manufacturer == 3) # -> Intel
sum(gpus$Manufacturer == 4) # -> Nvidia

numericBools <- gpus
cols <- sapply(gpus, is.logical)
numericBools[,cols] <- lapply(gpus[,cols], as.numeric)

## extract only the numerical featues
cols <- unlist(lapply(numericBools, is.numeric))
numericGpu <- numericBools[,cols]
names(numericGpu)

scaledGpus <- scale(numericGpu)

#
# CLUSTERING
#

#fviz_nbclust(scaledGpus, kmeans, method = "wss") #= 4

d  <- dist(scaledGpus[1:50,])
h1 <- hclust(d,method="ward.D")  # NOTICE THE COST
plot(h1)

d  <- dist(scaledGpus)
h1 <- hclust(d,method="ward.D")  # NOTICE THE COST


# KMEANS RUN, BUT HOW MANY CLASSES?

k4 <- kmeans(scaledGpus,4)
k2 <- kmeans(scaledGpus,2)
print(k4)
print(k2)

k4$size
k2$size

sum(numericGpu["Dedicated"] == 0) == k4$size[3] #ops

k4$withinss #the lower the better

k4$centers
k2$centers

k4$betweenss #the higher the better


Bss <- k4$betweenss
Wss <- sum(k4$withinss)
Wss

Bss+Wss

Ib1 <- 100*Bss/(Bss+Wss)
Ib1

############################################################

plot(k4$centers[,3],k4$centers[,2])

table(k2$cluster)


# HIERARCHICAL CLUSTERING

nc = 4
c4 <- cutree(h1,nc)
c4

cdg <- aggregate(as.data.frame(scaledGpus),list(c4),mean)
cdg

plot(cdg[,3], cdg[,6])

# LETS SEE THE PARTITION VISUALLY

plot(Dedicated,col=c4,main="Clustering of Dedicated feature data in 4 classes")
legend("right",c("class1","class2", "class3", "class4"),pch=1,col=c(1:4),cex=1.6)

Manufacturer = gpus$Manufacturer
plot(Manufacturer,col=c4,main="Clustering of Dedicated feature data in 4 classes")
legend("right",c("class1","class2", "class3", "class4"),pch=1,col=c(1:4),cex=1.6)


plot(Texture_Rate,Pixel_Rate, col=c1,main="Clustering of TextureRate and PixelRate data in 4 classes")
legend("topright",c("class1","class2","class3", "class4"),pch=1,col=c(1:4), cex=1.6)

pairs(scaledGpus[,1:7], col=c4)


# LETS SEE THE QUALITY OF THE HIERARCHICAL PARTITION


dissimMatrix <- daisy(numericGpu, metric = "gower", stand=TRUE)
distMatrix<-dissimMatrix^2
h1 <- hclust(distMatrix,method="ward.D")
plot(h1)


#memory
boxplot(scaledGpus[,11]~c4, horizontal=TRUE, main="Memory feature distribution")

#L2_cache
boxplot(scaledGpus[,8]~c4, horizontal=TRUE, main="L2_Cache feature distribution")

#Manufacturer
boxplot(numericGpu[,9]~c4, horizontal=TRUE, main="Manufacturer feature distribution")


