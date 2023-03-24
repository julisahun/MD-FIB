library(cluster)
library(factoextra)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
gpus <- read.csv("../data/preprocessed_GPUS.csv", header = T, sep = ",");

#create plot of number of clusters vs total within sum of squares
set.seed(1)
names(gpus)
dim(gpus)
summary(gpus)
attach(gpus)


numericBools <- gpus
cols <- sapply(gpus, is.logical)
numericBools[,cols] <- lapply(gpus[,cols], as.numeric)

## extract only the numerical featues
cols <- unlist(lapply(numericBools, is.numeric))
numericGpu <- numericBools[,cols]

scaledGpus <- scale(numericGpu)

#
# CLUSTERING
#

fviz_nbclust(scaledGpus, kmeans, method = "wss")


# KMEANS RUN, BUT HOW MANY CLASSES?
k<-4
k1 <- kmeans(scaledGpus,k)
names(scaledGpus)
print(k1)

attributes(k1)

k1$size

k1$withinss

k1$centers

Bss <- sum(rowSums(k1$centers^2)*k1$size)
Bss
Wss <- sum(k1$withinss)
Wss
Tss <- k1$totss
Tss

Bss+Wss

Ib1 <- 100*Bss/(Bss+Wss)
Ib1

############################################################

k2$centers
k1$centers

plot(k1$centers[,3],k1$centers[,2])

table(k1$cluster, k2$cluster)


# HIERARCHICAL CLUSTERING

d  <- dist(scaledGpus[1:50,])
h1 <- hclust(d,method="ward.D")  # NOTICE THE COST
plot(h1)

d  <- dist(scaledGpus)
h1 <- hclust(d,method="ward")  # NOTICE THE COST
plot(h1)

# BUT WE ONLY NEED WHERE THERE ARE THE LEAPS OF THE HEIGHT

# WHERE ARE THER THE LEAPS? WHERE WILL YOU CUT THE DENDREOGRAM?, HOW MANY CLASSES WILL YOU OBTAIN?

nc = 3

c1 <- cutree(h1,nc)

c1[1:20]

nc = 5

c5 <- cutree(h1,nc)

c5[1:20]


table(c1)
table(c5)
table(c1,c5)


cdg <- aggregate(as.data.frame(scaledGpus),list(c1),mean)
cdg

plot(cdg[,1], cdg[,7])

# LETS SEE THE PARTITION VISUALLY


plot(Edad,Estalvi,col=c1,main="Clustering of credit data in 3 classes")
legend("topright",c("class1","class2","class3"),pch=1,col=c(1:3))



plot(RatiFin,Estalvi)
plot(RatiFin,Estalvi,col=c1,main="Clustering of credit data in 3 classes")
legend("topright",c("class1","class2","class3"),pch=1,col=c(1:3), cex=0.6)

plot(Antiguedad.Trabajo,Estalvi,col=c1,main="Clustering of credit data in 3 classes")
legend("topright",c("class1","class2","class3"),pch=1,col=c(1:3), cex=0.6)
plot(Patrimonio, Ingresos,col=c1,main="Clustering of credit data in 3 classes")
legend("topright",c("class1","class2","class3"),pch=1,col=c(1:3), cex=0.6)
plot(Patrimonio, Antiguedad.Trabajo,col=c1,main="Clustering of credit data in 3 classes")
legend("topright",c("class1","class2","class3"),pch=1,col=c(1:3), cex=0.6)

pairs(scaledGpus[,1:7], col=c1)

#plot(FI[,1],FI[,2],col=c1,main="Clustering of credit data in 3 classes")
#legend("topleft",c("c1","c2","c3"),pch=1,col=c(1:3))

# LETS SEE THE QUALITY OF THE HIERARCHICAL PARTITION



Bss <- sum(rowSums(cdg^2)*as.numeric(table(c1)))

Ib4 <- 100*Bss/Tss
Ib4


#move to Gower mixed distance to deal 
#simoultaneously with numerical and qualitative data

library(cluster)

#dissimilarity matrix

actives<-c(2:16)
dissimMatrix <- daisy(dd[,actives], metric = "gower", stand=TRUE)

distMatrix<-dissimMatrix^2

h1 <- hclust(distMatrix,method="ward.D")  # NOTICE THE COST
#versions noves "ward.D" i abans de plot: par(mar=rep(2,4)) si se quejara de los margenes del plot

plot(h1)

c2 <- cutree(h1,4)

#class sizes 
table(c2)

#comparing with other partitions
table(c1,c2)


names(dd)
#ratiFin
boxplot(dd[,16]~c2, horizontal=TRUE)

#plazo
boxplot(dd[,4]~c2, horizontal=TRUE)

#gastos
boxplot(dd[,9]~c2, horizontal=TRUE)

pairs(scaledGpus[,1:7], col=c2)

plot(RatiFin,Estalvi,col=c2,main="Clustering of credit data in 3 classes")
legend("topright",levels(c2),pch=1,col=c(1:4), cex=0.6)

cdg <- aggregate(as.data.frame(scaledGpus),list(c2),mean)
cdg

plot(Edad, Gastos, col= c2)
points(cdg[,4],cdg[,5],pch=16,col="orange")
text(cdg[,4],cdg[,5], labels=cdg[,1], pos=2, font=2, cex=0.7, col="orange")

potencials<-c(3,4,6,7,10,11)
pairs(scaledGpus[,potencials],col=c2)

#Profiling plots

scaledGpus <- scale(scaledGpus)


