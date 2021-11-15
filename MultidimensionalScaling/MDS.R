##########################################################################
######## Principal Coordinate Analysis / Multidimensional Scaling ########
##########################################################################

### load libraries ###
library(ggplot2)
######################


## generate a toy dataset of 100 parameter readings 'p' for 10 objects 'r' and 's' ##
data.matrix <- matrix(nrow=100, ncol=10)
colnames(data.matrix) <- c(paste("r", 1:5, sep=""), paste("s", 1:5, sep=""))
rownames(data.matrix) <- paste("p", 1:100, sep="")
for (i in 1:100) {
  r.values <- rpois(5, lambda=sample(x=10:1000, size=1)) # generate five values from a Poisson distribution
  s.values <- rpois(5, lambda=sample(x=10:1000, size=1))
  
  data.matrix[i,] <- c(r.values, s.values) # insert the readings data for each variable / row
}
head(data.matrix)



######## For comparison, construct a PCA plot of this data ########
pca <- prcomp(t(data.matrix), 
              #center=T, 
              scale=T)
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1) 

pca.data <- data.frame(Object=rownames(pca$x), PC1=pca$x[,1], PC2=pca$x[,2]) # columns named 'Object' is the ID column

ggplot(data=pca.data, aes(x=PC1, y=PC2, label=Object)) + 
geom_text() + # plots object/record/sample labels rather than points
theme_bw() + # plot background theme
xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
ggtitle("PCA Graph")
####################################################################



######## Construct an MDS plot of the data (Euclidean distance) ########
distance.matrix <- dist(scale(t(data.matrix), # compute distance matrix using the Euclidean metric
                              center=TRUE, 
                              scale=TRUE), 
                        method="euclidean")

mds <- cmdscale(distance.matrix, # cmdscale(): classical (metric) multidimensional scaling
                eig=TRUE, # return the eigenvalues of the eigenvalue decomposition
                x.ret=TRUE # return the double centred version of the dist matrix
                ) 

mds.var.per <- round(mds$eig/sum(mds$eig)*100, 1) # compute percentage variation that each MDS axis accounts for
mds.var.per

mds.values <- mds$points
mds.data <- data.frame(Object=rownames(mds.values), MDS1=mds.values[,1], MDS2=mds.values[,2])

ggplot(data=mds.data, aes(x=MDS1, y=MDS2, label=Object)) + 
geom_text() +
theme_bw() +
xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
ggtitle("MDS plot using Euclidean distance") # using Eucl. dist, identical to PCA plot.
###########################################################



######## Construct another MDS plot of the data (Mean log_2(fold change) distance) ########
# log(fold change) defined as log_2(sample/previous sample) = log_2(sample) - log_2(prev sample)
log2.data.matrix <- log2(data.matrix) # log base 2 of dataset

log2.distance.matrix <- matrix(0,   # create an empty distance matrix to store our manually calculated distances
                          nrow=ncol(log2.data.matrix),
                          ncol=ncol(log2.data.matrix),
                          dimnames=list(colnames(log2.data.matrix),colnames(log2.data.matrix)))

for(i in 1:ncol(log2.distance.matrix)) { # compute the distance matrix using avg(absolute value(log_2(FC) ) )
  for(j in 1:i) {
    log2.distance.matrix[i, j] <-
      mean(abs(log2.data.matrix[,i] - log2.data.matrix[,j]) )
  }
} # actually populates only the lower triangle of the distance matrix
log2.distance.matrix

mds_lf <- cmdscale(as.dist(log2.distance.matrix), # ls: 'log(fold)', 'as.dist()' tells R to use our custom dist matrix
                      eig=TRUE,
                      x.ret=TRUE)

mds_lf.var.per <- round(mds_lf$eig/sum(mds_lf$eig)*100, 1) # compute percentage variation that each MDS axis accounts for
mds_lf.var.per

mds_lf.values <- mds_lf$points
mds_lf.data <- data.frame(Object=rownames(mds_lf.values), MDS1=mds_lf.values[,1], MDS2=mds_lf.values[,2])

ggplot(data=mds_lf.data, aes(x=MDS1, y=MDS2, label=Object)) +
  geom_text() +
  theme_bw() +
  xlab(paste("MDS1 - ", mds_lf.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds_lf.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using avg(logFC) as distance") # this MDS plot differs from the PCA and Euclidean MDS plots
##############################################################

