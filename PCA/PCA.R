##############################################
######## Principal Component Analysis ########
##############################################

# generate a toy dataset of 100 parameter readings 'p' for 10 objects 'r' and 's'
data.matrix <- matrix(nrow=100, ncol=10)
colnames(data.matrix) <- c(paste("r", 1:5, sep=""), paste("s", 1:5, sep=""))
rownames(data.matrix) <- paste("p", 1:100, sep="")
for (i in 1:100) {
  r.values <- rpois(5, lambda=sample(x=10:1000, size=1)) # generate five values from a Poisson distribution
  s.values <- rpois(5, lambda=sample(x=10:1000, size=1))
  
  data.matrix[i,] <- c(r.values, s.values) # insert the readings data for each variable / row
}

head(data.matrix)

pca <- prcomp(t(data.matrix), scale=T) # perform PCA on dataset; prcomp() expects records / samples as rows, so transpose.
                                       # scale option centres the data about the origin (required for PCA) 
# pca$x[,i] contains the data for the i-th principal component

pca.var <- pca$sdev^2 # compute variances of the PCs
pca.var.per <- round(pca.var/sum(pca.var)*100, 1) # compute percentage variances of the PCs

# generate a scree plot of the PCs
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation", ylim=c(0,100))


##############################################
# produce a better plot using ggplot2 package
library(ggplot2)

pca.data <- data.frame(Object=rownames(pca$x), PC1=pca$x[,1], PC2=pca$x[,2]) # columns named 'Object' is the ID column
pca.data # dataset is formatted as a dataframe

ggplot(data=pca.data, aes(x=PC1, y=PC2, label=Object)) +
 geom_text() +  # plots object/record/sample labels rather than points
 xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
 ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
 theme_bw() +  # plot background theme
 ggtitle("PCA Graph")

# obtain names of top 10 parameters which contribute most to PC1.
loading_scores <- pca$rotation[,1]
param_scores <- abs(loading_scores)
param_ranks <- sort(param_scores, decreasing=TRUE)
top_params <- names(param_ranks[1:10])

top_params
pca$rotation[top_params,1] # show the scores (and their +/- sign)

