########################################
######## Random Forest Learning ########
########################################

### load libraries ###
library(ggplot2)
library(cowplot) # some tweaks to ggplot2
library(randomForest)
######################


# import data from UCI machine learning repository, details at: http://archive.ics.uci.edu/ml/datasets/Heart+Disease
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
data <- read.csv(url, header=FALSE)


### Process the data: 1) insert descriptive column names, 2) clean, type format, and factor the data ###

# 1)
colnames(data) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","hd")  # specifics of parameters not important here
factnames <- c("sex","cp","fbs","restecg","exang","slope","ca","thal","hd")   # categorical data to be factor()ised later

# 2)
str(data)   # check dataframe structure, notice some chr-type columns with numeric data
data_char <- data[,sapply(data, is.character)]   # extract these chr-type columns of data,

for(i in 1:ncol(data_char)){   # for each chr-type column, outputs message to console listing unique values
  message(paste0("Unique chr vals in col ", 
               "'", colnames(data_char)[i], "'", " : ", 
               "(", paste(unique(data_char[,i]), collapse = ", "), ")" 
               ) 
        )
}   # notice that two of the columns of the original DF have missing data denoted by the character '?'


## clean the data: convert '?' -> NA, categorical integer data to 'M'/'F' etc.
data[data == "?"] <- NA
data$sex <- ifelse(test=data$sex == 0, yes="F", no="M")   # equivalent to: data[data$sex == 1,]$sex <- "M" etc.
data$hd <- ifelse(test=data$hd == 0, yes="Healthy", no="Unhealthy")


## type format the data: attributes which contained '?' are incorrectly classified as chr type data, convert to int.
for(i in 1:ncol(data_char)){
  data[colnames(data_char)[i] ] <- as.integer(data[colnames(data_char)][,i] )
}


## convert the categorical type data to factors (using the earlier defined 'factnames' vector)
for(i in 1:ncol(data)){
  if(colnames(data[i]) %in% factnames){
    data[i] <- as.factor(data[i][,1])
  }
}


################################################################################
### Generate a Random Forest, and train on the data ###

set.seed(16)  # set a random seed to ensure results are reproducible 
data_imp <- rfImpute(hd ~ ., data = data, iter=6) # impute missing values (NAs) in the training data using proximities,
                                                  # here heart disease 'hd' is the parameter to be predicted by all other cols

## NOTE: if parameter to be predicted (here 'hd') is a continuous variable, then by default randomForest() will set 
## 'mtry'--the number of variables to consider when growing a tree--to # of predictor variables divided by 3 
## (rounded down). If predicted parameter is a "factor" (categorical), then randomForest() will set 'mtry' to 
## the square root of # of variables (rounded down).

RF <- randomForest(hd ~ ., data=data_imp,
                   #ntree = 500,   # default value if this option unspecified
                   #mtry = 3,   # default value if this option unspecified
                   proximity=TRUE)   # generates a RF using imputed data, also returns proximity matrix


## test out-of-bag (OOB) error rate convergence as a function of # of trees in random forest RF ##
oob.error.data <- data.frame(
  Trees=rep(1:nrow(RF$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(RF$err.rate)),
  Error=c(RF$err.rate[,"OOB"], 
          RF$err.rate[,"Healthy"], 
          RF$err.rate[,"Unhealthy"]))


ggplot(data=oob.error.data, aes(x=Trees, y=Error)) + 
  geom_line(aes(color=Type)) + 
  ggtitle("Error convergence test")
#################################################


## Test for optimal number of tree node variables ('mtry') ##
oob_values <- vector(length = 10)   # create an empty vect to store the OOB values
for(i in 1:length(oob_values)) {
  test_RF <- randomForest(hd ~ ., data=data_imp, mtry=i, ntree=500)   # create a forest for each 'mtry' trial value
  oob_values[i] <- test_RF$err.rate[nrow(test_RF$err.rate),1]   # store the final (i.e. best) OOB value for each loop iteration
}


## Run the random forest training again for the optimal # of 'mtry' ##
RF_opt <- randomForest(hd ~ ., data=data_imp, ntree=500, proximity=TRUE, 
                      mtry=which(oob_values == min(oob_values) ) )


## Create an MDS plot ##
dist_matrix <- as.dist(1 - RF_opt$proximity)   # generate a distance matrix from the proximity matrix

mds <- cmdscale(dist_matrix,   # cmdscale(): classical (metric) multidimensional scaling
                eig=TRUE,   # return the eigenvalues of the eigenvalue decomposition
                x.ret=TRUE   # return the double centred version of the dist matrix, not really necessary here
) 

mds.values <- mds$points
mds.data <- data.frame(Object=rownames(mds.values), MDS1=mds.values[,1], MDS2=mds.values[,2],
                       Status=data_imp$hd)

ggplot(data=mds.data, aes(x=MDS1, y=MDS2, label=Object)) + 
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities) as distance matrix")

