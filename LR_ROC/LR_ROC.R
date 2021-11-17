#######################################################
######## Logistic Regression and ROC curve ########
#######################################################


### load libraries ###
library(pROC)  # display and analyse Receiver Operating Characteristic curves



# generate the data by sampling from a normal distribution
set.seed(18)  # setting a random seed ensures that results are reproducible
num_vals <- 120  # number of data values to generate
vals <- sort( rnorm(n=num_vals, mean=40, sd=4) )  # values sorted in ascending order

## binary classification is implemented here by testing if the rank (1 -> num_vals) of each value, 
## scaled to the interval [0,1] by dividing by num_vals, is less than or greater than a randomly generated 
## value from the uniform interval [0,1]; if value is larger, condition 'cond' is assumed to be true / 1
cond <- ifelse(test=(runif(n=num_vals) < (rank(vals)/num_vals)), yes=1, no=0)

# plot the classified data
plot(x=vals, y=cond,
     xlab="Values", ylab="Condition",
     main="Binary Classification of Values based on Condition")
abline(h=0, lty="dotted")
abline(h=1,lty="dotted")


# use glm() to fit a sigmoid logistic regression curve to the classified data
LC <- glm(cond ~ vals, family=binomial)  # 'condition' is to be predicted by 'values'
lines(vals, LC$fitted.values,  # y-values are probs that 'cond' is true, y coords of points on the sigmoid curve 
      col="#377eb8", lwd=3)
###############################


# use roc() to generate a ROC curve for many candidate classification thresholds
par(pty = "s")  # pty = "s" --> plot type = square (removes whitespace at sides)
roc(cond, LC$fitted.values, plot=TRUE, 
    legacy.axes=TRUE,  # reverse x-axis direction --> plotting FPR = 1 - TNR = 1 - Specificity
    col="#d84e48", 
    lwd=4,
    print.auc=TRUE,  # display the Area Under Curve (when comparing models, larger area is better.)
    print.auc.x=0.2, print.auc.y=0.04,  # coordinates at which to place the AUC label on the plot
    xlab="False Positive Rate (1 - Specificity)", ylab="True Positive Rate (Sensitivity)",
    main="ROC curve")
par(pty = "m")  # pty = "m" --> plot type = maximal (revert to default canvas)
###########################



## attempt tp determine optimal threshold from the ROC data
roc_dat <- roc(cond, LC$fitted.values, legacy.axes=TRUE)  # store the ROC data in a parameter
roc_df <- data.frame(  # construct a dataframe out of the (TPR, FPR, threshold) values
  tpr = roc_dat$sensitivities,   # TPR values
  fpr = (1 - roc_dat$specificities),  # FPR values
  thr = roc_dat$thresholds  # classification thresholds used to generate the ROC curve
  )


# determine the optimal threshold, based on requirement that (TPR^2 - FPR^2) is maximised 
subset_df <- roc_df[roc_df$tpr > 0.6 & roc_df$tpr < 0.9]  # consider a subset of the data from the top-left region of ROC plot
subset_df['eff'] <- subset_df['tpr']^2 - subset_df['fpr']^2  # compute custom variable 'effectiveness' for each threshold

# optimal threshold data according to our custom criterion:
opt <- subset_df[subset_df['eff']==max(subset_df['eff'])]
tpr_opt <- opt[1]; fpr_opt <- opt[2]; thr_opt <- opt[3]  # optimal TPR, FPR, threshold



# generate another ROC curve
offset <- 0.1  # will determine width of partial AUC
par(pty = "s")  # pty = "s" --> plot type = square (removes whitespace at sides)
roc(cond, LC$fitted.values, plot=TRUE, 
    legacy.axes=TRUE,  # reverse x-axis direction --> plotting FPR = 1 - TNR = 1 - Specificity
    col="#d84e48", 
    lwd=4,
    print.auc=TRUE,
    print.auc.x=0.32, print.auc.y=0.04,
    partial.auc=c(1-fpr_opt-offset, 1-fpr_opt+offset),  # compute partial AUC for a symmetric region about the optimal FPR
    auc.polygon = TRUE, auc.polygon.col = "#d84e4822",  # shade the polygon representing the partial AUC
    xlab="False Positive Rate (1 - Specificity)", ylab="True Positive Rate (Sensitivity)",
    main="ROC curve")
par(pty = "m")  # pty = "m" --> plot type = maximal (revert to default canvas)
# insert a blue point at the coordinates (FPR,TPR) corresponding to the optimal threshold determined above 
points(1-fpr_opt,tpr_opt, type="p", pch=21, col="blue", bg="blue", cex=1.2) 



