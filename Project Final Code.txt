#The changes made from the final code will be the addition of lines 15 and 16 which installs the package msm.
#Line 127 has a change that included the title for the plot
#Line 445 has an addition of Sex variable into the model 
#Line 456 has an interchange in the first two arguments
#Line 459 has an addition of three more exponential functions and the next line includes the poisson.one instead of poisson.two
#Line 463 includes changes of r.est.one which was r.est.two before
#Lines 476, 477, 478, and 479 are an addition for calculation of rmse of the poisson regression model.

install.packages("corrplot")
install.packages("ggplot2")
install.packages("PerformanceAnalytics")
install.packages("sandwich")
install.packages("olsrr")
install.packages("purrr")
install.packages("msm")
library(msm)
library(arm)
library(olsrr)
library(sandwich)
library(corrplot)
library(tidyverse)
library(PerformanceAnalytics)
library(ROCR)
library(mlbench)
library(lmtest)
library(Metrics)
library(caret)
library(tidyverse) 
library(cluster)    
library(factoextra)
library(purrr)
library(leaps)


#Read in the data set with no headers
abalone_dataset <- read.table( "abalone.data", sep=",", header=FALSE, na.strings="?" )
summary(abalone_dataset)
#getting the original column names
colnames(abalone_dataset)

#Renaming the column names for simplificity
names(abalone_dataset)[names(abalone_dataset) == "V1"] <- "Sex"
names(abalone_dataset)[names(abalone_dataset) == "V2"] <- "Length"
names(abalone_dataset)[names(abalone_dataset) == "V3"] <- "Diameter"
names(abalone_dataset)[names(abalone_dataset) == "V4"] <- "Height"
names(abalone_dataset)[names(abalone_dataset) == "V5"] <- "Whole Weight"
names(abalone_dataset)[names(abalone_dataset) == "V6"] <- "Shucked Weight"
names(abalone_dataset)[names(abalone_dataset) == "V7"] <- "Viscera Weight"
names(abalone_dataset)[names(abalone_dataset) == "V8"] <- "Shell Weight"
names(abalone_dataset)[names(abalone_dataset) == "V9"] <- "rings"

# Finding the missing values
#idx <- which( is.na(abalone_dataset), arr.ind=TRUE )
#miss <- abalone_dataset[idx[,1],]


#Variables interpretation using different plots

#boxplot(abalone_dataset$Length, ylab=("Length"),border=par("fg"), col="light blue", main="Length of the shell")
#boxplot(abalone_dataset$Diameter, ylab=("Diameter"), main="Diameter of the shell")
#boxplot(abalone_dataset$Height, ylab=("Height"), main="Height of the shell")
#boxplot(abalone_dataset$`Whole Weight`, ylab=("Whole Weight"), main="Whole Weight")
#boxplot(abalone_dataset$`Shucked Weight`, ylab=("Shucked Weight"), main="Shucked Weight")
#boxplot(abalone_dataset$`Viscera Weight`, ylab=("Viscera Weight"), main="Viscera Weight")
#boxplot(abalone_dataset$`Shell Weight`, ylab=("Shell Weight"), main = "Shell Weight")
#boxplot(abalone_dataset$`Number of Rings`, ylab=("Number of rings"), main = "Number of rings")

plot(density(abalone_dataset$Length), main="Density plot for Length") 
plot(density(abalone_dataset$Diameter), main="Density plot for Diameter")
plot(density(abalone_dataset$Height), main ="Density Plot for Height")
plot(density(abalone_dataset$`Whole Weight`), main="Density plot for Whole Weight")
plot(density(abalone_dataset$`Shucked Weight`), main ="Density Plot for Shucked Weight")
plot(density(abalone_dataset$`Viscera Weight`), main ="Density plot for Viscera Weight")
plot(density(abalone_dataset$`Shell Weight`), main="Density plot for Shell Weight")
plot(density(abalone_dataset$rings), main = "Density Plot for Number of Rings")

#sd(abalone_dataset$Sex)
#sd(abalone_dataset$Length)
#sd(abalone_dataset$Length)
#sd(abalone_dataset$Diameter)
#sd(abalone_dataset$`Whole Weight`)
#sd(abalone_dataset$`Shucked Weight`)
#sd(abalone_dataset$`Viscera Weight`)
#sd(abalone_dataset$`Shell Weight`)
#sd(abalone_dataset$`Number of Rings`)


#plot(abalone_dataset$Length,abalone_dataset$`Number of Rings`, xlab="Length of the shell", ylab="Number of Rings", main = "Scatterplot for Length and Number of Rings")
#plot(abalone_dataset$Height,abalone_dataset$`Number of Rings`,  xlab="Height of the shell", ylab="Number of Rings", main = "Scatterplot for Height and Number of Rings")
#plot(abalone_dataset$`Whole Weight`,abalone_dataset$`Number of Rings`,  xlab="Whole Weight", ylab="Number of Rings", main = "Scatterplot for Whole Weight and Number of Rings")
#plot(abalone_dataset$`Shucked Weight`,abalone_dataset$`Number of Rings`,  xlab="Shucked Weight", ylab="Number of Rings", main = "Scatterplot for Shucked Weight and Number of Rings")
#plot(abalone_dataset$`Viscera Weight`,abalone_dataset$`Number of Rings`,  xlab="Viscera Weight", ylab="Number of Rings", main = "Scatterplot for Viscera Weight and Number of Rings")
#plot(abalone_dataset$`Shell Weight`,abalone_dataset$`Number of Rings`,  xlab="Shell Weight", ylab="Number of Rings", main = "Scatterplot for Shell Weight and Number of Rings")
#plot(abalone_dataset$Diameter,abalone_dataset$`Number of Rings`, xlab="Diameter of the shell", ylab="Number of Rings", main = "Scatterplot for Diameter and Number of Rings")



dataset_correlation <- subset(abalone_dataset, select = -c(Sex))

#correlation plot

#round(cor(dataset_correlation, method=c("pearson")),2)
#round(cor(dataset_correlation,method=c("kendall")),2)

spearman_correlation <- round(cor(dataset_correlation,method=c("spearman")),2)
chart.Correlation(spearman_correlation, histogram=TRUE, pch=19, main="Correlation Chart")
corrplot(spearman_correlation,diag = TRUE,outline = TRUE,tl.col = "black", tl.offset = 1,tl.srt = 45,tl.cex=0.5,addCoefasPercent=TRUE, main = "Correlation Plot")
corrplot()



ggplot(data=abalone_dataset,aes(x=`Shucked Weight`,y=rings,color=Sex))+geom_point()+geom_smooth(method="lm")


#-------------------------------------------------------------------------------------------------------
#Final Analysis
#-------------------------------------------------------------------------------------------------------

#variable selection

#step-wise forward selection of variables
model.forward<-display(step(lm(rings ~ ., data = abalone_dataset), trace = F, direction = "forward"))
#step-wise backward selection of variables
model.backward<-display(step(lm(rings ~ ., data = abalone_dataset), trace = F, direction = "backward"))

#regsubsets selection of variables
plot(regsubsets(rings ~ ., data = abalone_dataset, method = "exhaustive", nbest = 1),main="regsubsets variable selection")


#cross validation selection and prediction
set.seed(123)
#training and test set
fit.model <- abalone_dataset[complete.cases(abalone_dataset), ]
rows <- sample(nrow(fit.model), .7 * nrow(fit.model))
train <- fit.model[rows, ]
test <- fit.model[-rows, ]

#cross validation on the complete dataset
full.model <- lm(rings ~., data = train)
#cross validation using the variables given by the step-wise backward model
select.model.backward<-lm(rings~Sex+Diameter+Height+`Whole Weight`+`Shucked Weight`+`Viscera Weight`+`Shell Weight`,data=train)

#gives the performance with the test set
results <- data.frame(Model = c("Full model in-sample",
                                "Select model in-sample",
                                "Full model out-of-sample",
                                "Select model out-of-sample"),
                      RMSE = round(c(rmse(fitted(full.model), train$rings),
                                     rmse(fitted(select.model.backward), train$rings),
                                     rmse(predict(full.model, newdata = test), test$rings), 
                                     rmse(predict(select.model.backward, newdata = test), test$rings)),1))

results

#Random Forest
set.seed(123)
rf <- train(rings ~ ., 
             data = train, 
             method = "rf", 
             importance = T)


#bootstrapped samples
#complete dataset
bootstrapped.one<-train(rings ~ .,  data = train, method = "lm")
bootstrapped.one
#Variables given by the step-wise backward model other than sex
bootstrapped.two<-train(rings ~Diameter+Height+`Whole Weight`+`Shucked Weight`+`Viscera Weight`+`Shell Weight`, 
         data = train,
         method = "lm")
bootstrapped.two

rmse(predict(bootstrapped.one, newdata = test), test$rings)
rmse(predict(bootstrapped.two, newdata = test), test$rings)


#elastic regression, combination of both alpha and lambda
#using bootstrapped approach
#Variables are scaled and centered
set.seed(123)
(glmnet.model <- train(rings ~ ., 
                       data = train,
                       preProcess = c("center", "scale"),
                       method = "glmnet"))

#ridge regression by forcing alpha as 0
set.seed(156)
(ridge.model <- train(rings ~ ., 
                      data = train,
                      preProcess = c("center", "scale"),
                      method = "glmnet",
                      tuneGrid = expand.grid(
                        alpha = 0,
                        lambda = seq(150,200, 10))))

#optimal lambda selected by cross validation above
glmnet.model$finalModel$tuneValue
#best final model object and coefficients
coef(glmnet.model$finalModel, glmnet.model$finalModel$tuneValue$lambda)
#performance
rmse(predict(select.model.backward, newdata = test), test$rings)
rmse(predict(glmnet.model, newdata = test), test$rings)
rmse(predict(ridge.model, newdata = test), test$rings)


#second way for elastic regression
#repeated cross validation function
train.cont <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           search = "random",
                           verboseIter = TRUE)
#building of elastic regression for selecting optimal value of lambda and alpha
elastic.regression <- train(rings ~ .,
                              data = train,
                              method = "glmnet",
                              preProcess = c("center", "scale"),
                              tuneLength = 10,
                              trControl = train.cont)

#gives the best parameters                     
elastic.regression$bestTune

#performance
predictions.train <- predict(elastic.regression, train)
rmse(train$rings,predictions.train)                                          
predictions.test <- predict(elastic.regression, test)
rmse(test$rings,predictions.test)


#--------------------------------------------------------------------------------------------------

#Simple linear regression model using p value
complete.model<-lm(rings~.,data=abalone_dataset)
summary(complete.model)                                          

#using p_value length and sex is excluded from the model

model.one<-lm(rings~Diameter+Height+`Whole Weight`,data=abalone_dataset)
summary(model.one)

#whole weight excluded
model.two<-lm(rings~Diameter+Height+`Shucked Weight`,data=abalone_dataset)
summary(model.two)

#shucked weight included
model.three<-lm(rings~Diameter+Height+`Shucked Weight`+`Viscera Weight`,data=abalone_dataset)
summary(model.three)

model.four<-lm(rings~Diameter+Height+`Shucked Weight`+`Viscera Weight`+`Shell Weight`,data=abalone_dataset)
summary(model.four)

#viscera weight excluded

prediction.final<-predict(model.four,abalone_dataset)
mae(abalone_dataset$rings,prediction.final)
rmse(abalone_dataset$rings,prediction.final)
#---------------------------------------------------------------------------------------------------

#logistic regression
#changing the number of rings column to a binary outcome
i<-1
for(i in 1:4177){
  if(abalone_dataset$rings[i]<=15){
    abalone_dataset$`rings`[i]=0
  }
  else{
    abalone_dataset$rings[i]=1
  }
}

#complete dataset
log.model.one <- glm(rings~., data=abalone_dataset,family=binomial(link="logit"))
summary(log.model.one)

#binned plot for assessing the residuals
binned.plot.one<-resid(log.model.one)
predmod.one <- predict(log.model.one, type="response")
binnedplot(predmod.one,binned.plot.one,nclass=NULL, 
           xlab="Expected Values", ylab="Average residual", 
           main="Binned residual plot", 
           cex.pts=0.8, col.pts=1, col.int="red")

coef(log.model.one)
t <- exp(coef(log.model.one)) 
format(t, scientific=FALSE)

#ROCR curve

ROCRpred.one <- prediction(predmod.one, abalone_dataset$rings)
ROCRperf.one <- performance(ROCRpred.one, "tpr", "fpr")
AUC.one <-performance(ROCRpred.one, "auc")
AUC.one@y.values
plot(ROCRperf.one)
plot(ROCRperf.one, colorize=TRUE)
plot(ROCRperf.one, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.3,0.3), xlim = c(0, 1))

cutoffPoint.one <- which.min( abs(ROCRperf.one@alpha.values[[1]]-0.1 ) )
cutoffPoint.one

testDF.one <- data.frame(cutoff = ROCRperf.one@alpha.values[[1]][cutoffPoint.one],
                     fpr = ROCRperf.one@x.values[[1]][cutoffPoint.one],
                     tpr = ROCRperf.one@y.values[[1]][cutoffPoint.one])
testDF.one

plot(log.model.one) # Just in order to see different residual plots to check the assumptions of the resiudals in a regression line.

class.glm.one <- as.factor(ifelse(predmod.one <= 0.1, 0, 1 ))
confusionMatrix(class.glm.one, as.factor(abalone_dataset$rings), positive = "0")

#we use the variables that are treated as significant from the output above and also which is similar to the model.four
#and the select.backward.model output

log.model.two <- glm(rings~Diameter+Height+`Whole Weight`+`Shucked Weight`+`Viscera Weight`+`Shell Weight`, data=abalone_dataset,family=binomial(link="logit"))
summary(log.model.two)

#binned plot for assessing the residuals
binned.plot.two<-resid(log.model.two)
predmod.two <- predict(log.model.two, type="response")
binnedplot(predmod.two,binned.plot.two,nclass=NULL, 
           xlab="Expected Values", ylab="Average residual", 
           main="Binned residual plot", 
           cex.pts=0.8, col.pts=1, col.int="red")

coef(log.model.two)
t <- exp(coef(log.model.two)) 
format(t, scientific=FALSE)

#ROCR curve

ROCRpred.two <- prediction(predmod.two, abalone_dataset$rings)
ROCRperf.two <- performance(ROCRpred.two, "tpr", "fpr")
AUC.two <-performance(ROCRpred.two, "auc")
AUC.two@y.values
plot(ROCRperf.two)
plot(ROCRperf.two, colorize=TRUE)
plot(ROCRperf.two, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.3,0.3), xlim = c(0, 1))

cutoffPoint.two <- which.min( abs(ROCRperf.two@alpha.values[[1]]-0.1 ) )
cutoffPoint.two

testDF.two <- data.frame(cutoff = ROCRperf.two@alpha.values[[1]][cutoffPoint.two],
                         fpr = ROCRperf.two@x.values[[1]][cutoffPoint.two],
                         tpr = ROCRperf.two@y.values[[1]][cutoffPoint.two])
testDF.two

#plot(log.model.two) # Just inorder to see different residual plots to check the assumptions of the resiudals in a regression line.

class.glm.two <- as.factor(ifelse(predmod.two <= 0.1, 0, 1 ))
confusionMatrix(class.glm.two, as.factor(abalone_dataset$rings), positive = "0")

lrtest(log.model.one,log.model.two)


#----------------------------------------------------------------------------------------------
#clustering

#read in the dataset again
abalone_dataset <- read.table( "abalone.data", sep=",", header=FALSE, na.strings="?" )
#Renaming the column names for simplificity
names(abalone_dataset)[names(abalone_dataset) == "V1"] <- "Sex"
names(abalone_dataset)[names(abalone_dataset) == "V2"] <- "Length"
names(abalone_dataset)[names(abalone_dataset) == "V3"] <- "Diameter"
names(abalone_dataset)[names(abalone_dataset) == "V4"] <- "Height"
names(abalone_dataset)[names(abalone_dataset) == "V5"] <- "Whole Weight"
names(abalone_dataset)[names(abalone_dataset) == "V6"] <- "Shucked Weight"
names(abalone_dataset)[names(abalone_dataset) == "V7"] <- "Viscera Weight"
names(abalone_dataset)[names(abalone_dataset) == "V8"] <- "Shell Weight"
names(abalone_dataset)[names(abalone_dataset) == "V9"] <- "rings"

#distances for clustering
set.seed(123)
gow <- daisy(abalone_dataset[, 2:9], metric="gower") 
kmedoids1 <-pam(gow, 3)
head(kmedoids1)
kmedoids1$clusinfo

#clustering information table based on the variable sex 
table(kmedoids1$clustering, abalone_dataset$Sex)

#distance for better k value calculation
totalDistance <- function(k)
{
  #getting the medoid
  kmedoids1 <- pam(gow, k)
  print(kmedoids1$clusinfo)
  #summing to find the dissimilarity
  totalDis <- sum(kmedoids1$clusinfo[,1]*kmedoids1$clusinfo[,3])
  return(totalDis)
}

k.values <- 2:7

#using map function to apply a function to each element and returning a vector of the same length 
distance_values <- map_dbl(k.values, totalDistance)

#using the elbow method we can see the different values of k and how the dissmilarity between medoids decreases as k increases. Where there is a 'kink' we can see a suitable choice for k
plot(k.values, distance_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Average dissimilarity",
     main = "Elbow Method")

##SILHOUETTE METHOD 
totalDistance <- function(k)
{
  kmedoids1 <- pam(gow, k)
  print(kmedoids1$silinfo$avg.width)
  totalDis <- kmedoids1$silinfo$avg.width
  return(totalDis)
}

k.values <- 2:7
distance_values <- map_dbl(k.values, totalDistance)
plot(distance_values)

#plot the k values against the distance to again see how the dissimilarity decreases as the value of k increases
plot(k.values, distance_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Silhouette Width",
     main="Silhouette method")

#---------------------------------------------------------------------------------------------------

#poisson regression

#poisson regression for the complete dataset
poisson.one <- glm(rings ~., family="poisson", data=abalone_dataset)
summary(poisson.one)

#robust standard errors calculation
covariance.poisson.one<- vcovHC(poisson.one, type="HC0")
std.err.one <- sqrt(diag(covariance.poisson.one))
r.est.one <- cbind(Estimate= coef(poisson.one), "Robust SE" = std.err.one,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(poisson.one)/std.err.one), lower.tail=FALSE),
               LL = coef(poisson.one) - 1.96 * std.err.one,
               UL = coef(poisson.one) + 1.96 * std.err.one)
r.est.one
#residual deviance
with(poisson.one, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

#second poisson model using the step-wise backward variable selection
poisson.two<-glm(rings ~ Sex+Diameter+Height+`Whole Weight`+`Shucked Weight`+`Viscera Weight`+`Shell Weight`, family="poisson", data=abalone_dataset)

covariance.poisson.two<- vcovHC(poisson.two, type="HC0")
std.err.two <- sqrt(diag(covariance.poisson.two))
r.est.two <- cbind(Estimate= coef(poisson.two), "Robust SE" = std.err.two,
                   "Pr(>|z|)" = 2 * pnorm(abs(coef(poisson.two)/std.err.two), lower.tail=FALSE),
                   LL = coef(poisson.two) - 1.96 * std.err.two,
                   UL = coef(poisson.two) + 1.96 * std.err.two)
r.est.two

#Testing of the difference in  models using the chi square test
anova(poisson.two, poisson.one,test="Chisq")

#one poisson model regression results
coeff.poisson.two <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4),~ exp(x5), ~exp(x6), ~ exp(x7),~exp(x8),~exp(x9),~exp(x10)), 
                 coef(poisson.one), covariance.poisson.one)

# exponentiate old estimates dropping the p values
rexp.est.two <- exp(r.est.one[, -3])
# replace SEs with estimates for exponentiated coefficients
rexp.est.two[, "Robust SE"] <- coeff.poisson.two
rexp.est.two

#the levels of the target variable rings
abalone_dataset$rings<-as.factor(abalone_dataset$rings)
levels(abalone_dataset$rings)

#fitting the variables in a data frame with the levels of the target variable
#(s1 <- data.frame(rings = factor(1:28, levels = 1:28, labels = levels(abalone_dataset$rings))))

#prediction using the second poisson model
x<-predict(poisson.two, type="response")
y<- predict(poisson.one, type="response")
rmse(x, abalone_dataset$rings)
rmse(y,abalone_dataset$rings)

#updating it into the original dataset data frame.
abalone_dataset$predicted <- predict(poisson.one, type="response")

#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
















