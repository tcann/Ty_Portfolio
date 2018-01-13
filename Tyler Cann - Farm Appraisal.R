setwd("C:/Users/thety/Desktop/330/Exam 2")
farm <- read.table("Farms.txt", header = TRUE)

#b
farm2 <- farm[,-c(7:11)]
farm2 <- farm2[,-4]
cor(farm2)

library(car)
farmlm <- lm(acrePrice~.,data=farm)
avPlots(farmlm)
vif(farmlm)
coef(farmlm)

library(lmtest)
plot(farmlm$fitted.values,farmlm$residuals,pch=19, ylab = "Residuals", xlab = "Fitted Values",main="Residuals")
abline(0,0,lty=4,col="red")
bptest(farmlm)

library(MASS)
res <- stdres(farmlm)
hist(res, freq=FALSE, main="Histogram of Residuals", xlab="Std. Residuals")
resso <- seq(-3,3,length=1000)
lines(resso,dnorm(resso),col="purple")
ks.test(res,"pnorm")

summary(farmlm)

#2
#a

#move your response variable to be the last column in the data frame before running it
farm3 <- cbind(farm[,-1],farm[,1])
names(farm3)[11]<-"acrePrice"

library(bestglm)

vs.res <- bestglm(farm3, IC="BIC", method = "exhaustive") 
vs.res$BestModels 
vs.res$Subsets 
plot(vs.res$Subsets$BIC,type="b",pch=19,xlab="number of variables",ylab="BIC")
#they're numbering from 0 up, our plot goes from 1 up (so you just need to know it goes 1 further than needed)
best.lm <- vs.res$BestModel
summary(best.lm)

vs.res2 <- bestglm(farm3, IC="AIC", method = "exhaustive") #Not a lot of difference between 1st and 2nd best
vs.res2$BestModels #true means its included, false means it isnt
vs.res2$Subsets #minimize AIC maximize Rsquared
plot(vs.res2$Subsets$AIC,type="b",pch=19,xlab="number of variables",ylab="AIC")
#the graph gives us a 10, but the original gave us a 9
best.lm2 <- vs.res2$BestModel 
summary(best.lm2)

#3

#a b and c
#### AFTER A LOG TRANSFORMATION ####
lm2 <- lm(log(acrePrice)~.,data=farm)

avPlots(lm2)
vif(lm2)
coef(lm2)

plot(lm2$fitted.values,lm2$residuals,pch=19, ylab = "Residuals", xlab = "Fitted Values",main=" Transformed Residuals")
abline(0,0,lty=4,col="red")
bptest(lm2) #still below .05 but probs due to outliers, we're okay

res <- stdres(lm2)
hist(res, freq=FALSE, main="Histogram of Tranfsormed Res.", xlab="Std. Residuals")
resso <- seq(-3,3,length=1000)
lines(resso,dnorm(resso),col="purple")
ks.test(res,"pnorm")

summary(lm2)

n.cv <- 100
bias <- rep(NA,n.cv) 
rpmse <- rep(NA,n.cv)
cvg <- rep(NA,n.cv)
width <- rep(NA,n.cv)
dv <- rep(NA,n.cv)
for(i in 1:n.cv){
  ## Step 1 - split into test and training sets
  obs.test <- sample(1:nrow(farm),round(.1*nrow(farm)))
  head(obs.test)
  test.data <- farm[obs.test,] #putting a blank after a comma will give you all of teh columns
  head(test.data)
  training.data <- farm[-obs.test,]
  training.data
  ## Step 2 - fit model to training data
  my.model <- lm(log(acrePrice)~.,data=training.data)
  ## Step 3 - predict for test data
  test.predict <- exp(predict.lm(my.model,newdata=test.data, interval="prediction", level = .95))
  test.predict
  ## Step 4 - calculate bias and RMPSE
  bias[i] <- mean(test.predict[,1]-test.data$acrePrice)
  rpmse[i] <- sqrt(mean((test.predict[,1]-test.data$acrePrice)^2))
  cvg[i] <- mean(test.predict[,2]<test.data$acrePrice & test.predict[,3]>test.data$acrePrice)
  mcvg <- mean(cvg)
  width[i] <- mean(test.predict[,3]-test.predict[,2])
  mw <- mean(width)
}
mean(bias)
mean(rpmse)
mcvg
mw

mean(farm$acrePrice)
hist(farm$acrePrice)

#using the BIC model...

#4
#a
farm4 <- lm(acrePrice~improvements+tillable+productivity+NW+WC,data=farm3)
coef(farm4)
confint(farm4)

#c
predicto <- data.frame(improvements=0,tillable=94,productivity=96,NW="Yes",WC="No")
predict.lm(farm4,newdata=predicto,interval="prediction")
