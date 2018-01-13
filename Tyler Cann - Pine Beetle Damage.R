setwd("C:/Users/thety/Desktop/330/Final Exam")
beetle <- read.csv("PineBeetle2.csv", header = TRUE)

beetle$Infested<-ifelse(beetle$Infested=='Yes', 1,0)

head(beetle)
summary(beetle)

#1b
scatter.smooth(beetle$January,jitter(beetle$Infested,amount=0.3),main="Jitter Jan. Min. Temp. Scatterplot")
#strong negative correlation (infestation numbers go down as it gets warmer from the super cold)
scatter.smooth(beetle$August_max,jitter(beetle$Infested,amount=0.3),main="Jitter Aug. Max. Temp. Scatterplot")
#strong negative correlation after 25 (infestation numbers go down after it gets too hot)
scatter.smooth(beetle$Slope,jitter(beetle$Infested,amount=0.3),main="Jitter Mtn. Slope Scatterplot")
#weak negative correlation (the steeper the slope, the less beetles infesting)
scatter.smooth(beetle$Elev,jitter(beetle$Infested,amount=0.3),main="Jitter Elevation Scatterplot")
#weak positive correlation (about the same infestation all the way through)
scatter.smooth(beetle$Precip,jitter(beetle$Infested,amount=0.3),main="Jitter Precipitation Scatterplot")
#strong positive correlation until 500 inches (more infestation until 500 inches)
plot(beetle$NC,jitter(beetle$Infested,amount=0.3),main="North Central Infestation") #mostly no
plot(beetle$NW,jitter(beetle$Infested,amount=0.3)) #mostly yes
plot(beetle$EC,jitter(beetle$Infested,amount=0.3)) #mostly no
plot(beetle$WC,jitter(beetle$Infested,amount=0.3),main="West Central Infestation") #mostly yes
plot(beetle$SE,jitter(beetle$Infested,amount=0.3)) #mostly no
plot(beetle$SC,jitter(beetle$Infested,amount=0.3)) #mostly yes
plot(beetle$SW,jitter(beetle$Infested,amount=0.3)) #even but mostly yes

#2a
#fortunately diabetes is already the last variable
library(bestglm)
vs.res <- bestglm(beetle, IC="BIC", method = "exhaustive",family=binomial) 
vs.res$BestModels 
vs.res$Subsets 
plot(vs.res$Subsets$BIC,type="b",pch=19,xlab="number of variables",ylab="BIC")
best.lm <- vs.res$BestModel
summary(best.lm)

vs.res2 <- bestglm(beetle, IC="AIC", method = "exhaustive",family=binomial) 
vs.res2$BestModels #true means its included, false means it isnt
vs.res2$Subsets #minimize AIC maximize Rsquared
plot(vs.res2$Subsets$AIC,type="b",pch=19,xlab="number of variables",ylab="AIC")
best.lm2 <- vs.res2$BestModel 
summary(best.lm2)


#3a
coef(best.lm)
confint(best.lm,level=.95)

multiple.conf <- exp(confint(best.lm))
multiple.conf
percentage.conf <- 100*(multiple.conf-1)
percentage.conf

exp(-0.147)
exp(-1.218)
exp(-0.157)

#3b

pred.probs <- predict.glm(best.lm,type="response")
thresh <- seq(0,1,length=100)
misclass <- rep(NA, length=length(thresh))

for(i in 1:length(thresh)){
  #If probability greater than threshold then 1 else 0
  my.classification <- ifelse(pred.probs>thresh[i],1,0)
  # calculate the pct where my classification not eq truth
  misclass[i] <- mean(my.classification!= beetle$Infested)
}
#Find threshold which minimizes miclassification
cutty <- thresh[which.min(misclass)]
cutty
plot(thresh,misclass,type="l",main="Expected Threshold Plot")

table(beetle$Infested,pred.probs>cutty) #this is a confusion matrix
conf.mat <- addmargins(table(beetle$Infested,pred.probs>cutty)) 
conf.mat

sens <- conf.mat[2,2]/conf.mat[2,3]
sens 
spec <- conf.mat[1,1]/conf.mat[1,3]
spec
ppv <- conf.mat[2,2]/conf.mat[3,2]
ppv
npv <- conf.mat[1,1]/conf.mat[3,1]
npv

###find pseudo r-squared###
up <- best.lm$deviance
down <- best.lm$null.deviance
pseudorsqr <- (1-(up/down))
pseudorsqr

#3c
## Choose number of CV studies to run in a loop & test set size
n.cv <- 500
n.test <- round(.1*nrow(beetle))
## Set my threshold for classifying
cutoff <- cutty #the value we found earlier after running "thresh"
## Initialize matrices to hold CV results
sens <- rep(NA,n.cv)
spec <- rep(NA,n.cv)
ppv <- rep(NA,n.cv)
npv <- rep(NA,n.cv)
## Begin for loop
for(cv in 1:n.cv){
  ## Separate into test and training sets
  obs.test <- sample(1:nrow(beetle),round(.1*nrow(beetle)))
  test.set <- beetle[obs.test,] #putting a blank after a comma will give you all of teh columns
  train.set <- beetle[-obs.test,]
  train.set
  ## Fit best model to training set
  train.model <- glm(Infested~January+August_max+Precip+NC+SE,data=train.set)
  ## Use fitted model to predict test set
  pred.probs <- predict.glm(train.model,newdata=test.set,
                            type="response") #response gives probabilities
  ## Classify according to threshold
  test.class <- ifelse(pred.probs>cutoff,1,0)
  ## Create a confusion matrix
  conf.mat <- addmargins(table(factor(test.set$Infested,levels=c(0,1)),
                               factor(test.class,levels=c(0,1))))
  ## Pull of sensitivity, specificity, PPV and NPV
  ## using bracket notation
  sens[cv] <- conf.mat[2,2]/conf.mat[2,3]
  spec[cv] <- conf.mat[1,1]/conf.mat[1,3]
  ppv[cv] <- conf.mat[2,2]/conf.mat[3,2]
  npv[cv] <- conf.mat[1,1]/conf.mat[3,1]
} 
##End for-loop

mean(sens)
mean(spec)
mean(ppv)
mean(npv)

#3d
d2018 <- data.frame(NC="No",SE="Yes",January=-13.98,August_max=15.89,Precip=771.13)
pred.log.odds <- predict.glm(best.lm,newdata=d2018)
pred.log.odds
pred.prob <- exp(pred.log.odds) / (1+exp(pred.log.odds))
pred.prob
#0.8625
pred.prob.alt <- predict.glm(best.lm, newdata=d2018, type="response")
pred.prob.alt #same as before

d2019 <- data.frame(NC="No",SE="Yes",January=-17.80,August_max=18.07,Precip=788.54)
pred.prob.alt <- predict.glm(best.lm, newdata=d2019, type="response")
pred.prob.alt

d2020 <- data.frame(NC="No",SE="Yes",January=-17.27,August_max=16.74,Precip=677.63)
pred.prob.alt <- predict.glm(best.lm, newdata=d2020, type="response")
pred.prob.alt

d2021 <- data.frame(NC="No",SE="Yes",January=-12.52,August_max=18.06,Precip=522.77)
pred.prob.alt <- predict.glm(best.lm, newdata=d2021, type="response")
pred.prob.alt

d2022 <- data.frame(NC="No",SE="Yes",January=-15.99,August_max=18.23,Precip=732.32)
pred.prob.alt <- predict.glm(best.lm, newdata=d2022, type="response")
pred.prob.alt

d2023 <- data.frame(NC="No",SE="Yes",January=-11.97,August_max=15.81,Precip=615.96)
pred.prob.alt <- predict.glm(best.lm, newdata=d2023, type="response")
pred.prob.alt

d2024 <- data.frame(NC="No",SE="Yes",January=-15.75,August_max=16.85,Precip=805.90)
pred.prob.alt <- predict.glm(best.lm, newdata=d2024, type="response")
pred.prob.alt

d2025 <- data.frame(NC="No",SE="Yes",January=-16.19,August_max=16.51,Precip=714.57)
pred.prob.alt <- predict.glm(best.lm, newdata=d2025, type="response")
pred.prob.alt

d2026 <- data.frame(NC="No",SE="Yes",January=-17.87,August_max=17.84,Precip=740.50)
pred.prob.alt <- predict.glm(best.lm, newdata=d2026, type="response")
pred.prob.alt

d2027 <- data.frame(NC="No",SE="Yes",January=-12.44,August_max=16.96,Precip=801.22)
pred.prob.alt <- predict.glm(best.lm, newdata=d2027, type="response")
pred.prob.alt
