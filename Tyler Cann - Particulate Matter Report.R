### THIS SHOULD READ LIKE A REPORT
### NOT NUMBERED LIKE THE HW
### USE THE SECTION HEADINGS IN THE EXPLANATION
setwd("C:/Users/thety/Desktop/330/exam1")
pm <- read.table(file="PM.txt",header=TRUE)

#1b
scatter.smooth(pm$Cars,pm$Particles,col="red",
  xlab="Number of Cars",ylab="Amount of Particulate Matter",main="Relationship Between Cars and PM at Intersection")
cor(pm$Cars,pm$Particles)

#lmpm <- lm(pm$Particles~pm$Cars)#put the y first, then the x
lmpm <- lm(Particles~Cars, data=pm)
coef(lmpm)
summary(lmpm)
abline(lmpm,col="darkgreen",lty=2,lwd=1)

#install.packages("lmtest")
library(lmtest)
bptest(lmpm)

library(MASS)
stres <- stdres(lmpm)
#cooks.Particles(lmpm)
#which(cooks.Particles(lmpm)>3) #if you have greater than 3 then you probs have outliers

hist(stres,freq=FALSE,main="Histogram of Residuals",xlab="Std. Residuals")
ressy <- seq(-3,3,length=1000)
lines(ressy,dnorm(ressy),col="blue")

plot(lmpm$fitted.values,stres,main="Residuals Vs. Fitted Values",xlab = "Fitted Values", ylab = "Residuals")
abline(0,0,lty=1,col="red")

#1c and 3a

##log
#
#

logpm <- lm(log(Particles)~log(Cars),data=pm)
logpm
summary(logpm) #0.12 r squared

plot(log(pm$Cars),log(pm$Particles),xlab="log(Cars)",
     ylab="log(Particles)",main="Transformed Scatterplot",pch=19)
abline(logpm,col="purple",lty=2,lwd=1)

cor(log(pm$Cars),log(pm$Particles)) #0.356 cor

plot(logpm$fitted.values,logpm$residuals,pch=19, ylab = "Residuals", xlab = "Particles",main="Transformed Residuals")
abline(0,0,lty=4,col="red")

logstres <- stdres(logpm)
hist(logstres, freq=FALSE, main="Histogram of Transformed Res.", xlab="Std. Residuals")
resso <- seq(-3,3,length=1000)
lines(resso,dnorm(resso),col="purple")

##sqrt
#
#
#
#sqpm <- lm(sqrt(Particles)~sqrt(Cars),data=pm)
#sqpm
#summary(sqpm) #0.11 r squared

#plot(sqrt(pm$Cars),sqrt(pm$Particles),xlab="log(Cars)",
     #ylab="log(Particles)",main="Transformed Scatterplot",pch=19)
#abline(sqpm,col="purple",lty=2,lwd=1)

#cor(sqrt(pm$Cars),sqrt(pm$Particles)) #0.346 cor

#plot(sqpm$fitted.values,sqpm$residuals,pch=19, ylab = "Residuals", xlab = "Particles",main="Transformed Residuals")
#abline(0,0,lty=4,col="red")

#sqstres <- stdres(sqpm)
#hist(sqstres, freq=FALSE, main="Histogram of Transformed Res.", xlab="Std. Residuals")
#resso <- seq(-3,3,length=1000)
#lines(resso,dnorm(resso),col="purple")

#more 3a

n.cv <- 100
bias <- rep(NA,n.cv)
rpmse <- rep(NA,n.cv)
for(i in 1:n.cv){
  ## Step 1 - split into test and training sets
  obs.test <- sample(1:nrow(pm),round(.1*nrow(pm)))
  obs.test
  test.data <- pm[obs.test,] #putting a blank after a comma will give you all of teh columns
  test.data
  training.data <- pm[-obs.test,]
  training.data
  ## Step 2 - fit model to training data
  my.model <- lm(sqrt(Particles)~Cars,data=training.data)
  ## Step 3 - predict for test data
  test.predict <- predict.lm(my.model,newdata=test.data)^2
  test.predict
  ## Step 4 - calculate bias and RMPSE
  bias[i] <- mean(test.predict-test.data$Particles)
  rpmse[i] <- sqrt(mean((test.predict-test.data$Particles)^2))
}
mean(bias) #-5.818
mean(rpmse) #34.13

#4a

summary(logpm) #4.76E-16

confint(logpm, level = .95)

coef(logpm)
hey <- seq(0,4500,length=500)
points <- predict.lm(logpm,newdata=data.frame(Cars=hey,Particles=1))
predx <- exp(points)
plot(pm$Cars,pm$Particles, main="Particle Matter given Cars",xlab="Number of Cars",ylab="Amount of Particle Matter")
lines(hey,predx,pch=19,col="red") #doing the exp undoes the log transformation that we made

#4b
predict.lm(logpm, newdata = data.frame(Cars=1800), interval = "prediction", level = .95)


