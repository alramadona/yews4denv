library(mgcv)

form <- as.formula("dengue ~ s(templ3,k=4)+s(rainl2,k=4)+s(rainl3,k=4)+s(denguel2,k=4)+s(denguel24,k=4)")

### training
attach(training)
mod.train <- gam(form, family=quasipoisson, na.action=na.exclude, data=training)
summary(mod.train)

par(mfrow=c(1,1))
p <- fitted.values(mod.train)
plot(training$time, training$dengue, type="l")
points(predict(mod.train, type="response"),type="l", col="red")

sqrt(mean((training$dengue-p)^2,na.rm=T))
sqrt(mean((training$dengue-p)^2,na.rm=T))/sqrt(mean((training$dengue)^2))
detach(training)

### testing
attach(f.total)
preddata <- data.frame(templ3,rainl2,rainl3,denguel2,denguel24)
f.total$predict<-predict(mod.train, type="response", newdata=as.data.frame(preddata))

p <- f.total$predict
train <- subset(f.total,year<2011)
pred <- subset(f.total,year>2010)

par(mfrow=c(1,1))
plot(f.total$time, f.total$dengue, type="l")
points(train$p,type="l", col="red")
points(pred$time, pred$p,type="l", col="blue")
abline(h=60, col = "gray60")

#for training data
sqrt(mean((train$dengue-train$p)^2,na.rm=T))/sqrt(mean((train$dengue)^2))
#for validation data 2011-2013
sqrt(mean((pred$dengue-pred$p)^2,na.rm=T))/sqrt(mean((pred$dengue)^2))

detach(f.total)
