library(mgcv)

attach(training)
form <- as.formula("dengue ~ s(templ3,k=4)+s(rainl2,k=4)+s(rainl3,k=4)+s(denguel2,k=4)+s(denguel24,k=4)")
mod.train <- gam(form, family=quasipoisson, na.action=na.exclude, data=training)
detach(training)

attach(f.total)
preddata <- data.frame(dengue,templ3,rainl2,rainl3,denguel2,denguel24,time)

# CI based on http://stackoverflow.com/questions/15843654/extracting-data-used-to-make-a-smooth-plot-in-mgcv

pred<-predict(mod.train, type="response", newdata=as.data.frame(preddata), se.fit = TRUE)
preddata <- transform(preddata, fitted = pred$fit)
# 95% confidence interval
preddata <- transform(preddata, 
                      upper = fitted + (1.96 * pred$se.fit),
                      lower = fitted - (1.96 * pred$se.fit))

tr <- subset(preddata,year<2011)
pr <- subset(preddata,year>2010)

par(mfrow=c(1,1))

plot(preddata$time, preddata$upper, type="l", col = "gray", ylab="Number Cases", axes=F, xlab="Year")
points(preddata$time, preddata$dengue,type="l")
points(tr$fitted,type="l", col="red")
points(pr$time, pr$fitted,type="l", col="blue")
points(preddata$time, preddata$lower,type="l", col="gray")

axis(1, at=c(6,18,30,42,54,66,78,90,102,114,126,138,150),labels=c(2001:2013))
axis(2, at=c(0,50,100,150,200,250,300))
abline(h=60, col = "gray60")
title(main="Predicted dengue cases versus reported dengue cases in 2001–2013")

detach(f.total)