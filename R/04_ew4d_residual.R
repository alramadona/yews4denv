attach(training)

resid <- residuals(mod.fin)
par(mfrow=c(2,2))
hist(resid, xlab="Residuals", main=" ")
pacf(resid, na.action = na.pass, main=" ")      
#plot.ts(resid, xlab="Month", ylab="Residuals") 
qq.gam(mod.fin, main="")
plot(dengue, fitted(mod.fin), ylab="Predicted Cases", xlab="Reported Cases", main=" ")

Box.test(resid)
shapiro.test(resid)

detach(training)
