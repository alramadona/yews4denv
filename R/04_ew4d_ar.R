library(dlnm)
attach(training)

dlag <- data.frame()
cb1.temp <- crossbasis(denguel1, lag=47, argvar=list(df=4, cen=FALSE),
                       arglag=list(fun="poly",degree=4))

summary(cb1.temp)

mod.dlnm <- glm(dengue ~  cb1.temp,
                family=quasipoisson())
pred1.temp <- crosspred(cb1.temp, mod.dlnm)

par(mfrow=c(1,1))
plot(pred1.temp, "contour")
plot(pred1.temp)

detach(training)
