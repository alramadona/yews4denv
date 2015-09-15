library(mgcv)
attach(training)

# association models
# meteorology only model - all variables all lags
mod.fmet <- gam(dengue ~ 
                  s(templ0, k=4) + s(templ1, k=4) + s(templ2,k=4) + s(templ3,k=4) 
                + s(rainl0,k=4) + s(rainl1,k=4) + s(rainl2,k=4) + s(rainl3,k=4) 
                + s(huml0, k=4) + s(huml1, k=4) + s(huml2, k=4) + s(huml3, k=4)
                , family=quasipoisson, na.action=na.exclude)
summary(mod.fmet)

par(mfrow=c(1,4))
plot.gam(mod.fmet, ylim=c(-1.2,1.2), ylab="log(RR)")

# meteorology optimal model
mod.omet <- gam(dengue ~ 
                s(templ3,k=4) + s(rainl2,k=4) + s(rainl3,k=4), 
                family=quasipoisson, na.action=na.exclude)
summary(mod.omet)

par(mfrow=c(1,1))
p <- fitted.values(mod.omet)
plot(time, dengue, type="l",ylab="Number Cases",axes=F,xlab="Year")
points(predict(mod.omet, type="response"),type="l", col="red")
axis(1, at=c(6,18,30,42,54,66,78,90,102,114),labels=c(2001:2010))
axis(2, at=c(0,50,100,150,200,250))
title(main="Model A")

sqrt(mean((dengue-p)^2,na.rm=T))
sqrt(mean((dengue-p)^2,na.rm=T))/sqrt(mean((dengue)^2))

# AR lag 2 model
mod.l2 <- gam(dengue ~ s(denguel2,k=4), 
              family=quasipoisson, na.action=na.exclude)
summary(mod.l2)

par(mfrow=c(1,1))
p <- fitted.values(mod.l2)
plot(time, dengue, type="l")
plot(time, dengue, type="l",ylab="Number Cases",axes=F,xlab="Year")
points(predict(mod.l2, type="response"),type="l", col="red")
axis(1, at=c(6,18,30,42,54,66,78,90,102,114),labels=c(2001:2010))
axis(2, at=c(0,50,100,150,200,250))
title(main="Model B")

sqrt(mean((dengue-p)^2,na.rm=T))
sqrt(mean((dengue-p)^2,na.rm=T))/sqrt(mean((dengue)^2))

# AR optimal model
mod.fcum <- gam(dengue ~ s(denguel2,k=4) + s(denguel24,k=4), 
                  family=quasipoisson, na.action=na.exclude)
summary(mod.fcum)

par(mfrow=c(1,1))
p <- fitted.values(mod.fcum)
plot(time, dengue, type="l")
plot(time, dengue, type="l",ylab="Number Cases",axes=F,xlab="Year")
points(predict(mod.fcum, type="response"),type="l", col="red")
axis(1, at=c(6,18,30,42,54,66,78,90,102,114),labels=c(2001:2010))
axis(2, at=c(0,50,100,150,200,250))
title(main="Model C")

sqrt(mean((dengue-p)^2,na.rm=T))
sqrt(mean((dengue-p)^2,na.rm=T))/sqrt(mean((dengue)^2))

# final model
mod.fin <- gam(dengue ~ 
                 s(templ3,k=4) + s(rainl2,k=4) + s(rainl3,k=4) 
                + s(denguel2,k=4) + s(denguel24,k=4), 
                family=quasipoisson, na.action=na.exclude)
summary(mod.fin)

par(mfrow=c(1,1))
p <- fitted.values(mod.fin)
plot(time, dengue, type="l")
plot(time, dengue, type="l",ylab="Number Cases",axes=F,xlab="Year")
points(predict(mod.fin, type="response"),type="l", col="red")
axis(1, at=c(6,18,30,42,54,66,78,90,102,114),labels=c(2001:2010))
axis(2, at=c(0,50,100,150,200,250))
title(main="Model D")

sqrt(mean((dengue-p)^2,na.rm=T))
sqrt(mean((dengue-p)^2,na.rm=T))/sqrt(mean((dengue)^2))

detach(training)
