library(gam)

mod <- gam(hum~s(temp,)+s(rain,),family= gaussian, na.action=na.exclude)
summary(mod)
par(mfrow=c(1,2))
plot.gam(mod)

hum_res <- residuals(mod,type="response")
cor(dengue,hum_res, method="spearman", use="pairwise.complete.obs")