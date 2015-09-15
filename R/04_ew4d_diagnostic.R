attach(training)

bind <- dengue>60
tr <-rep(60,times=length(dengue))

par(mfrow=c(1,1))
p <- fitted.values(mod.fin)
plot(time, dengue, type="l")
points(p,type="l", col="red")
abline(h=60)
points(tr,type="p",col="grey")

p1 <- p>60
dengue1 <- dengue>60
xtabs(p1~dengue1)
table(p1,dengue1)
prop.table(table(p1,dengue1))

title(main="Specificity=81.0, Sensitivity=88.9, PPV=74.4, NPV=92.2")

detach(training)
