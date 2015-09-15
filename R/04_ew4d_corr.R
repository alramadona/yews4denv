attach(training)

# 
lag <- as.integer(c(0:3))
par(mfrow=c(2,2))

# dengue 
short_den <- data.frame(denguel0, denguel1, denguel2, denguel3)
cor(dengue, short_den, method="spearman", use="pairwise.complete.obs")
plot(lag, as.numeric(cor(dengue, short_den , method="spearman", use="pairwise.complete.obs")), ylab="Correlation coefficient", xlab="Lag of monthly dengue cases")          
title(main="Dengue cases vs. Dengue cases")

# temp
short_temp <- data.frame(templ0, templ1, templ2, templ3)
cor(dengue, short_temp, method="spearman", use="pairwise.complete.obs")
plot(lag, as.numeric(cor(dengue, short_temp, method="spearman", use="pairwise.complete.obs")), ylab="Correlation coefficient", xlab="Lag of monthly temperature")          
title(main="Dengue cases vs. Temperature")

# rain
short_rain <- data.frame(rainl0, rainl1, rainl2, rainl3)
cor(dengue, short_rain, method="spearman", use="pairwise.complete.obs")
plot(lag, as.numeric(cor(dengue, short_rain, method="spearman", use="pairwise.complete.obs")), ylab="Correlation coefficient", xlab="Lag of monthly rainfall")          
title(main="Dengue cases vs. Rainfall")

# hum
short_hum <- data.frame(huml0, huml1, huml2, huml3)
cor(dengue, short_hum, method="spearman", use="pairwise.complete.obs")
plot(lag, as.numeric(cor(dengue, short_hum, method="spearman", use="pairwise.complete.obs")), ylab="Correlation coefficient", xlab="Lag of monthly adj. relative humidity")          
title(main="Dengue cases vs. Adj. relative humidity")

detach(training)

