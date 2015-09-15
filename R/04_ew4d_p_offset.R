##
mod.offset <- gam(dengue ~ 
                 s(templ3,k=4) + s(rainl2,k=4) + s(rainl3,k=4) 
               + s(denguel2,k=4) + s(denguel24,k=4) + offset(log(census)) + s(census), 
               family=quasipoisson, na.action=na.exclude)
summary(mod.offset)
##