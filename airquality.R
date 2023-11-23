library(datasets)

aq <- airquality[(airquality$Month == 6 | airquality$Month == 7),]

aq_mean <- mean(aq$Wind[aq$Temp > 80])
round(aq_mean,digits = 1)


aq_may <- airquality[airquality$Month == 5,]

aq_may_temp_mean <- mean(aq_may$Temp)

aq_may_conf_int <- t.test(aq_may$Temp, conf.level = 0.90)$conf.int[1:2]