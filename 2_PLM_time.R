library(plm)
setwd("C:/Users/Alicja/Documents/Doktorat/Rozprawa doktorska/Panel_analysis/Panel_analysis/Data")
x <- read.csv2("Panel.data.csv", as.is = T, dec = ".")
r <- apply(x[, c(4, 7:9, 12:22)], 2, as.numeric)
x$OB <- r[,1]
x[, 7:9] <- r[, 2:4]
x[, 12:22] <- r[,5:15]

pd <- plm.data(x, indexes = c("Nazwa.IZFiA", "Rok"))

pool <- plm(x$OB ~ x$mean_daily_log_rr + x$std_daily_log_rr + x$es + x$eg + x$Profil, 
            data = pd, model = "pooling")
summary(pool)

beet <- plm(x$OB ~ x$mean_daily_log_rr + x$std_daily_log_rr + x$es + x$eg + x$Profil, 
            data = pd, model = "between")
summary(beet)

fixed <- plm(x$OB ~ x$mean_daily_log_rr + x$std_daily_log_rr + x$es + x$eg + x$Profil, 
            data = pd, model = "within")
summary(fixed)

rand <- plm(x$OB ~ x$mean_daily_log_rr + x$std_daily_log_rr + x$es + x$eg + x$Profil, 
            data = pd, model = "random")
summary(rand)

plmtest(pool)
pFtest(fixed, pool)
phtest(fixed, rand)