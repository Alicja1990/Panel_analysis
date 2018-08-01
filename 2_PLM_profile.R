library(plm)
setwd("C:/Users/Alicja/Documents/Doktorat/Rozprawa doktorska/Panel_analysis/Panel_analysis/Data")
x <- read.csv2("pd.profile.csv", as.is = T, dec = ".")
x$rr_y17 <- x$mean_daily_log_rr_17*250
x$sd_y17 <- x$std_daily_log_rr_17*250
x$es17y <- x$es17 * 250
x$eg17y <- x$eg17 * 250

pd <- plm.data(x, indexes = c("Nazwa.IZFiA", "Profil17"))

pool <- plm(x$OB18 ~ x$mean_daily_log_rr_17*250 + x$std_daily_log_rr_17*250 
            + x$es17*250 + x$eg17*250 + x$Profil17 
            + x$Aktywa18 + x$Lokalizacja, 
            data = pd, model = "pooling")
summary(pool)
pool <- plm(x$OB18 ~ x$rr_y17 + x$sd_y17 
            + x$es17y + x$eg17y + x$Profil17 
            + x$Aktywa18 + x$Lokalizacja, 
            data = pd, model = "pooling")
summary(pool)

lm <- lm(x$OB18 ~ x$rr_y17 + x$sd_y17 
         + x$es17y + x$eg17y + x$Profil17 
         + x$Aktywa18 + x$Lokalizacja)
lm <- lm(x$OB18 ~ x$rr_y17 + x$sd_y17 
         + x$Profil17)


pool_scale <- plm(x$OB18 ~ scale(x$mean_daily_log_rr_17) + scale(x$std_daily_log_rr_17) 
            + scale(x$es17) + scale(x$eg17) + x$Profil17 
            + scale(x$Aktywa18) + x$Lokalizacja, 
            data = pd, model = "pooling")
summary(pool_scale)

beet <- plm(x$OB18 ~ x$mean_daily_log_rr_17 + x$std_daily_log_rr_17 
            + x$es17 + x$eg17 + x$Profil17 
            + x$Aktywa18 + x$Lokalizacja, 
            data = pd, model = "between")
summary(beet)

fixed <- plm(x$OB18 ~ x$mean_daily_log_rr_17 + x$std_daily_log_rr_17 
             + x$es17 + x$eg17 + x$Profil17 
             + x$Aktywa18 + x$Lokalizacja, 
            data = pd, model = "within")
summary(fixed)

rand <- plm(x$OB18 ~ x$mean_daily_log_rr_17 + x$std_daily_log_rr_17 
            + x$es17 + x$eg17 + x$Profil17 
            + x$Aktywa18 + x$Lokalizacja, 
            data = pd, model = "random")
summary(rand)

plmtest(pool)
pFtest(fixed, pool)
phtest(fixed, rand)