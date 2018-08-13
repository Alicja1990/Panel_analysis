library(plm)
library(olsrr)
setwd("C:/Users/Alicja/Documents/Doktorat/Rozprawa doktorska/Panel_analysis/Data")
x <- read.csv("pd.profile.csv", as.is = T)
x$Aktywa18 <- as.numeric(gsub(",", ".", x$Aktywa18))
x$zm.akt.17.18.proc <- as.numeric(gsub(",", ".", x$zm.akt.17.18.proc))
x$zm.akt.17.18.flow.proc <- as.numeric(gsub(",", ".", x$zm.akt.17.18.flow.proc))
x$Wiek <- as.numeric(gsub(",", ".", x$Wiek))
x$sr16 <- as.numeric(gsub(",", ".", x$sr16))
x$sr17 <- as.numeric(gsub(",", ".", x$sr17))
x <- x[!is.na(x$yearly_rr_17),]
x <- x[!is.na(x$yearly_std_17),]
x <- x[!is.na(x$zm.akt.17.18.proc),]
x <- x[!is.na(x$zm.akt.17.18.flow.proc),]
x <- x[!is.na(x$es17),]
x <- x[!is.na(x$eg17),]
x <- x[!is.na(x$var17_5),]
x <- x[!is.na(x$var17_95),]
x <- x[!is.na(x$sr17),]

pd <- pdata.frame(x, index = c("Nazwa_IZFiA", "Profil17"))

pool <- plm(x$OB18 ~ x$yearly_rr_17 + x$yearly_std_17
            + x$es17 + x$eg17 + Profil17
            + x$Aktywa18 + x$Lokalizacja + x$zm.akt.17.18.flow.proc
            + x$PW17 + x$Wiek
            + x$sr17 + var16_5 + var16_95 + var17_5 + var17_95, 
            data = pd, model = "pooling")
summary(pool)

pool_scale <- plm(x$OB18 ~ scale(x$mean_daily_log_rr_17) + scale(x$std_daily_log_rr_17) 
            + scale(x$es17) + scale(x$eg17) + x$Profil17 
            + scale(x$Aktywa18) + x$Lokalizacja, 
            data = pd, model = "pooling")
summary(pool_scale)


plmtest(pool)
pFtest(fixed, pool)
phtest(fixed, rand)