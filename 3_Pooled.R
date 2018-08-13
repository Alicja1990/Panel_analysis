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
x$Profil17[x$Profil17 == ""] <- "mieszane"
x$Profil17[x$Profil17 == "absolutnej stopy zwrotu"] <- "akcjopodobne"
x$Profil17[x$Profil17 == "akcji"] <- "akcjopodobne"
x$Profil17[x$Profil17 == "d³u¿ne"] <- "d³u¿nopodobne"
x$Profil17[x$Profil17 == "pieniê¿ne"] <- "d³u¿nopodobne"
x$Profil17[x$Profil17 == "mieszane"] <- "akcjopodobne"
x$Profil17[x$Profil17 == "inne"] <- "akcjopodobne"

r <- lm(x$OB18 ~ x$yearly_rr_17 + x$yearly_std_17
        + x$es17 + x$eg17 + x$Profil17 + x$Lokalizacja
        + x$Aktywa18 + x$zm.akt.17.18.flow.proc
        + x$PW17 + x$Wiek
        + x$sr17 + x$var17_5 + x$var17_95, data = x)

ols_step_backward(r, details = F, prem = 0.05)

s <- lm(x$OB18 ~ x$Profil17
        + x$Aktywa18
        + x$PW17 + x$Wiek
        + x$eg17 + x$var17_5, data = x)
