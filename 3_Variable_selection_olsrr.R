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
x$Profil17[x$Profil17 == "d?u?ne"] <- "d?u?nopodobne"
x$Profil17[x$Profil17 == "pieni??ne"] <- "d?u?nopodobne"
x$Profil17[x$Profil17 == "mieszane"] <- "akcjopodobne"
x$Profil17[x$Profil17 == "inne"] <- "akcjopodobne"

lm <- lm(pd$OB ~ pd$yearly_rr + pd$yearly_std
         + pd$es + pd$eg + pd$sr + pd$var_5 + pd$var_95
         + pd$Profil_ad + pd$Lokalizacja
         + pd$Aktywa + pd$Wiek + pd$Rok
         + pd$PW + pd$NW
         + pd$OZN + pd$OZU + pd$OZW01, 
         data = pd)

ols_step_backward(lm, details = F, prem = 0.05)

lm <- lm(pd$OB ~ pd$sr + pd$var_5
         + pd$Profil_ad + pd$Wiek
         + pd$PW + pd$NW
         + pd$OZN + pd$OZW01, 
         data = pd)
summary(lm)
0.53

lm <- lm(pd$TER ~ pd$yearly_rr + pd$yearly_std
         + pd$es + pd$eg + pd$sr + pd$var_5 + pd$var_95
         + pd$Profil_ad + pd$Lokalizacja
         + pd$Aktywa + pd$Wiek + pd$Rok
         + pd$PW + pd$NW
         + pd$OZN + pd$OZU + pd$OZW01, 
         data = pd)

ols_step_backward(lm, details = F, prem = 0.05)

lm <- lm(pd$TER ~ pd$var_95
         + pd$Profil_ad + pd$Lokalizacja
         + pd$Wiek + pd$Rok
         + pd$PW + pd$NW
         + pd$OZN + pd$OZW01, 
         data = pd)
summary(lm)
0.52

lm <- lm(pd$OZZ ~ pd$yearly_rr + pd$yearly_std
         + pd$es + pd$eg + pd$sr + pd$var_5 + pd$var_95
         + pd$Profil_ad + pd$Lokalizacja
         + pd$Aktywa + pd$Wiek + pd$Rok
         + pd$PW + pd$NW
         + pd$OZN + pd$OZU + pd$OZW01, 
         data = pd)

ols_step_backward(lm, details = F, prem = 0.05)

lm <- lm(pd$OZZ ~ pd$eg + pd$var_95
         + pd$Profil_ad + pd$Lokalizacja
         + pd$Wiek
         + pd$PW
         + pd$OZN, 
         data = pd)
summary(lm)
0.71
