library(olsrr)
library(plm)

setwd("C:/Users/Alicja/Documents/Doktorat/Rozprawa doktorska/Panel_analysis/Data")
x <- read.csv("Panel.data.csv", as.is = T)
x$Aktywa <- as.numeric(x$Aktywa)
x$Profil_ad <- x$Profil
x$Profil_ad[x$Profil_ad == ""] <- "mieszane"
x$Profil_ad[x$Profil_ad == "absolutnej stopy zwrotu"] <- "akcjopodobne"
x$Profil_ad[x$Profil_ad == "akcji"] <- "akcjopodobne"
x$Profil_ad[x$Profil_ad == "dłużne"] <- "dluznopodobne"
x$Profil_ad[x$Profil_ad == "pieniężne"] <- "dluznopodobne"
x$Profil_ad[x$Profil_ad == "mieszane"] <- "akcjopodobne"
x$Profil_ad[x$Profil_ad == "inne"] <- "akcjopodobne"
x$Profil_ad[x$Profil_ad == "rynku surowców"] <- "akcjopodobne"
x$OZW[is.na(x$OZW)] <- 0
x$OZW01 <- x$OZW
x$OZW01[!(x$OZW01 == 0)] <- 1
x$TER <- as.numeric(x$TER)
x$OZN <- as.numeric(x$OZN)


pd <- plm.data(x, indexes = c("Name_time", "Profil_ad"))

pool <- plm(pd$OB ~ pd$yearly_rr + pd$yearly_std
            + pd$es + pd$eg + pd$sr + pd$var_5 + pd$var_95
            + pd$Profil_ad + pd$Lokalizacja + pd$TFI
            + pd$Aktywa + pd$Wiek + pd$Rok
            + pd$PW + pd$NW
            + pd$OZZ + pd$OZN + pd$OZU + pd$TER + x$OZW01, 
            data = pd, model = "pooling")
summary(pool)
plmtest(pool)

pool <- plm(pd$OB ~ pd$sr + pd$var_5
            + pd$Profil_ad + pd$Wiek
            + pd$PW + pd$NW
            + pd$OZN + pd$OZW01, 
            data = pd, model = "pooling")


lm <- lm(pd$OB ~ pd$yearly_rr + pd$yearly_std
            + pd$es + pd$eg + pd$sr + pd$var_5 + pd$var_95
            + pd$Profil_ad + pd$Lokalizacja
            + pd$Aktywa + pd$Wiek + pd$Rok
            + pd$PW + pd$NW
            + pd$OZN + pd$OZU + pd$OZW01, 
            data = x)

ols_step_backward(lm, details = F, prem = 0.05)

lm <- lm(pd$OB ~ pd$sr + pd$var_5
         + pd$Profil_ad + pd$Wiek
         + pd$PW + pd$NW
         + pd$OZN + pd$OZW01, 
         data = x)
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


