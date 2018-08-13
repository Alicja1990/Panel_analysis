library(plm)
setwd("C:/Users/Alicja/Documents/Doktorat/Rozprawa doktorska/Panel_analysis/Panel_analysis/Data")
x <- read.csv2("Panel.data.csv", as.is = T, dec = ".")
r <- apply(x[, c(4, 7:9, 11:23, 25:27)], 2, function(y) as.numeric(gsub(",", ".", y)))
x$OB <- r[,1]
x[, 7:9] <- r[, 2:4]
x[, 11:23] <- r[,5:17]
x[, 25:27] <- r[,18:20]

pd <- plm.data(x, indexes = c("Nazwa.IZFiA", "Rok"))

pool <- plm(x$OB ~ x$yearly_rr + x$yearly_std
            + x$es + x$eg + x$Profil 
            + x$Aktywa + x$Lokalizacja
            + x$Wiek
            + x$sr + var_5 + var_95, 
            data = pd, model = "pooling")
summary(pool)

beet <- plm(x$OB ~ x$yearly_rr + x$yearly_std
            + x$es + x$eg + x$Profil 
            + x$Aktywa + x$Lokalizacja
            + x$Wiek
            + x$sr + var_5 + var_95, 
            data = pd, model = "between")
summary(beet)

fixed <- plm(x$OB ~ x$yearly_rr + x$yearly_std
             + x$es + x$eg + x$Profil 
             + x$Aktywa + x$Lokalizacja
             + x$Wiek
             + x$sr + var_5 + var_95, 
            data = pd, model = "within")
summary(fixed)

rand <- plm(x$OB ~ x$yearly_rr + x$yearly_std
               + x$es + x$eg + x$Profil 
               + x$Aktywa + x$Lokalizacja
               + x$Wiek
               + x$sr + x$var_5 + x$var_95, 
              data = pd, model = "random")
summary(rand)
+ x$Pierwsza.wpłata

plmtest(pool)
pFtest(fixed, pool)
phtest(fixed, rand)