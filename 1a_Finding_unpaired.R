setwd("C:/Users/Alicja/Documents/Doktorat/Rozprawa doktorska/Panel_analysis/Data")
x <- read.csv("pd.profile.csv", as.is = T)

a <- read.csv("data_analizy_18.csv", as.is = T)
b <- read.csv2("stooq_17_18.csv", as.is = T)
c <- read.csv("data_analizy_17.csv", as.is = T)

d <- merge(a, b, by.x = 1, by.y = "name")
e <- merge(c, d, by="Nazwa_IZFiA")

dif <- setdiff(d$Nazwa_IZFiA, e$Nazwa_IZFiA)