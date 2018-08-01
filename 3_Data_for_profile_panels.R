setwd("C:/Users/Alicja/Documents/Doktorat/Rozprawa doktorska/Panel_analysis/Panel_analysis/Data")
x <- read.csv2("mydata1_corrected.csv", as.is = T)

setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Pobieranie_analizy")

data_analizy_18 <- read.csv2("mydata1_corrected.csv", as.is = T)

data_analizy_18[, c(20, 26:30)] <- apply(data_analizy_18[, c(20, 24:28)], 2, as.numeric)
data_analizy_18$Od.kiedy1 <- as.Date(data_analizy_18$Od.kiedy1, format = "%d.%m.%Y")
data_analizy_18$Od.kiedy2 <- as.Date(data_analizy_18$Od.kiedy2, format = "%d.%m.%Y")
data_analizy_18$Od.kiedy3 <- as.Date(data_analizy_18$Od.kiedy3, format = "%d.%m.%Y")
data_analizy_18$Od.kiedy4 <- as.Date(data_analizy_18$Od.kiedy4, format = "%d.%m.%Y")
data_analizy_18$Od.kiedy5 <- as.Date(data_analizy_18$Od.kiedy5, format = "%d.%m.%Y")
data_analizy_18$Data.utworzenia <- as.Date(data_analizy_18$Data.utworzenia, format = "%d.%m.%Y")
colnames(data_analizy_18)[names(data_analizy_18) == "Opłaty.bieżące"] <- "OB18"
colnames(data_analizy_18)[names(data_analizy_18) == "Aktywa"] <- "Aktywa18"
colnames(data_analizy_18)[names(data_analizy_18) == "Profil"] <- "Profil18"
colnames(data_analizy_18)[names(data_analizy_18) == "OZZ"] <- "OZZ18"
colnames(data_analizy_18)[names(data_analizy_18) == "OZN"] <- "OZN18"
colnames(data_analizy_18)[names(data_analizy_18) == "OZU"] <- "OZU18"
colnames(data_analizy_18)[names(data_analizy_18) == "OZW"] <- "OZW18"
colnames(data_analizy_18)[names(data_analizy_18) == "TER"] <- "TER18"
colnames(data_analizy_18)[names(data_analizy_18) == "Pierwsza.wplata"] <- "PW18"
colnames(data_analizy_18)[names(data_analizy_18) == "Nastepna.wplata"] <- "NW18"

setwd("C:/Users/Alicja/Documents/Doktorat/Rozprawa doktorska/Pobieranie_stooq/Pobieranie_stooq")
data_stooq <- read.csv2("test1.csv", as.is = T)
data_analizy_18 <-  merge(data_analizy_18, data_stooq, by.x = 1, by.y = "name")

setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wstępna analiza")
data_analizy_17 <- read.csv2("Baza danych_oplaty_2017.csv", as.is = T)
data_analizy_17 <- data_analizy_17[, c("Opłata.za.zarządzanie", "TER", "Opłaty.bieżące", "Typ", 
                       "Pierwsza.wpłata", "Następna.wpłata", "Aktywa", "Opłata.za.nabycie", 
                       "Opłata.za.umorzenie", "Opłata.za.wynik", "Nazwa.IZFiA")]
colnames(data_analizy_17)[names(data_analizy_17) == "Opłaty.bieżące"] <- "OB17"
colnames(data_analizy_17)[names(data_analizy_17) == "Aktywa"] <- "Aktywa17"
colnames(data_analizy_17)[names(data_analizy_17) == "Opłata.za.zarządzanie"] <- "OZZ17"
colnames(data_analizy_17)[names(data_analizy_17) == "Opłata.za.nabycie"] <- "OZN17"
colnames(data_analizy_17)[names(data_analizy_17) == "Opłata.za.umorzenie"] <- "OZU17"
colnames(data_analizy_17)[names(data_analizy_17) == "Opłata.za.wynik"] <- "OZW17"
colnames(data_analizy_17)[names(data_analizy_17) == "TER"] <- "TER17"
colnames(data_analizy_17)[names(data_analizy_17) == "Pierwsza.wpłata"] <- "PW17"
colnames(data_analizy_17)[names(data_analizy_17) == "Następna.wpłata"] <- "NW17"
colnames(data_analizy_17)[names(data_analizy_17) == "Typ"] <- "Profil17"


data <- merge(data_analizy_17, data_analizy_18, by = "Nazwa.IZFiA")


data_18 <- data_18[, c("Nazwa", "Nazwa.IZFiA", "Opłaty.bieżące", "TFI", "Data.utworzenia", 
                       "Pierwsza.wpłata", "Następna.wpłata", "Aktywa", 
                       "Profil", "OZZ", "OZN", "OZU", "OZW", "TER", 
                       "mean_daily_log_rr_17", "std_daily_log_rr_17",  
                       "var17_5", "var17_95", "es17", "eg17")]

data_17 <- data_17[, c("Nazwa.funduszu", "Nazwa.IZFiA", "Opłaty.bieżące", "TFI", 
                       "Data.utworzenia", "Pierwsza.wpłata", "Następna.wpłata", "Aktywa", 
                       "Typ", "Opłata.za.zarządzanie", "Opłata.za.nabycie", 
                       "Opłata.za.umorzenie", "Opłata.za.wynik", "TER", 
                       "mean_daily_log_rr_16", "std_daily_log_rr_16",  
                       "var16_5", "var16_95", "es16", "eg16")]
data_17$Rok <- "2017"
data_18$Rok <- "2018"

colnames(data_17) <- c("Nazwa", "Nazwa.IZFiA", "OB", "TFI", "Data.utworzenia", 
                       "Pierwsza.wpłata", "Następna.wpłata", "Aktywa", 
                       "Profil", "OZZ", "OZN", "OZU", "OZW", "TER", 
                       "mean_daily_log_rr", "std_daily_log_rr",  
                       "var_5", "var_95", "es", "eg", "Rok")
colnames(data_18) <- c("Nazwa", "Nazwa.IZFiA", "OB", "TFI", "Data.utworzenia", 
                       "Pierwsza.wpłata", "Następna.wpłata", "Aktywa", 
                       "Profil", "OZZ", "OZN", "OZU", "OZW", "TER", 
                       "mean_daily_log_rr", "std_daily_log_rr",  
                       "var_5", "var_95", "es", "eg", "Rok")
panel.data <- rbind(data_17, data_18)
panel.data$OZW <- gsub("%", "", panel.data$OZW)
panel.data$Profil <- gsub("akcji ", "akcji", panel.data$Profil)
panel.data$Profil <- gsub("dłużne ", "dłużne", panel.data$Profil)
panel.data$Profil <- gsub("mieszane polskie", "mieszane", panel.data$Profil)
panel.data$Profil <- gsub("mieszane ", "mieszane", panel.data$Profil)
panel.data$Profil <- gsub("gotówkowe i pieniężne ", "pieniężne", panel.data$Profil)
panel.data <- panel.data[!(panel.data$Profil == "inne" | panel.data$Profil == "rynku surowców"),]

setwd("C:/Users/Alicja/Documents/Doktorat/Rozprawa doktorska/Panel_analysis/Panel_analysis/Data")
write.csv2(panel.data, "Panel.data.csv")

