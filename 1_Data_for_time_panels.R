options(scipen = 100)
setwd("C:/Users/Alicja/Documents/Doktorat/Rozprawa doktorska/Panel_analysis/Panel_analysis/Data")
data_analizy_18 <- read.csv2("mydata1_corrected.csv", as.is = T)

data_analizy_18[, c(20, 26:30)] <- apply(data_analizy_18[, c(20, 24:28)], 2, as.numeric)
data_analizy_18$Od.kiedy1 <- as.Date(data_analizy_18$Od.kiedy1, format = "%d.%m.%Y")
data_analizy_18$Od.kiedy2 <- as.Date(data_analizy_18$Od.kiedy2, format = "%d.%m.%Y")
data_analizy_18$Od.kiedy3 <- as.Date(data_analizy_18$Od.kiedy3, format = "%d.%m.%Y")
data_analizy_18$Od.kiedy4 <- as.Date(data_analizy_18$Od.kiedy4, format = "%d.%m.%Y")
data_analizy_18$Od.kiedy5 <- as.Date(data_analizy_18$Od.kiedy5, format = "%d.%m.%Y")
data_analizy_18$Data.utworzenia <- as.Date(data_analizy_18$Data.utworzenia, format = "%d.%m.%Y")

setwd("C:/Users/Alicja/Documents/Doktorat/Rozprawa doktorska/Pobieranie_stooq")
data_stooq <- read.csv2("test4.csv", as.is = T)

data_18 <-  merge(data_analizy_18, data_stooq, by.x = 1, by.y = "name")
data_stooq <- merge(data_analizy_18[, c("X1", "Nazwa.IZFiA")], data_stooq, by.x = 1, by.y = "name")

setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wstępna analiza")
data_analizy_17 <- read.csv2("Baza danych_oplaty_2017.csv", as.is = T)

data_17 <-  merge(data_analizy_17, data_stooq[, c("X1", "Nazwa.IZFiA", "mean_daily_log_rr_16", 
                                                  "std_daily_log_rr_16",  "var16_5", "var16_95", 
                                                  "es16", "eg16", 
                                                  "yearly_rr_16", "yearly_std_16")], 
                                                  by = "Nazwa.IZFiA")
data_17 <- merge(data_17, data_18[, c("Data.utworzenia", "Nazwa.IZFiA", "Lokalizacja")], by = "Nazwa.IZFiA")
data_18 <- merge(data_18, data_17[, "Nazwa.IZFiA"], by.x = "Nazwa.IZFiA", by.y = 1, all.y = T)


data_18 <- data_18[, c("Nazwa", "Nazwa.IZFiA", "Opłaty.bieżące", "TFI", "Data.utworzenia", 
                         "Pierwsza.wpłata", "Następna.wpłata", "Aktywa", 
                         "Profil", "OZZ", "OZN", "OZU", "OZW", "TER", 
                         "mean_daily_log_rr_17", "std_daily_log_rr_17",  
                         "var17_5", "var17_95", "es17", "eg17", 
                          "yearly_rr_17", "yearly_std_17", "Lokalizacja")]

data_17 <- data_17[, c("Nazwa.funduszu", "Nazwa.IZFiA", "Opłaty.bieżące", "TFI", 
                         "Data.utworzenia", "Pierwsza.wpłata", "Następna.wpłata", "Aktywa", 
                         "Typ", "Opłata.za.zarządzanie", "Opłata.za.nabycie", 
                         "Opłata.za.umorzenie", "Opłata.za.wynik", "TER", 
                         "mean_daily_log_rr_16", "std_daily_log_rr_16",  
                         "var16_5", "var16_95", "es16", "eg16", 
                        "yearly_rr_16", "yearly_std_16", "Lokalizacja")]
data_17$Rok <- "2017"
data_18$Rok <- "2018"
sr.oproc.lokat.17 <- 0.014
sr.oproc.lokat.16 <- 0.015
data_17$sr <- (as.numeric(data_17$yearly_rr_16) - sr.oproc.lokat.16) / as.numeric(data_17$yearly_std_16)
data_18$sr <- (as.numeric(data_18$yearly_rr_17) - sr.oproc.lokat.17) / as.numeric(data_18$yearly_std_17)


colnames(data_17) <- c("Nazwa", "Nazwa.IZFiA", "OB", "TFI", "Data.utworzenia", 
                         "Pierwsza.wpłata", "Następna.wpłata", "Aktywa", 
                         "Profil", "OZZ", "OZN", "OZU", "OZW", "TER", 
                         "mean_daily_log_rr", "std_daily_log_rr",  
                         "var_5", "var_95", "es", "eg", 
                       "yearly_rr", "yearly_std", "Lokalizacja", "Rok", "sr")
colnames(data_18) <- c("Nazwa", "Nazwa.IZFiA", "OB", "TFI", "Data.utworzenia", 
                         "Pierwsza.wpłata", "Następna.wpłata", "Aktywa", 
                         "Profil", "OZZ", "OZN", "OZU", "OZW", "TER", 
                         "mean_daily_log_rr", "std_daily_log_rr",  
                         "var_5", "var_95", "es", "eg", 
                       "yearly_rr", "yearly_std", "Lokalizacja", "Rok", "sr")
panel.data <- rbind(data_17, data_18)
panel.data$OZW <- gsub("%", "", panel.data$OZW)
panel.data$Profil <- gsub("akcji ", "akcji", panel.data$Profil)
panel.data$Profil <- gsub("dłużne ", "dłużne", panel.data$Profil)
panel.data$Profil <- gsub("mieszane polskie", "mieszane", panel.data$Profil)
panel.data$Profil <- gsub("mieszane ", "mieszane", panel.data$Profil)
panel.data$Profil <- gsub("gotówkowe i pieniężne ", "pieniężne", panel.data$Profil)
panel.data <- panel.data[!(panel.data$Profil == "inne" | panel.data$Profil == "rynku surowców"),]

panel.data$Wiek <- as.numeric(time_length(difftime(Sys.Date(), panel.data$Data.utworzenia), "years"))


setwd("C:/Users/Alicja/Documents/Doktorat/Rozprawa doktorska/Panel_analysis/Panel_analysis/Data")
write.csv2(panel.data, "Panel.data.csv")

