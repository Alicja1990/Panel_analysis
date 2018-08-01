setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Pobieranie_analizy")

data_analizy_18 <- read.csv2("mydata1_corrected.csv", as.is = T)

data_analizy_18[, c(5, 19:20, 22, 26:30)] <- apply(data_analizy_18[, c(5, 19:20, 22, 26:30)], 2, as.numeric)
data_analizy_18$Data.utworzenia <- as.Date(data_analizy_18$Data.utworzenia, format = "%d.%m.%Y")
colnames(data_analizy_18)[names(data_analizy_18) == "Opłaty.bieżące"] <- "OB18"
colnames(data_analizy_18)[names(data_analizy_18) == "Aktywa"] <- "Aktywa18"
colnames(data_analizy_18)[names(data_analizy_18) == "Profil"] <- "Profil18"
colnames(data_analizy_18)[names(data_analizy_18) == "OZZ"] <- "OZZ18"
colnames(data_analizy_18)[names(data_analizy_18) == "OZN"] <- "OZN18"
colnames(data_analizy_18)[names(data_analizy_18) == "OZU"] <- "OZU18"
colnames(data_analizy_18)[names(data_analizy_18) == "OZW"] <- "OZW18"
colnames(data_analizy_18)[names(data_analizy_18) == "TER"] <- "TER18"
colnames(data_analizy_18)[names(data_analizy_18) == "Pierwsza.wpłata"] <- "PW18"
colnames(data_analizy_18)[names(data_analizy_18) == "Następna.wpłata"] <- "NW18"

setwd("C:/Users/Alicja/Documents/Doktorat/Rozprawa doktorska/Pobieranie_stooq/Pobieranie_stooq")
data_stooq <- read.csv2("test1.csv", as.is = T)
data_analizy_18 <-  merge(data_analizy_18, data_stooq, by.x = 1, by.y = "name")
data_analizy_18 <- data_analizy_18[, c("OZZ18", "OZN18", "OZU18", "OZW18", "TER18", "OB18", 
                                       "Profil18", "PW18", "NW18", "Aktywa18", "TFI", 
                                       "Lokalizacja", "Nazwa.IZFiA", "mean_daily_log_rr_16", 
                                       "std_daily_log_rr_16", "var16_5", "var16_95", "es16", "eg16", 
                                       "mean_daily_log_rr_17", "std_daily_log_rr_17",  "var17_5", 
                                       "var17_95", "es17", "eg17")]

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

data$Profil18 <- gsub("mieszane polskie", "mieszane", data$Profil18)
data$OZW17 <- gsub("%", "", data$OZW18)
data$Profil18 <- gsub("mieszane ", "mieszane", data$Profil18)
data <- data[!(data$Profil18 == "inne" | data$Profil18 == "rynku surowców"),]
data$OB18 <- gsub(",", ".", data$OB18)
data$Aktywa18 <- gsub(",", ".", data$Aktywa18)


setwd("C:/Users/Alicja/Documents/Doktorat/Rozprawa doktorska/Panel_analysis/Panel_analysis/Data")
write.csv2(data, "pd.profile.csv")

