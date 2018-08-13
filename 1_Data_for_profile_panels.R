setwd("C:/Users/Alicja/Documents/Doktorat/Rozprawa doktorska/Panel_analysis/Data")
library(lubridate)
options(scipen = 100)

data_analizy_18 <- read.csv2("Analizy.pl_2018_downl.csv", as.is = T)

data_analizy_18[, c(5, 19:20, 22, 26:30)] <- apply(data_analizy_18[, c(5, 19:20, 22, 26:30)], 2, as.numeric)
data_analizy_18$Data_utworzenia <- as.Date(data_analizy_18$Data_utworzenia, format = "%d.%m.%Y")
colnames(data_analizy_18)[names(data_analizy_18) == "Oplaty_biezace"] <- "OB18"
colnames(data_analizy_18)[names(data_analizy_18) == "Aktywa"] <- "Aktywa18"
colnames(data_analizy_18)[names(data_analizy_18) == "Profil"] <- "Profil18"
colnames(data_analizy_18)[names(data_analizy_18) == "OZZ"] <- "OZZ18"
colnames(data_analizy_18)[names(data_analizy_18) == "OZN"] <- "OZN18"
colnames(data_analizy_18)[names(data_analizy_18) == "OZU"] <- "OZU18"
colnames(data_analizy_18)[names(data_analizy_18) == "OZW"] <- "OZW18"
colnames(data_analizy_18)[names(data_analizy_18) == "TER"] <- "TER18"
colnames(data_analizy_18)[names(data_analizy_18) == "Pierwsza_wplata"] <- "PW18"
colnames(data_analizy_18)[names(data_analizy_18) == "Nastepna_wplata"] <- "NW18"
data_analizy_18 <- data_analizy_18[!is.na(data_analizy_18$X1),]

data_stooq <- read.csv2("stooq_17_18.csv", as.is = T)
data_analizy_18 <-  merge(data_analizy_18, data_stooq, by.x = 1, by.y = "name")
data_analizy_18 <- data_analizy_18[, c("OZZ18", "OZN18", "OZU18", "OZW18", "TER18", "OB18", 
                                       "Profil18", "PW18", "NW18", "Aktywa18", "TFI", 
                                       "Lokalizacja", "Nazwa_IZFiA", "mean_daily_log_rr_16", 
                                       "std_daily_log_rr_16", "var16_5", "var16_95", "es16", "eg16", 
                                       "mean_daily_log_rr_17", "std_daily_log_rr_17",  "var17_5", 
                                       "var17_95", "es17", "eg17", "Data_utworzenia", 
                                       "yearly_rr_17", "yearly_rr_16", "yearly_std_16", "yearly_std_17")]

data_analizy_17 <- read.csv2("Analizy.pl_2017.csv", as.is = T)
data_analizy_17 <- data_analizy_17[, c("Oplata_za_zarzadzanie", "TER", "Oplaty_biezace", "Typ", 
                       "Pierwsza_wplata", "Nastepna_wplata", "Aktywa", "Oplata_za_nabycie", 
                       "Oplata_za_umorzenie", "Oplata_za_wynik", "Nazwa_IZFiA")]
colnames(data_analizy_17)[names(data_analizy_17) == "Oplaty_biezace"] <- "OB17"
colnames(data_analizy_17)[names(data_analizy_17) == "Aktywa"] <- "Aktywa17"
colnames(data_analizy_17)[names(data_analizy_17) == "Oplata_za_zarzadzanie"] <- "OZZ17"
colnames(data_analizy_17)[names(data_analizy_17) == "Oplata_za_nabycie"] <- "OZN17"
colnames(data_analizy_17)[names(data_analizy_17) == "Oplata_za_umorzenie"] <- "OZU17"
colnames(data_analizy_17)[names(data_analizy_17) == "Oplata_za_wynik"] <- "OZW17"
colnames(data_analizy_17)[names(data_analizy_17) == "TER"] <- "TER17"
colnames(data_analizy_17)[names(data_analizy_17) == "Pierwsza_wplata"] <- "PW17"
colnames(data_analizy_17)[names(data_analizy_17) == "Nastepna_wplata"] <- "NW17"
colnames(data_analizy_17)[names(data_analizy_17) == "Typ"] <- "Profil17"


data <- merge(data_analizy_17, data_analizy_18, by = "Nazwa_IZFiA")

data$Profil18 <- gsub("mieszane polskie", "mieszane", data$Profil18)
data$OZW17 <- gsub("%", "", data$OZW18)
data$Profil18 <- gsub("mieszane ", "mieszane", data$Profil18)
data <- data[!(data$Profil18 == "inne" | data$Profil18 == "rynku surowców"),]
data$OB18 <- gsub(",", ".", data$OB18)

data$Aktywa17 <- as.numeric(data$Aktywa17)
data$Aktywa18 <- as.numeric(data$Aktywa18)
data$zm.akt.17.18.proc <- (as.numeric(data$Aktywa18) - as.numeric(data$Aktywa17)) / as.numeric(data$Aktywa17)
data$zm.akt.17.18.flow.proc <- as.numeric(data$zm.akt.17.18.proc) - as.numeric(data$yearly_rr_17)
data$Aktywa18 <- log(as.numeric(data$Aktywa18))
data$Aktywa17 <- log(as.numeric(data$Aktywa17))

sr.oproc.lokat.17 <- 0.014
sr.oproc.lokat.16 <- 0.015
data$sr16 <- (as.numeric(data$yearly_rr_16) - sr.oproc.lokat.16) / as.numeric(data$yearly_std_16)
data$sr17 <- (as.numeric(data$yearly_rr_17) - sr.oproc.lokat.17) / as.numeric(data$yearly_std_17)
data$Wiek <- as.numeric(time_length(difftime(Sys.Date(), data$Data_utworzenia), "years"))

write.csv(data, "pd.profile.csv")

