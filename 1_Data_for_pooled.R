options(scipen = 100)
setwd("C:/Users/Alicja/Documents/Doktorat/Rozprawa doktorska/Panel_analysis/Data")

data_analizy_17 <- read.csv("data_analizy_17.csv", as.is = T)
data_analizy_18 <- read.csv("data_analizy_18.csv", as.is = T)
data_stooq <- read.csv2("stooq_17_18.csv", as.is = T)

data_18 <-  merge(data_analizy_18, data_stooq, by.x = 1, by.y = "name")
data_stooq <- merge(data_analizy_18[, c("X1", "Nazwa_IZFiA", "Nazwa")], data_stooq, by.x = 1, by.y = "name")
data_17 <-  merge(data_analizy_17, data_stooq, by = "Nazwa_IZFiA")
data_17 <- merge(data_17, data_18[, c("Data_utworzenia", "Nazwa_IZFiA", "Lokalizacja", "TFI")], by = "Nazwa_IZFiA")


data_18 <- data_18[, c("Nazwa", "Nazwa_IZFiA", "OB18", "TFI", "Data_utworzenia", 
                       "PW18", "NW18", "Aktywa18", 
                       "Profil18", "OZZ18", "OZN18", "OZU18", "OZW18", "TER18", 
                       "mean_daily_log_rr_17", "std_daily_log_rr_17",  
                       "var17_5", "var17_95", "es17", "eg17", 
                       "yearly_rr_17", "yearly_std_17", "Lokalizacja")]

data_17 <- data_17[, c("Nazwa", "Nazwa_IZFiA", "OB17", "TFI", 
                       "Data_utworzenia", "PW17", "NW17", "Aktywa17", 
                       "Profil17", "OZZ17", "OZN17", 
                       "OZU17", "OZW17", "TER17", 
                       "mean_daily_log_rr_16", "std_daily_log_rr_16",  
                       "var16_5", "var16_95", "es16", "eg16", 
                       "yearly_rr_16", "yearly_std_16", "Lokalizacja")]
data_17$Rok <- "2017"
data_18$Rok <- "2018"
sr.oproc.lokat.17 <- 0.014
sr.oproc.lokat.16 <- 0.015
data_17$sr <- (as.numeric(data_17$yearly_rr_16) - sr.oproc.lokat.16) / as.numeric(data_17$yearly_std_16)
data_18$sr <- (as.numeric(data_18$yearly_rr_17) - sr.oproc.lokat.17) / as.numeric(data_18$yearly_std_17)


colnames(data_17) <- c("Nazwa", "Nazwa_IZFiA", "OB", "TFI", "Data_utworzenia", 
                       "PW", "NW", "Aktywa", 
                       "Profil", "OZZ", "OZN", "OZU", "OZW", "TER", 
                       "mean_daily_log_rr", "std_daily_log_rr",  
                       "var_5", "var_95", "es", "eg", 
                       "yearly_rr", "yearly_std", "Lokalizacja", "Rok", "sr")
colnames(data_18) <- c("Nazwa", "Nazwa_IZFiA", "OB", "TFI", "Data_utworzenia", 
                       "PW", "NW", "Aktywa", 
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
panel.data$Profil <- gsub("inne ", "akcji", panel.data$Profil)
panel.data$Profil <- gsub("rynku surowców ", "akcji", panel.data$Profil)
panel.data$Wiek <- as.numeric(time_length(difftime(Sys.Date(), panel.data$Data_utworzenia), "years"))
panel.data$Name_time <- paste(panel.data$Nazwa, panel.data$Rok, sep = "")

setwd("C:/Users/Alicja/Documents/Doktorat/Rozprawa doktorska/Panel_analysis/Data")
write.csv(panel.data, "Panel.data.csv")

