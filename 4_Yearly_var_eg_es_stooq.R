setwd("C:/Users/Alicja/Documents/Doktorat/Rozprawa doktorska/Pobieranie_stooq/Files")
write.table(paste("name", "rr_16", "std_16", "var16_5", "var16_95", "es16", "eg16", 
                  "rr_17", "std_17", "var17_5", "var17_95", "es17", "eg17", sep = ";"), file = "stooq_yearly.csv", sep = ";", row.names = F, col.names = F, quote = F, append = T)

files_csv <- list.files(path=getwd(), pattern=c("*.csv"))
pv <- 0.05
pb <- txtProgressBar(min = 0, max = length(files_csv), style = 3)

sapply(1:length(files_csv), function(i) {
  r <- read.csv2(files_csv[i], as.is = T)
  r <- r[!apply(r == "", 1, all), c(1, 5)]
  r$Data <- as.Date(r$Data, format = "%Y-%m-%d")
  r[,2] <- as.numeric(r[,2])
  name <- substr(files_csv[i], 1, 4)
  if(sum(r$Data >= "2016-01-01" & r$Data<= "2016-12-31") > 200) {
    
    # 2016
    r16 <- r[which(r$Data >= "2016-01-01" & r$Data<= "2016-12-31"),]
    r16$Log_price <- log(r16$Zamkniecie)
    r16$Daily_rr_log <- c(NA, diff(r16$Log_price))
    rr16 <- sum(r16$Daily_rr_log, na.rm = T)
    std16 <- sd(r16$Daily_rr_log, na.rm = T)
  } else { 
    rr16 <- NA
    std16 <- NA
    
  }
  if(sum(r$Data >= "2016-01-01" & r$Data<= "2016-12-31" & sum(r$Data >= "2015-01-01" & r$Data<= "2015-12-31")) > 200) {
    r15.6 <- r[which(r$Data >= "2015-01-01" & r$Data<= "2016-12-31"),]
    r15 <- r[which(r$Data >= "2015-01-01" & r$Data<= "2015-12-31"),]
    d1 <- dim(r15.6)[1]
    d2 <- dim(r15)[1]
    r15.6$Log_price <- log(r15.6$Zamkniecie)
    r15.6$Daily_rr_log <- c(NA, diff(r15.6$Log_price))
    var_r5 <- sapply((d2+1):d1, function(y) 
      sd(r15.6[(y-d2):y, "Daily_rr_log"], na.rm = T) * qnorm(pv)
    )
    es16_r <- sapply((d2+1):d1, function(y){
      r <- r15.6[(y-d2):y, "Daily_rr_log"]
      mean(r[which(r < var_r5[y-d2])], na.rm = T)
      })
    var16_5 <- mean(var_r5, na.rm = T)
    es16 <- mean(es16_r, na.rm = T)
    
    var_r95 <- sapply((d2+1):d1, function(y) 
      sd(r15.6[(y-d2):y, "Daily_rr_log"], na.rm = T) * qnorm(1-pv)
    )
    eg16_r <- sapply((d2+1):d1, function(y){
      r <- r15.6[(y-d2):y, "Daily_rr_log"]
      mean(r[which(r > var_r95[y-d2])], na.rm = T)
      })
    var16_95 <- mean(var_r95, na.rm = T)
    eg16 <- mean(eg16_r, na.rm = T)
  } else {
    var16_5 <- NA
    var16_95 <- NA
    es16 <- NA
    eg16 <- NA
  }
  
  # 2017
  if(sum(r$Data >= "2017-01-01" & r$Data<= "2017-12-31")) {
    r17 <- r[which(r$Data >= "2017-01-01" & r$Data<= "2017-12-31"),]
    r17$Log_price <- log(r17$Zamkniecie)
    r17$Daily_rr_log <- c(NA, diff(r17$Log_price))
    rr17 <- sum(r17$Daily_rr_log, na.rm = T)
    std17 <- sd(r17$Daily_rr_log, na.rm = T)
  } else { 
    rr17 <- NA
    std17 <- NA
  }
  
  if(sum(r$Data >= "2016-01-01" & r$Data<= "2016-12-31") & sum(r$Data >= "2017-01-01" & r$Data<= "2017-12-31")) {
    r16.7 <- r[which(r$Data >= "2016-01-01" & r$Data<= "2017-12-31"),]
    r16 <- r[which(r$Data >= "2016-01-01" & r$Data<= "2016-12-31"),]
    d1 <- dim(r16.7)[1]
    d2 <- dim(r16)[1]
    r16.7$Log_price <- log(r16.7$Zamkniecie)
    r16.7$Daily_rr_log <- c(NA, diff(r16.7$Log_price))
    
    var_r5 <- sapply((d2+1):d1, function(y) 
      sd(r16.7[y-d2:y, "Daily_rr_log"], na.rm = T) * qnorm(pv)
    )
    es17_r <- sapply((d2+1):d1, function(y){
      r <- r16.7[(y-d2):y, "Daily_rr_log"]
      mean(r[which(r > var_r5[y-d2])], na.rm = T)
    })
    var17_5 <- mean(var_r5, na.rm = T)
    es17 <- mean(es17_r, na.rm = T)
    
    var_r95 <- sapply((d2+1):d1, function(y) 
      sd(r16.7[(y-d2):y, "Daily_rr_log"], na.rm = T) * qnorm(1-pv)
    )
    eg17_r <- sapply((d2+1):d1, function(y){
      r <- r16.7[(y-d2):y, "Daily_rr_log"]
      mean(r[which(r > var_r95[y-d2])], na.rm = T)
    })
    var17_95 <- mean(var_r95, na.rm = T)
    eg17 <- mean(eg17_r, na.rm = T)
  } else {
    var17_5 <- NA
    var17_95 <- NA
    es17 <- NA
    eg17 <- NA
  }
  write.table(paste(name,  rr16, std16, var16_5, var16_95, es16, eg16, 
                    rr17, std17, var17_5, var17_95, es17, eg17, sep = ";"), 
              file = "stooq_yearly.csv", append = T, sep = ";", row.names = F, col.names = F, quote = F)
  
  setTxtProgressBar(pb, i)
})

file.rename("C:/Users/Alicja/Documents/Doktorat/Rozprawa doktorska/Pobieranie_stooq/Files/stooq_yearly.csv", 
            "C:/Users/Alicja/Documents/Doktorat/Rozprawa doktorska/Pobieranie_stooq/stooq_yearly.csv")