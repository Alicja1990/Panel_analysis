files_csv <- list.files(path=getwd(), pattern=c("*.csv"))

sum(sapply(1:length(files_csv), function(i) {
  r <- read.csv2(files_csv[i], as.is = T)
  r$Data <- as.Date(r$Data, format = "%Y-%m-%d")
  name <- substr(files_csv[i], 1, 4)
  r <- r[, c(1, 5)]
  as.numeric(r$Data[2]-r$Data[1]) == 6
}))

# tylko 7 przypadków, pomijam je.