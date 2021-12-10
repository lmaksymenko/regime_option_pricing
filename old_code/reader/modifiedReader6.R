library(timeDate)
library(lubridate)
library(xlsx)

# setwd("C:/Users/user/Desktop/2021ResearchData")


fileRead <- function(ticker,folder) {
  #Read in file
  data = read.csv(paste(folder,"/",ticker, ".csv", sep = ''))
  if (nrow(data) < 5000) {
    return("skip")
  }
  # intialize new data sets
  time = c()
  date = c()
  close = c()
  stdtime = c()
  
  
  h = strftime(data$time, format = "%H:%M:%S")
  
  
  # Seeing if data time is standardized 
  if (substring(h[1], 1, 5) != "09:31") {
    h <- strftime(ymd_hms(data$time) - dhours(3), format = "%H:%M:%S")
  }
  
  d = as.Date(data$time)
  p = isWeekday(d, wday = 1:5)
  
  # Create a daily sequence of times for each minute in the open trading day
  # Y/M/D do not matter as they are stripped, and are only present as to fill the parameter
  openHours = strftime(seq.POSIXt(as.POSIXct("2015-08-01 09:31:00"), as.POSIXct("2015-08-01 16:00:00"),by = "1 min"), format = "%H:%M")
  cnt = 0
  
  # Getting only trading times
  
  for (i in 1:length(d)) {
    if (p[i]) {
      if (substring(h[i], 1, 5) %in% openHours){
        time = c(time, h[i])
        date = c(date, d[i])
        close = c(close, data$close[i])
        stdtime = c(stdtime, cnt)
      }
      cnt = cnt + 1
      if (cnt == 390) {
        cnt = 0
      }
    }
  }
  
  
  b = 390 / 5
  baskets = rep(NA,length(stdtime))
  
  t = 1
  
  for (h in 1:b) {
    while (t <= (5 * h)) {
      baskets[which(substring(time, 1, 5) == openHours[t])] = h
      t = t + 1
    }
  }
  
  df = data.frame(time, as.Date(date, origin = '1970-01-01'), close, stdtime, baskets)
  colnames(df) = c("Time", "Date", "Close", "stdtime", "baskets")
  
  return(df)
}



extraction <- function(day, df, style) {
  # intialize new data sets
  time = c()
  close = c()
  baskets = c()
  
  h = strftime(strptime(df$Time, format = "%H:%M:%S"), format = "%H:%M:%S")
  d = as.Date(day, format = "%m/%d/%Y")
  
  if (style == 1) {
    # Trailing 30
    s = d - days(30)
    period = c(as.Date(seq.POSIXt(as.POSIXct(s), as.POSIXct(d - days(1)), by = "1 day")))
    for (i in 1:nrow(df)) {
      if (as.Date(df$Date[i]) %in% period) {
        time = c(time, h[i])
        close = c(close, df$Close[i])
        baskets = c(baskets, df$baskets[i])
      }
    }
  }
  
  if (style == 2) {
    # Multi-year
    s = d - days(5)
    period = c(as.Date(seq.POSIXt(as.POSIXct(s), as.POSIXct(d - days(1)), by = "1 day")))
    for (i in 1:nrow(df)) {
      if (as.Date(df$Date[i]) %in% period) {
        time = c(time, h[i])
        close = c(close, df$Close[i])
        baskets = c(baskets, df$baskets[i])
      }
    }
  }
  
  data = data.frame(time, close, baskets)
  colnames(data) = c("Time", "Close", "baskets")
  
  return(data)
}



regimeRead <- function(df) {
  #Regime creating function
  
  #Log Returns
  num2 = nrow(df)
  logret = log(df$Close[2:num2]/df$Close[1:(num2-1)])
  logret = c(0,logret)
  df = data.frame(df,logret)
  #head(df)
  #summary(df)
  
  #Comparison between times
  
  l = matrix(ncol = 5, nrow=1)
  colnames(l) = c("Regime","Start Time","End Time","Mean","Standard Deviation")
  counter = 1
  carryOver = NULL
  for (k in 2:78) {
    set1 = rbind(carryOver,df[which(df$baskets == (k-1)),])
    s1 = as.character(set1$Time)
    
    set2 = df[which(df$baskets == k),]
    s2 = as.character(set2$Time)
    
    if (ks.test(set1$logret,set2$logret) < 0.05) {
      #New Regime
      temprow = c(counter,min(s1),max(s1),mean(set1$logret),sd(set1$logret))
      
      l = rbind(l,temprow)
      counter = counter + 1
      carryOver = NULL
    } else {
      #Same Regime
      carryOver = set1
    }
    if (k == 78) {
      carryOver = rbind(carryOver,set2)
      if (!is.null(carryOver)) {
        s = as.character(carryOver$Time)
        temprow = c(counter,min(s),max(s),mean(carryOver$logret),sd(carryOver$logret))
        l = rbind(l,temprow)
      }
    }
  }
  l[,5] = as.numeric(l[,5])/sqrt((1/(60*6.5*252)))
  l = l[-1,]
  
  return(l)
}


library(beepr)

start = Sys.time()

# Date to compare two regime models from
x = "9/1/"
sum = 0

# Set up for my directory - may require changes based on users directory
folders = list.dirs(".", FALSE, FALSE)[2:7]

# Creating necessary storage
if (!(dir.exists(paste(getwd(), "/regimes", sep='')))) {
  dir.create(paste(getwd(), "/regimes", sep=''))
}


# Reading through data files and storing the regimes
# Also stores the extracted data from previous years in a new folder
for (i in 1:length(folders)) {
  folder = folders[i]
  stonks = list.files(folder)
  for (j in 1:length(stonks)){
    # getting only the ticker - ".csv" is attached to end
    stonk = substr(stonks[j], 1, nchar(stonks[j]) - 4)
    info = fileRead(stonk, paste(getwd(), "/", folder, sep = ''))
    
    if (class(info) == "data.frame") {
      # Only previous years
      if (i != length(folders)) {
        good_info = extraction(paste(x, substr(folder, nchar(folder) - 3, nchar(folder)), sep = ''), info, 2)
        print(nrow(good_info))
        sum = sum + nrow(good_info)
        if (!file.exists(paste(getwd(), "/multipleYear/", stonk, ".csv", sep = ''))) {
          write.csv(good_info,  paste(getwd(), "/multipleYear/", stonk, ".csv", sep = ''), row.names = FALSE)
        }
        else {
          write.table(good_info, file = paste(getwd(), "/multipleYear/", stonk, ".csv", sep = ''), sep = ",", append = TRUE,
                      quote = FALSE, col.names = FALSE, row.names = FALSE)
        }
      }
       # Final folder indicates final year
      else {
        trail = extraction(paste(x, substr(folder, nchar(folder) - 3, nchar(folder)), sep = ''), info, 1)
        print(nrow(trail))
        sum = sum + nrow(trail)
          
        write.xlsx(regimeRead(trail), file = paste(getwd(), "/regimes/", stonk, ".xlsx", sep=''), 
                     sheetName = "Trailing", col.names = TRUE, row.names = TRUE)
        write.xlsx(regimeRead(read.csv(paste(getwd(), "/multipleYear/", stonk, ".csv", sep=''))), 
                     file = paste(getwd(), "/regimes/", stonk, ".xlsx", sep=''), 
                     sheetName = "Multiple", append = TRUE, col.names = TRUE, row.names = TRUE)
      }
    }
  }
}
print(Sys.time() - start)
beep(3)