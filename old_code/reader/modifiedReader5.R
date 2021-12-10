library(timeDate)
library(lubridate)

# setwd("C:/Users/user/Desktop/2021ResearchData")


# work on getting pricing and plotting for duel regimes
fileRead <- function(ticker,folder) {
  #Read in file
  data = read.csv(paste(folder,"/",ticker, sep = ''))
  if (nrow(data) < 5000) {
    return("skip")
  }
  # intialize new data sets
  time = c()
  date = c()
  close = c()
  volume = c()
  stdtime = c()


  h = strftime(data$time,format = "%H:%M:%S")


  # Seeing if data time is standardized 
  if (substring(h[1], 1, 5) != "09:31") {
    h <- strftime(ymd_hms(data$time) - dhours(3), format = "%H:%M:%S")
  }

  d = as.Date(data$time)
  p = isWeekday(d, wday = 1:5)

  # Create a daily sequence of times for each minute in the open trading day
  # Y/M/D do not matter as they are stripped, and are only present as to fill the parameter
  temp = strftime(seq.POSIXt(as.POSIXct("2015-08-01 09:31:00"), as.POSIXct("2015-08-01 16:00:00"),by = "1 min"), format = "%H:%M")
  cnt = 0

  # Getting only trading times

  for (i in 1:length(d)) {
   if (p[i]) {
      if (substring(h[i], 1, 5) %in% temp){
        time = c(time, h[i])
        date = c(date, d[i])
        close = c(close, data$close[i])
        volume = c(volume, data$volume[i])
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
      baskets[which(substring(time, 1, 5) == temp[t])] = h
      t = t + 1
    }
  }
  
  df = data.frame(time, as.Date(date, origin = '1970-01-01'), close, volume, stdtime, baskets)
  colnames(df) = c("Time", "Date", "Close", "Volume", "stdtime", "baskets")

  
  ### Creating regimes is below-might be useful to put it as a seperate function for getting multiple years vs 30 trailing regimes  
  num2 = nrow(df)
  
  #Log Returns
  logret = log(df$Close[2:num2]/df$Close[1:(num2-1)])
  logret = c(0,logret)
  df = data.frame(df,logret)
  #head(df)
  #summary(df)
  
  #Comparison between times
  
  l = matrix(ncol = 5,nrow=1)
  colnames(l) = c("Regime","Start Time","End Time","Mean","Standard Deviation")
  counter = 1
  carryOver = NULL
  for (k in 2:b) {
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
    if (k == b) {
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
folders = list.dirs(".",FALSE,FALSE)[1:6]
#This Line (109) may require tweaking on the individual user's system. Specific folders have been excluded
#for my project directory
for (i in 1:length(folders)) {
  if (!(dir.exists(paste(getwd(), "/sortedData4/",folders[i],sep='')))) {
    dir.create(paste(getwd(), "/sortedData4/",folders[i],sep=''), ,TRUE)
  }
  l = list.files(folders[i])
  for (j in 1:length(l)) {
    temp = fileRead(l[j],folders[i])
    if (temp != "skip") {
      print(l[j])
      write.csv(temp, paste(getwd(),"/sortedData4/",folders[i],"/",l[j], sep = ''))
    }
  }
}
print(Sys.time() - start)
beep(3)
