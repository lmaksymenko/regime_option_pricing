#setwd("C:/Users/Thom Lonon/Downloads/Pulaski/Sparky/")
#getwd()

fileRead <- function(ticker,folder) {
  #Read in file
  Goog=read.csv(paste(folder,"/",ticker,sep = ''))
  if (nrow(Goog) < 1000) {
    return("skip")
  }
  head(Goog)
  
  Goog$time = as.POSIXct(as.character(Goog$time))
  
  stdtime=unclass(Goog$time) #Convert to integers
  stdtime=stdtime/60 #Convert to a base-minute standard
  #stdtime=temp4-1231 #Shifting beginning of day to Index 1
  #stdtime=stdtime%%1440  #Modulo by minutes in a day
  
  #Cleaned Dataframe
  Goog=cbind(Goog,stdtime)
  NewGoog = Goog
  
  NewGoog = NewGoog[which(NewGoog$volume >= summary(NewGoog$volume)[[2]]),]
  
  for (i in 1:3) {
    num2=length(NewGoog$stdtime)
    timediff=NewGoog$stdtime[2:num2]-NewGoog$stdtime[1:(num2-1)]
    timediff=c(timediff[1],timediff)
    if (ncol(NewGoog) == 9) {
      NewGoog$timediff = timediff
    } else {
      NewGoog=cbind(NewGoog,timediff)
    }
    if (i != 3) {
      NewGoog = NewGoog[which(NewGoog$timediff == 1),]
    }
  }
  num2 = nrow(NewGoog)
  
  #Log Returns
  logret=log(NewGoog$close[2:num2]/NewGoog$close[1:(num2-1)])
  logret=c(0,logret)
  NewGoog=data.frame(NewGoog,logret)
  #head(NewGoog)
  #summary(NewGoog$timediff)
  
  #Comparison between times
  probs=seq(1:floor(nrow(NewGoog)/5))
  times=rep(0,length(probs))
  listedTimes = NULL
  means = NULL
  stddevs = NULL
  i = 1
  s = 1:5
  l = as.data.frame(matrix(NA,nrow=nrow(NewGoog),ncol=5))
  colnames(l) = c("Time","Indicator","Prob","Mean","SD")
  while (s[length(s)] < nrow(NewGoog)) {
    set1 = NewGoog$logret[s]
    s2 = s[(length(s)-5):length(s)]+5
    set2 = NewGoog$logret[s2]
    
    out=ks.test(na.omit(set1),na.omit(set2))
    
    l[i,1] = as.character(NewGoog$time[s[length(s)]])
    
    if(out$p.value>.05) {
      s = c(s,s2)
      l[i,2] = 0
      l[i,3] = out$p.value
      l[i,4] = mean(set1)
      l[i,5] = sd(set1)
    } else {
      l[i,2] = 1
      l[i,3] = out$p.value
      l[i,4] = mean(set2)
      l[i,5] = sd(set2)
      s = s2
    }
    i = i+1
  }
  
  #probs
  #times
  
  # set1=NewGoog$logret[NewGoog$stdtime<=5]
  # Temp=NewGoog[NewGoog$stdtime>5,]
  # set2=Temp$logret[(Temp$stdtime<=10)]
  # 
  # ks.test(set1,set2)
  # 
  # set1=c(set1,set2)
  # Temp=NewGoog[NewGoog$stdtime>10,]
  # set2=Temp$logret[(Temp$stdtime<=15)]
  # out=ks.test(set1,set2)
  
  return(l)
}

library(beepr)

start = Sys.time()
folders = list.dirs(".",FALSE,FALSE)[-1]
folders = folders[-c(1,8,9,length(folders))]
for (i in 1:length(folders)) {
  if (!dir.exists(paste("./sortedData/",folders[i],sep=''))) {
    dir.create(paste("./sortedData/",folders[i],sep=''))
  }
  l = list.files(folders[i])
  for (j in 1:length(l)) {
    temp = fileRead(l[j],folders[i])
    if (temp != "skip") {
      write.csv(temp,paste(getwd(),"/sortedData/",folders[i],"/",l[j],sep = ''))
    }
  }
}
print(Sys.time() - start)
beep(3)
