#setwd("C:/Users/Thom Lonon/Downloads/Pulaski/Sparky/")
#getwd()

fileRead <- function(ticker,folder) {
  #Read in file
  Goog=read.csv(paste(folder,"/",ticker,sep = ''))
  if (nrow(Goog) < 5000) {
    return("skip")
  }
  head(Goog)
  
  #Goog$time = as.POSIXct(as.character(Goog$time))
  
  #stdtime=unclass(Goog$time) #Convert to integers
  #stdtime=stdtime/60 #Convert to a base-minute standard
  #stdtime=temp4-1231 #Shifting beginning of day to Index 1
  #stdtime=stdtime%%1440  #Modulo by minutes in a day
  
  h = strftime(Goog$time,format = "%H:%M:%S")
  stripTime = as.character(h)
  h = as.POSIXct.POSIXlt(as.POSIXlt.character(h,format = "%H:%M:%S"))
  h = unclass(h)
  h = h/60 #Adjust to increment by minute
  if (stripTime[1] != "09:31:00") {
    #Russian Adjustment
    mod = unclass(as.POSIXct.POSIXlt(as.POSIXlt.character("16:31:00",format = "%H:%M:%S")))
    mod = mod/60
  } else {
    mod = h[1]
  }
  h = h%%mod
  stdtime = h
  b = 390/5
  
  #stdtime now represents the "basket" (five minute interval) to which the given data belongs
  baskets = rep(NA,length(h))
  for (i in 1:b) {
    cutoff = 5*i
    for (j in 1:length(baskets)) {
      if (is.na(baskets[j]) && h[j] < cutoff) {
        baskets[j] = i
      }
    }
  }
  
  #Cleaned Dataframe
  Goog=cbind(Goog,stripTime,baskets,stdtime)
  NewGoog = Goog[which(!is.na(Goog$baskets)),]
  
  #NewGoog = NewGoog[which(NewGoog$volume >= summary(NewGoog$volume)[[2]]),]
  
  for (i in 1:3) {
    num2=length(NewGoog$stdtime)
    timediff=NewGoog$stdtime[2:num2]-NewGoog$stdtime[1:(num2-1)]
    timediff=c(timediff[1],timediff)
    if (!is.null(NewGoog$timediff)) {
      NewGoog$timediff = timediff
    } else {
      NewGoog=cbind(NewGoog,timediff)
    }
    if (i != 3) {
      indices = NULL
      for (j in 1:nrow(NewGoog)) {
        if (NewGoog$timediff[j] == 1 || NewGoog$timediff[j] == (-5*b)) {
          indices = c(indices,j)
        }
      }
      NewGoog = NewGoog[indices,]
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
  
  l = matrix(ncol = 5,nrow=1)
  colnames(l) = c("Regime","Start Time","End Time","Mean","Standard Deviation")
  counter = 1
  carryOver = NULL
  for (i in 2:b) {
    set1 = rbind(carryOver,NewGoog[which(NewGoog$baskets == (i-1)),])
    s1 = as.character(set1$stripTime)
      
    set2 = NewGoog[which(NewGoog$baskets == i),]
    s2 = as.character(set2$stripTime)
    
    if (ks.test(set1$logret,set2$logret) < 0.05) {
      #New Regime
      temprow = c(counter,min(s1),max(s1),mean(set1$logret),sd(set1$logret))
      
      l = rbind(l,temprow)
      counter = counter + 1
      carryOver = NULL
    } else {
      #Same Regime
      if (i == 2) {
        carryOver = rbind(set1,set2)
      } else {
        carryOver = set1
      }
    }
    if (i == b) {
      carryOver = rbind(carryOver,set2)
      if (!is.null(carryOver)) {
        s = as.character(carryOver$stripTime)
        temprow = c(counter,min(s),max(s),mean(carryOver$logret),sd(carryOver$logret))
        l = rbind(l,temprow)
      }
    }
  }
  l[,5] = as.numeric(l[,5])/sqrt((1/(60*6.5*252)))
  l = l[-1,]
  
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
#This Line (109) may require tweaking on the individual user's system. Specific folders have been excluded
#for my project directory
folders = folders[-c(1,8,9,length(folders)-2,length(folders)-1,length(folders))]
for (i in 1:length(folders)) {
  if (!dir.exists(paste("./sortedData3/",folders[i],sep=''))) {
    dir.create(paste("./sortedData3/",folders[i],sep=''))
  }
  l = list.files(folders[i])
  for (j in 1:length(l)) {
    temp = fileRead(l[j],folders[i])
    if (temp != "skip") {
      print(l[j])
      write.csv(temp,paste(getwd(),"/sortedData3/",folders[i],"/",l[j],sep = ''))
    }
  }
}
print(Sys.time() - start)
beep(3)
  