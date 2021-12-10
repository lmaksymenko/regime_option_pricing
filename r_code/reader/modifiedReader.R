#setwd("C:/Users/Thom Lonon/Downloads/Pulaski/Sparky/")
#getwd()

fileRead <- function(ticker,folder) {
  #Read in file
  Goog=read.csv(paste(folder,"/",ticker,sep = ''))
  head(Goog)
  
  #Time manipulation
  Goog$time=as.POSIXct(Goog$time)
  stdtime=unclass(Goog$time) #Convert to integers
  temp4=stdtime/60 #Convert to a base-minute standard
  stdtime=temp4-1231 #Shifting beginning of day to Index 1
  stdtime=stdtime%%1440  #Modulo by minutes in a day
  
  #Cleaned Dataframe
  Goog=data.frame(Goog,stdtime)
  if (Goog$stdtime[1] == 1020) {
    #We know we are in the US
    Goog$stdtime = Goog$stdtime - 1020
  }
  NewGoog=Goog[(Goog$stdtime)<390,] 
  head(NewGoog)
  
  
  num2=length(NewGoog$stdtime)
  timediff=NewGoog$stdtime[2:num2]-NewGoog$stdtime[1:(num2-1)]
  timediff=c(0,timediff)
  NewGoog=data.frame(NewGoog,timediff)
  #summary(NewGoog)
  #head(NewGoog)
  
  #Log Returns
  logret=log(NewGoog$close[2:num2]/NewGoog$close[1:(num2-1)])
  logret=c(0,logret)
  NewGoog=data.frame(NewGoog,logret)
  #head(NewGoog)
  NewGoog=NewGoog[NewGoog$timediff==1,]
  #summary(NewGoog$timediff)
  
  #Comparison between times
  times=c()
  probs=seq(1:77)
  for (i in 1:77) {
    set1=NewGoog$logret[NewGoog$stdtime<=(5*i)]
    Temp=NewGoog[NewGoog$stdtime>(i*5),]
    set2=Temp$logret[(Temp$stdtime<=((i+1)*5))]
    out=ks.test(set1,set2)
    probs[i]=out$p.value
    if(probs[i]>.05) {
      times=c(times,i)
    }
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

  l = list(times,probs)
  names(l) = c("times","probs")
  return(l)
}

folders = list.dirs(".",FALSE,FALSE)[-1]
topLevelList = list()
for (i in 1:length(folders)) {
  m = list()
  l = list.files(folders[i])
  for (j in 1:length(l)) {
    temp = fileRead(l[j],folders[i])
    t = list(temp)
    names(t) = l[j]
    m = c(m,t)
  }
  m = list(m)
  names(m) = folder[i]
  topLevelList = c(topLevelList,m)
}
