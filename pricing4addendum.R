library(xlsx)
# setwd("C:/Users/user/Desktop/2021ResearchData")

pricing <- function(type, days) {
  # Read in regimes
  files = list.files("C:/Users/user/Desktop/2021ResearchData/regimes")
  
  sheets = c("Trailing", "Multiple")
  for (b in 1:length(files)) {
    file = files[b]
    total = NULL
    
    for (a in sheets) {
      regime = read.xlsx(paste(getwd(), "/regimes/", file, sep = ''), sheetName = a)
      
      if (dim(regime)[2] == 2) {
        numreg = 1
        vols = as.numeric(regime[5,2])
      } else {
        numreg = dim(regime)[1]
        vols = as.numeric(as.vector(regime$Standard.Deviation))
      }
      
      
      # Price
      
      BlackScholes <- function(stockPrice, strikePrice, riskFreeRate, timeToExpiry, volatility, type){
        
        if(type == "call"){
          d1 <- (log(stockPrice/strikePrice) + (riskFreeRate + volatility^2/2)*timeToExpiry) / (volatility*sqrt(timeToExpiry))
          d2 <- d1 - volatility*sqrt(timeToExpiry)
          
          value <- stockPrice*pnorm(d1) - strikePrice*exp(-riskFreeRate*timeToExpiry)*pnorm(d2)
          return(value)}
        
        if(type == "put"){
          d1 <- (log(stockPrice/strikePrice) + (riskFreeRate + volatility^2/2)*timeToExpiry) / (volatility*sqrt(timeToExpiry))
          d2 <- d1 - volatility*sqrt(timeToExpiry)
          
          value <-  (strikePrice*exp(-riskFreeRate*timeToExpiry)*pnorm(-d2) - stockPrice*pnorm(-d1))
          return(value)}
      }
      
      
      stocks = 78.9
      K = 78
      r = .01
      tau = .3
      sig = c(.01,0.015,.05,.02)
      
      #BlackScholes(stocks,strike,r, tau, sig, type)
      
      # price loop
      
      price = read.csv(paste(getwd(),"/trailing/", substr(file, 1,nchar(file) - 5), ".csv", sep = ''))
      strike = tail(price$Close, n = 1)
      prices = head(tail(price$Close, n = (390*days + 1)), -1)
      
      # most recent close price is the strike
      
      taumin = 390 * days + 1
      output = matrix(NA, (taumin - 1),length(vols))
      
      for (i in 1:(390 * days))
      {
        stock = prices[i]
        
        taumin = taumin - 1
        tauyear = taumin / (60 * 6.5 * 252)
        output[i,] = BlackScholes(stock,strike,0,tauyear,vols,"call")
      }
      
      # time loop
      #numreg = num regimes
      
      weights = numeric(length = numreg)
      
      for (i in 1:numreg) {
        
        #this is ok
        if (numreg == 1) {
          startTime = as.POSIXct.POSIXlt(as.POSIXlt.character(as.character(regime[2,2]),format = "%H:%M:%OS"))
          endTime = as.POSIXct.POSIXlt(as.POSIXlt.character(as.character(regime[3,2]),format = "%H:%M:%OS"))
        } else {
          
          startTime = as.POSIXct.POSIXlt(as.POSIXlt.character(as.character(regime$Start.Time[i]),format = "%H:%M:%OS"))
          endTime = as.POSIXct.POSIXlt(as.POSIXlt.character(as.character(regime$End.Time[i]),format = "%H:%M:%OS"))
        }
        weights[i] = (unclass(endTime)[1] - unclass(startTime)[1])
      }
      
      if (numreg == 1) {
        startTime = as.POSIXct.POSIXlt(as.POSIXlt.character(as.character(regime[2,2]),format = "%H:%M:%OS"))
        endTime = as.POSIXct.POSIXlt(as.POSIXlt.character(as.character(regime[3,2]),format = "%H:%M:%OS"))
      } else {
        startTime = as.POSIXct.POSIXlt(as.POSIXlt.character(as.character(regime$Start.Time[1]),format = "%H:%M:%OS"))
        endTime = as.POSIXct.POSIXlt(as.POSIXlt.character(as.character(regime$End.Time[numreg]),format = "%H:%M:%OS"))
      }
      
      weights = weights / (unclass(endTime)[1] - unclass(startTime)[1])
      weights = weights / sum(weights) #norms weights?
      
      # time loop
      #worked 
      output2 = output %*% weights
      output2 = rowSums(output2)
      
      library(xts)
      output2 = as.xts(output2,order.by = seq(
                  as.POSIXct.POSIXlt(as.POSIXlt.character("09:31:00",format = "%H:%M:%OS")),
                  as.POSIXct.POSIXlt(as.POSIXlt.character("16:00:00",format = "%H:%M:%OS")),
                  60))
      
      if (is.null(total)) {
        total = output2
      } else {
        total = merge.xts(total,output2)
      }
      
      if (a == "Multiple") {
        colnames(total) = sheets
        write.csv(total, paste(getwd(), "/pricing/", substr(file, 1, nchar(file) - 5), ".csv", sep='')) 
      }
    }
  }
  
  
  # Graphing
  
  s = list.files(paste(getwd(), "/pricing/", sep = ''))
  for (k in 1:length(s)) {
    data = read.csv(paste(getwd(), "/pricing/", s[k], sep = ''))
    plot(data$X, data$Trailing, type = "l",col = "red", xlab = "Minute", ylab = "Price", main =  substr(s[k], 1, nchar(s[k]) - 4),
         ylim = c(0, max(c(data$Trailing, data$Multiple))))
    lines(data$X, data$Multiple, type = "l",col = "blue")
    legend("bottomleft", legend = sheets, col = c("red", "blue"), lty = c(1, 1))
  }
}

pricing("call", 1)


# vector of weights called weights. 
nummin=390*days

timeineach = nummin*weights
timeineach = c(60,30,120,175,5) #test times

regimeswitch = c(0,cumsum(timeineach)[-length(timeineach)])
regimeswitch = data.frame(seq(1:length(regimeswitch)),regimeswitch)

k = regimeswitch[regimeswitch[,2]<i,]
k = k[dim(k)[1],1]
for (i in 1:390)
{
  newweights=timeineach/(sum(timeineach))
  print(c(i,newweights))
  #price as previously done, 
  k=regimeswitch[regimeswitch[,2]<=(i-1),]
  k=k[dim(k)[1],1]
  timeineach[k]=timeineach[k]-1
  

}

#checks?
days=4
weights=newweights


