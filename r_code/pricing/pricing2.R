library(xlsx)
library(ggplot2)
# Read in regimes

files = list.files("C:/Users/user/Desktop/2021ResearchData/regimes")
strike = 120
type = "call"
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
    
    
    price = read.csv(paste(getwd(),"/multipleYear/", substr(file, 1,nchar(file) - 5), ".csv", sep = ''))
    price = price$Close[1:390]
    
    #Using average just so the strikes aren't too high/low for the actual prices
    strike = mean(price)
    
    taumin = 391
    output = matrix(NA,length(price),length(vols))
    
    for (i in 1:390)
    {
      stock = price[i]
      
      taumin = taumin - 1
      tauyear = taumin / (60 * 6.5 * 252)
      output[i,] = BlackScholes(stock,strike,0,tauyear,vols,"call")
    }
    
    # time loop
    
    weights = numeric(length = numreg)
    for (i in 1:numreg) {
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
    weights = weights/(unclass(endTime)[1] - unclass(startTime)[1])
    weights = weights/sum(weights)
    
    # time loop
    
    output2 = output %*% weights
    library(xts)
    output2 = as.xts(output2,order.by = seq(
      as.POSIXct.POSIXlt(as.POSIXlt.character("09:31:00",format = "%H:%M:%OS")),
      as.POSIXct.POSIXlt(as.POSIXlt.character("16:00:00",format = "%H:%M:%OS")),
      60
    ))
    
    if (is.null(total)) {
      total = output2
    } else {
      total = merge.xts(total,output2)
    }
    
    if (a == "Multiple") {
      colnames(total) = sheets
      write.csv(total, paste(getwd(), "/pricing/", substr(file, 1,nchar(file) - 5), ".csv", sep='')) 
    }
  }
}


# Graphing

s = list.files(paste(getwd(), "/pricing/", sep = ''))
for (k in 1:length(s)) {
  data = read.csv(paste(getwd(), "/pricing/", s[k], sep = ''))
  print(ggplot(data = data, aes(x = X))+ geom_line(aes(y = Trailing), color = "blue") + 
          geom_line(aes(y = Multiple), color = "red") +
          labs(x = "Minute", y = "Price") + 
          ggtitle(substr(s[k], 1, nchar(s[k]) - 4)))
}
