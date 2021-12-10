library(quantmod)

BlackScholes <- function(stockPrice, strikePrice, riskFreeRate, timeToExpiry, volatility, type){
  
  if(type=="call"){
    d1 <- (log(stockPrice/strikePrice) + (riskFreeRate + volatility^2/2)*timeToExpiry) / (volatility*sqrt(timeToExpiry))
    d2 <- d1 - volatility*sqrt(timeToExpiry)
    
    value <- stockPrice*pnorm(d1) - strikePrice*exp(-riskFreeRate*timeToExpiry)*pnorm(d2)
    return(value)}
  
  if(type=="put"){
    d1 <- (log(stockPrice/strikePrice) + (riskFreeRate + volatility^2/2)*timeToExpiry) / (volatility*sqrt(timeToExpiry))
    d2 <- d1 - volatility*sqrt(timeToExpiry)
    
    value <-  (strikePrice*exp(-riskFreeRate*timeToExpiry)*pnorm(-d2) - stockPrice*pnorm(-d1))
    return(value)}
}