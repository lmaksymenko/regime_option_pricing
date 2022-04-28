#intakes regimes
#prices options and optputs option data


library(lubridate)
library(bizdays)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

BlackScholes <- function(stockPrice, strikePrice, riskFreeRate, timeToExpiry, volatility, type){
  
  #timeToExp is assumed to be in years
  
  if(type == "call"){
    d1 <- (log(stockPrice/strikePrice) + (riskFreeRate + volatility^2/2)*timeToExpiry) / (volatility*sqrt(timeToExpiry))
    d2 <- d1 - volatility*sqrt(timeToExpiry)
  
    
    value <- stockPrice*pnorm(d1) - strikePrice*exp(-riskFreeRate*timeToExpiry)*pnorm(d2)
    return(value)
  }
  
  if(type == "put"){
    d1 <- (log(stockPrice/strikePrice) + (riskFreeRate + volatility^2/2)*timeToExpiry) / (volatility*sqrt(timeToExpiry))
    d2 <- d1 - volatility*sqrt(timeToExpiry)
    
    value <-  (strikePrice*exp(-riskFreeRate*timeToExpiry)*pnorm(-d2) - stockPrice*pnorm(-d1))
    return(value)
  }
}

load_data <- function (ticker, date, method, num){
  # loads the data for the ticker
  # ticker: string, all caps, no extra spaces
  # date: string, mm-dd-yy
  # method: int
  #   0 days: num trailing days
  #   1 weeks: current day of the week for num trailing weeks 
  #   2 months: current day of the month for num trailing months?
  #   3 years: current day of the year for num trailing years?
  #num: int number of days to get
  #returns: data.frame with days
  
  
  wd = paste(getwd(), '/proc_data/', ticker, sep = '')
  tmp = strptime(paste(date, "09:29"), "%m-%d-%y %H:%M", 'EST')#9:29 bc non-inclusive select
  
  #TODO: Breaks if the day data doesnt exist
  get_data <- function(ticker, dates, wd){
    #function is query the day data for a given list of days
    
    #makes minute indicies 
    inds = lapply(seq(strptime("09:31", "%H:%M"), 
                      strptime("09:30", "%H:%M") + minutes(390),
                      by = 'min'), 
                  function (x) format(ymd_hms(x), '%H:%M'))
    
    full_dt = read.csv(wd)
    full_dt$datetime = strptime(full_dt$datetime, '%Y-%m-%d %H:%M:%S', 'EST')
    
    fin_d = data.frame(matrix(ncol = 0, nrow = 390)) #matrix bc dataframe needs defined row num 
    rownames(fin_d) = inds
    
    cols = c()
    
    for (d in dates){
      #query
      upper = d[2]
      lower = d[1]
      tmp_dt = subset(full_dt, full_dt$datetime > lower & full_dt$datetime < upper)
      rownames(tmp_dt) = tmp_dt$time
      
      #naming
      d_tmp = format(tmp_dt$datetime[1], '%Y-%m-%d')
      cols = c(cols, d_tmp)
      
      if(is.na(d_tmp)){
        print(paste('No Data For:', upper))
      }else{
        print(paste('added: ', d_tmp))
      }
      
      #merge by row index
      fin_d = cbind(fin_d, tmp_dt[,"close"][match(rownames(fin_d), rownames(tmp_dt))])
    }
    colnames(fin_d) = cols
    return(fin_d)
  }
  
  #daily
  if (method == 0){
    #old basic calculation
    #dates = lapply(c(0:num), function(x) c(tmp - days(x), tmp - days(x) + minutes(392)) )
    
    #with business day calculations
    t1 = bizseq(offset(tmp, -num, 'USA 1980-2030'), tmp, 'USA 1980-2030')
    t1 = lapply(c(paste(t1, "09:29")), function (x) strptime(x, "%Y-%m-%d %H:%M", 'EST'))
    dates = lapply(t1, function(x) c(x, x + minutes(392)))
  }
  
  #weekly
  if (method == 1){
    dates = lapply(c(0:num), function(x) c(tmp - weeks(x), tmp - weeks(x) + minutes(392)))
  }
  
  #monthly
  if (method == 2){
    #DOESNT WORK BC MONTHS HAVE DIFF DAYS
    #days = lapply(c(0:5), function(x) c(tmp - months(x), tmp - months(x) + minutes(392)))
    #WORKING ALTERNATIVE, just go 4 weeks back
    dates = lapply(c(0:num), function(x) c(tmp - weeks(x * 4), tmp - weeks(x * 4) + minutes(392)))
  }
  
  #yearly
  if (method == 3){
    #we add a days bc of annual day drift 
    dates = lapply(c(0:num), function(x) c(tmp - years(x) + days(x), tmp - years(x) + days(x) + minutes(392)))
  }
  
  #return the dataframe
  return(data.frame(get_data(ticker, dates, wd)))
}

price_options <- function(regs, pricing_data, strike, rfr, type, days_to_exp){
  #Regimes in the standard regime format
  #Data: Dataframe MUST HAVE Time HH:MM, Price MUST BE FOR 1 STRIKE ONLY (INGNORE FOR NOW)
  
  #regs: regime table in the standard format
  #pricing_data: currently, needs to have `Time` col HH:MM, and `Stock.Quote` with the stock price
  #strike: strike of option
  #rfr: risk free rate: just set to 0 bc we poor
  #type: call or put
  #days_to_exp: days until expiration of the option
  
  
  days = days_to_exp
  vol = as.numeric(unlist(regs['Volatility']))
  data = pricing_data
  row.names(data) = data[["Time"]]
  
  reg_index = c()
  reg_time = c()
  
  for(i in 1:nrow(regs)){
    reg_index = c(reg_index, rep.int(i, regs[i,'End.Time']- regs[i, 'Start.Time']))
    reg_time[i] = (regs[i,'End.Time'] - regs[i, 'Start.Time']) * days #sums to 389 min
  }
  
  reg_time_vectors = list()
  
  #get the time vectors
  for(i in 1:length(reg_index)){
    reg_time[reg_index[i]] = reg_time[reg_index[i]] - 1
    reg_time_vectors = append(reg_time_vectors, list(reg_time))
  }
  #times current come out indexed 1 backwards?
  
  inds = lapply(seq(strptime("09:31", "%H:%M"), 
                    strptime("09:30", "%H:%M") + minutes(389),
                    by = 'min'), 
                function (x) format(ymd_hms(x), '%H:%M'))
  
  reg_time_vectors = data.frame(reg_time_vectors)#, row.names = inds)
  reg_time_vectors = t(reg_time_vectors)
  row.names(reg_time_vectors) = inds 
  #yea we want to have "as if open" so we are slightly off
  
  prices = c()
  
  for(i in row.names(data)){
    stock_price = data[i, "Stock.Quote"]
    time_vec = reg_time_vectors[i,]
    
    bs_tmp = BlackScholes(stock_price,
                          strike, #option strike
                          rfr, #risk free rate
                          time_vec / (60 * 6.5 * 252),#time till exp in years
                          vol, #annualized vol of regimes
                          type)
    
    
    #could use log ret to find regimes, we could use realized vol instead of log returns to get the vol (stretch)
    weight = time_vec / sum(time_vec)
    p = sum(bs_tmp * weight)
    prices = c(prices, p)
  }
  

  
  return(prices)
  
}


april11_scraped_data_pricing <- function(){
    
  files = list.files("raw_data")
  for(i in files){
    regs = read.table(paste(getwd(), '/regimes/', i, '/', i, '_4-11-22_0_5.csv', sep = ''), header = TRUE)
    strike_files = list.files("ScrapebyDateCoStrike/11APRStrike/", 
                              paste("_", i, "_", sep = ""))
    
    for(j in strike_files){
      cat("Processing: ", j, "\n")
      data = read.csv(paste("ScrapebyDateCoStrike/11APRStrike/", j, sep=""))
      
      
      data = subset(data, data$Time < "16:01" & data$Time > "09:29") #remove post close times
      #jank but i dont have time
      str = j
      strike = as.numeric(unlist(regmatches(str,
                                            gregexpr("[[:digit:]]+\\.*[[:digit:]]*",str))))[2]
      
      tmp = price_options(regs, data, strike, 0.01, 'call', 3)
      data["Prices"] = tmp
      write.csv(data, paste("pricing_outputs/11APR/", "pricing_", toString(j), sep = ""))
    }
    
  }
}

#april11_scraped_data_pricing()

april11_scraped_data_pricing_benchmark <- function(){
  
  files = list.files("raw_data")
  for(i in files){
    #regime file name format (modify the ending to be the regimes to be what you want)
    regs = read.table(paste(getwd(), '/regimes/', i, '/', i, '_4-11-22_0_30_d.csv', sep = ''), header = TRUE)
    #folder to read the price, bid, ask data
    strike_files = list.files("ScrapebyDateCoStrike/11APRStrike/", 
                              paste("_", i, "_", sep = ""))
    
    for(j in strike_files){
      cat("Processing: ", j, "\n")
      data = read.csv(paste("ScrapebyDateCoStrike/11APRStrike/", j, sep=""))
      
      
      data = subset(data, data$Time < "16:01" & data$Time > "09:29") #remove post close times
      #jank but i dont have time, extracts the strike from the name
      str = j
      strike = as.numeric(unlist(regmatches(str,
                                            gregexpr("[[:digit:]]+\\.*[[:digit:]]*",str))))[2]
      
      tmp = price_options(regs, data, strike, 0, 'call', 3)
      data["Prices"] = tmp
      write.csv(data, paste("pricing_outputs/11APR_B/", "pricing_", toString(j),"_b", sep = ""))
    }
    
  }
}

april11_scraped_data_pricing_benchmark()
