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
  # date: string, mm/dd/yy
  # method: int
  #   0 days: num trailing days
  #   1 weeks: current day of the week for num trailing weeks 
  #   2 months: current day of the month for num trailing months?
  #   3 years: current day of the year for num trailing years?
  #num: int number of days to get
  #returns: data.frame with days
  
  
  wd = paste(getwd(), '/proc_data/', ticker, sep = '')
  tmp = strptime(paste(date, "09:29"), "%m/%d/%y %H:%M", 'EST')#9:29 bc non-inclusive select
  
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


#startdate, method, num_periods, ticker 

price_options <- function(type, reg_data, date){
  #prices options 
  #0th min is 9:30
  
  #load stock data
  data = load_data(ticker, date, 0, 0)
  
  #flatten regimes
  mat = matrix(data = NA, nrow = 390, ncol = 2)
  
  for(i in 1:nrow(reg_data)){
    mat[reg_data[i, 'Start.Time']:reg_data[i,'End.Time'], 1] = reg_data[i, 'Mean']
    mat[reg_data[i, 'Start.Time']:reg_data[i,'End.Time'], 2] = reg_data[i, 'Standard.Deviation']
  }
  
  
  
  #give rate, calculate strike
  
  
  #for 1 day
    
    #for each price
      #price BS for each regime 
        #option price = min weighted BS price
  
  
}



####TESTING STUFF#######
data = load_data('FB', '4-14-20', 0, 0)
data = as.vector(data[,1], mode = 'numeric')

regs = read.table(paste(getwd(), '/regimes/FB/FB_4-14-20_0_10.csv', sep = ''), header = TRUE)

mat = matrix(data = NA, nrow = 390, ncol = 3)
mat[,1] = data

for(i in 1:nrow(regs)){
  mat[regs[i, 'Start.Time']:regs[i,'End.Time'], 2] = regs[i, 'Mean']
  mat[regs[i, 'Start.Time']:regs[i,'End.Time'], 3] = regs[i, 'Standard.Deviation']
}



#weighted by rest of regimes - fuck


###TRY AGAIN###

data = load_data('FB', '4-14-20', 0, 0)
data = as.vector(data[,1], mode = 'numeric')

regs = read.table(paste(getwd(), '/regimes/FB/FB_4-14-20_0_10.csv', sep = ''), header = TRUE)

#BS params##
strike = data[1]
rfr = 0.01
type = 'call'
tte = 390 #* (days) #needs to be number of names

mat = matrix(data = NA, nrow = 390, ncol = 2)
mat[,1] = data

reg_time = matrix(data = NA, nrow = nrow(regs), ncol = 1)

for(i in 1:nrow(regs)){
  #print(length(rep.int(i, regs[i,'End.Time']- regs[i, 'Start.Time'])))
  mat[regs[i, 'Start.Time']:regs[i,'End.Time'], 2] = rep.int(i, regs[i,'End.Time']- regs[i, 'Start.Time'] + 1)
  reg_time[i,1] = regs[i,'End.Time'] - regs[i, 'Start.Time']#sums to 389 min
}




#pricing loop
for(i in 1:nrow(mat)){
  reg_data = 
  
  #stockPrice -k, strikePrice-k , riskFreeRate-k , timeToExpiry, volatility, type
  #how do we know time until exp?
  #what vol do we need
  
  #do this for each reg, then weighted avg
  BlackScholes(mat[i,1], 
               strike, 
               rfr, 
               tte,#this is tough 
               regs[mat[i,2], 'Standard.Deviation'], 
               type)
  
  
  #sub times
  reg_time[mat[i,2], 1] = reg_time[mat[i,2], 1] - 1
  tte = tte - 1
}

sum(reg_time[,1])


       