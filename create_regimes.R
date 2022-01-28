#creates the regimes and saves them

library(lubridate)
library(quantmod)

#dynamic wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load_data <- function (ticker, date, method, num){
  # loads the data for the ticker
  # ticker: string, all caps, no extra spaces
  # date: string, mm/dd/yy
  # method: int
  #   0 days: num trailing days
  #   1 weeks: current day of the week for num trailing weeks 
  #   2 months: current day of the month for num trailing months?
  #   3 years: current day of the year for num trailing years?
  #num: int number of units to get
  #returns: data.frame with days
  
  
  wd = paste(getwd(), '/proc_data/', ticker, sep = '')
  tmp = strptime(paste(date, "09:29"), "%m/%d/%y %H:%M", 'EST')

  
  get_data <- function(ticker, dates, wd){
    #function is query the day data for a given list of days
    
    full_dt = read.csv(wd)
    full_dt$datetime = strptime(full_dt$datetime, '%Y-%m-%d %H:%M:%S', 'EST')

    fin_d = data.frame(matrix(ncol = 0, nrow = 390)) #need matrix bc dataframe needs defined row num 
    
    for (d in dates){
      upper = d[2]
      lower = d[1]
      
      tmp_dt = subset(full_dt, full_dt$datetime > lower & full_dt$datetime < upper)
      
      d_tmp = format(tmp_dt$datetime[1], '%Y-%m-%d')
      print(paste('added: ', d_tmp))
      fin_d[, d_tmp] = tmp_dt$close 
      #if a day doesnt have all the minutes it will fail (auto integrity check)
    }

    return(fin_d)
  }
  
  #currently only the TRAILING is implemented
  
  #daily
  if (method == 0){
    dates = lapply(c(0:num), function(x) c(tmp - days(x), tmp - days(x) + minutes(392)) )
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
    #we add a day bc of annual day drift 
    dates = lapply(c(0:num), function(x) c(tmp - years(x) + days(x), tmp - years(x) + days(x) + minutes(392)))
  }
  
  #return the dataframe
  return(get_data(ticker, dates, wd))
  
}


create_regimes <- function(data){
  #creates regimes based on matrix input
  
  wd = paste(getwd(), '/regimes/', ticker, sep = '')
  
  if (!(dir.exists(wd))) {
    dir.create(wd)
  }
  
  #log returns
  log_data = data.frame(matrix(ncol = ncol(data), nrow = 389))
  colnames(log_data) = colnames(data)
  
  for (col in colnames(data)){
    #adding 1 to avoid NaNs from neg logs
    log_data[,col] = log( c(diff(data[,col])/data[,col][-1]) + 1 )
    
  }
  
  #regimes
  
}
####In progress stuff outside function


t = load_data('FB', '4/29/20', 0, 2)
colnames(t)

data = t


log_data = data.frame(matrix(ncol = ncol(data), nrow = 389))
colnames(log_data) = colnames(data)
for (col in colnames(data)){
  #adding 1 to avoid NaNs from neg logs
  log_data[,col] = log( c(diff(data[,col])/data[,col][-1]) + 1 )
}
#regimes

#params
min_sample = 5 #number of minutes we sample
day_scale = c(5,4,3,2,1,0) #how we discount the previous days


#for loop vars
regs = matrix(ncol = 5, nrow=1)
colnames(regs) = c("Regime","Start Time","End Time","Mean","Standard Deviation")

curr_reg = c()

log_data[1]

#get_data = function


#need to create the sets of data for the test to run
for (i in seq(from = 0, to = 389, by = 5)){
  
  for(j in 1:ncol(log_data)){
    log_data[1:5, j]
    
  }
  curr_reg = rbind(curr_reg, datai-1)
  
  
}


create_regimes(t)
