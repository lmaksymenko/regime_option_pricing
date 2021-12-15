#creates the regimes and saves them

library(lubridate)

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
  #num: int
  #returns: 
  
  ###
  ticker = 'FB'
  date = '4/30/20'
  ####
  
  wd = paste(getwd(), '/', ticker, sep = '')
  tmp = strptime(paste(date, "09:29"), "%m/%d/%y %H:%M", 'EST')
  #data list of stuff
  
  paste(wd, '/', ticker, '_', tmp$year-100, sep = '')
  
  
  
  get_data <- function(ticker, dates, wd){
    #The only point of this function is querying the day data for a given list of days
    
    #sub 100 bc posix 
    df = read.csv(paste(wd, '/', ticker, '_', date$year-100, sep = ''))
    #turn datetime back into posix
    
    upper = 
    lower = 
    
    tmp = subset(data, data$datetime > lower & data$datetime < upper)
    return(tmp['close'])
  }
  

  
  #daily
  if (method == 0){
    days = lapply(c(0:num), function(x) c(tmp - days(x), tmp - days(x) + minutes(392)))
    get_data(ticker, dates, wd)
  }
  
  #weekly
  if (method == 1){
    days = lapply(c(0:num), function(x) c(tmp - weeks(x), tmp - weeks(x) + minutes(392)))

  }
  
  #monthly
  if (method == 2){
    #DOESNT WORK BC MONTHS HAVE DIFF DAYS
    #days = lapply(c(0:5), function(x) c(tmp - months(x), tmp - months(x) + minutes(392)))
    #work around
    days = lapply(c(0:num), function(x) c(tmp - weeks(x * 4), tmp - weeks(x * 4) + minutes(392)))

    for (day in days){
      #pull files
    }
  }
  
  #yearly
  if (method == 3){
    #we add a day bc of annual day drift 
    days = lapply(c(0:num), function(x) c(tmp - years(x) + days(x), tmp - years(x) + days(x) + minutes(392)))
    
    for (day in days){
      #pull files
    }
   
  }
  
  #return the dataframe
  return()
  
}


create_regimes <- function(data){
  #creates regimes based on matrix input
  
  #convert data to log returns
  
  
  #
  
  
  
}


#########TESTING


x = data$datetime[1] 
x = x - days(1) #or %m-%
x$yday
x
names(unclass(x))



###Subsetting the data
d = "1/3/20"
lower = strptime(paste(d, "09:29"), "%m/%d/%y %H:%M")
upper = lower + minutes(392)



new = subset(data, data$datetime > lower & data$datetime < upper)

paste( 120 == data$datetime[505]$year) 


unclass(tmp)
