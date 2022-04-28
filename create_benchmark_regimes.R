#loads data from csvs
#creates the regimes
#most of this code is just duplicated from the create_regimes 

#dynamic wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(lubridate)
library(bizdays)
source("bizdays_config_file.R")

rm(list = ls())


load_data <- function (ticker, date, method, num, mute = F){
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
  
  
  wd = paste(getwd(), '/proc_data/', ticker, ".csv", sep = '')
  tmp = strptime(paste(date, "09:29"), "%m-%d-%y %H:%M", 'EST')#9:29 bc non-inclusive select
  
  #TODO: Breaks if the day data doesnt exist
  get_data <- function(ticker, dates, wd, mute){
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
        if(!mute){
          print(paste('added: ', d_tmp))
        }
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
  return(data.frame(get_data(ticker, dates, wd, mute)))
}


create_benchmark_regimes <- function(data, ticker, save_file){
  #creates regimes based on matrix input
  
  #log returns
  log_data = data.frame(matrix(ncol = ncol(data), nrow = 389))
  colnames(log_data) = colnames(data)
  
  for (col in colnames(data)){
    log_data[,col] = diff(log(data[,col]))
  }
  
  regs = matrix(ncol = 6, nrow = 0)
  colnames(regs) = c("Regime","Start Time","End Time","Mean","Standard Deviation","Volatility")

  tmp = c(1,
          1,
          390,
          mean(unlist(log_data), na.rm = TRUE),
          sd(unlist(log_data), na.rm = TRUE),
          sd(unlist(log_data), na.rm = TRUE) * sqrt(60*6.5*252))
  regs = rbind(regs, tmp)
  
  
  return(regs)
}

#for creating regimes interact with the wrapper instead
create_regimes_wrapper <- function(ticker, date, method, num, save_file, mute_load = T){
  wd = paste(getwd(), '/regimes/', ticker, sep = '')
  
  if (!(dir.exists(wd))) {
    dir.create(wd)
  }
  
  
  dt = load_data(ticker, date, method, num, mute_load)
  reg = create_benchmark_regimes(dt, ticker)
  wd = paste(getwd(), '/regimes/', ticker, sep = '')
  w_loc = paste(wd, '/',
                ticker, '_', 
                date, '_', 
                method, '_', 
                num, '_d',
                '.csv', sep ='')
  
  if (save_file){
    write.table(reg, file = w_loc, row.names = FALSE) 
    cat('Wrote: ', w_loc, '\n')
  }
  
  return(reg)
}

#This will run the function for all the stocks that raw_data exists for
auto_fun <- function(){
  date = '4-11-22'
  days_back = 30
  
  files = list.files("raw_data")
  
  for(i in files){
    cat("Regimes for:", i, "\n")
    create_regimes_wrapper(i, date, 0, days_back, TRUE, TRUE)
  }
}

auto_fun()


################################################
####In progress stuff outside function
################################################

#t = load_data('APPL', '4-14-22', 0, 10)
# data = t
# create_regimes(t, 'FB')

#create_regimes_wrapper('FB', '4-14-20', 0, 10, TRUE)