#loads data from csvs
#creates the regimes

library(lubridate)
library(bizdays)

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
  return(get_data(ticker, dates, wd))
  
}

###
#TODO
#add data encoding for the regimes, to include ticker, date, lag, type 
#for data retrieval during options pricing

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
  
  #params
  smpl = 5 #number of minutes we sample
  day_scale = c(5,4,3,2,1,0) #how we discount the previous days
  
  #for loop vars
  regs = matrix(ncol = 5, nrow=0)
  colnames(regs) = c("Regime","Start Time","End Time","Mean","Standard Deviation")
  
  #helper vars
  curr_reg = c()
  start = 1
  count = 1
  
  
  for (i in seq(from = 1 + smpl, to = 389, by = smpl)){
    #old data
    curr_reg = rbind(curr_reg, log_data[(i - smpl):(i - 1), 1])
    
    #TODO
    #for each day in the data
    # for(j in 1:ncol(log_data)){
    #   log_data[i:(i + smpl), j]
    #   
    # }
    
    #data
    new_data = log_data[i:(i + smpl - 1), 1]
    
    if(ks.test(curr_reg, new_data)$p.value < 0.05){  #replace with ks.boot?, allwos ties
      #new regime
      tmp = c(count,
              start,#start index
              i,#stop index
              mean(curr_reg),
              sd(curr_reg))
      regs = rbind(regs, tmp)
      
      start = i
      count = count + 1
      curr_reg = c()
    }
  }
  #currently the ks.test generates warnings about ties (duplicated values in the later stages of the dataset)
  
  #add the remaining points to the last regime
  ###we treat the remaining datapoints as part of the latest regime and add regime to dataset
  curr_reg = rbind(curr_reg, new_data)
  tmp = c(count,
          start,#start index
          390,#stop index
          mean(new_data, na.rm=TRUE),
          sd(new_data, na.rm=TRUE))
  regs = rbind(regs, tmp)
  
  return(regs)
}

################################################
####In progress stuff outside function



#######################################

t = load_data('FB', '4/14/20', 0, 10)
data = t
# 
# {
# 
# 
# 
# log_data = data.frame(matrix(ncol = ncol(data), nrow = 389))
# colnames(log_data) = colnames(data)
# 
# for (col in colnames(data)){
#   #adding 1 to avoid NaNs from neg logs
#   log_data[,col] = log( c(diff(data[,col])/data[,col][-1]) + 1 )
# }
# 
# 
# 
# #params
# smpl = 5 #number of minutes we sample
# day_scale = c(5,4,3,2,1,0) #how we discount the previous days
# 
# 
# #for loop vars
# regs = matrix(ncol = 5, nrow=0)
# colnames(regs) = c("Regime","Start Time","End Time","Mean","Standard Deviation")
# 
# 
# #helper vars
# curr_reg = c()
# start = 1
# count = 1
# #carry = NULL
# 
# #need to create the sets of data for the test to run
# for (i in seq(from = 1 + smpl, to = 389, by = smpl)){
#   #old data
#   curr_reg = rbind(curr_reg, log_data[(i - smpl):(i - 1), 1])
#   print(i)
#   #for each day in the data
#   # for(j in 1:ncol(log_data)){
#   #   log_data[i:(i + smpl), j]
#   #   
#   # }
#  
#   #data
#   new_data = log_data[i:(i + smpl - 1), 1]
#   
#   print(tail(new_data, 1))
#   if(ks.test(curr_reg, new_data)$p.value < 0.05){  #replace with ks.boot?, allwos ties
#     print('ADD')
#     #new regime
#     tmp = c(count,
#             start,#start index
#             i,#stop index
#             mean(curr_reg),
#             sd(curr_reg))
#     regs = rbind(regs, tmp)
#     
#     start = i
#     count = count + 1
#     curr_reg = c()
#   }
#  
#   #test
#   # if (i == 386){
#   #   tmp = c(count,
#   #           start,#start index
#   #           i,#stop index
#   #           mean(curr_reg),
#   #           sd(curr_reg))
#   #   regs = rbind(regs, tmp)
# }
# #currently the ks.test generates warnings about ties (duplicated values in the later stages of the dataset)
# 
# #we treat the remaining datapoints as part of the latest regime and add regime to dataset
# curr_reg = rbind(curr_reg, new_data)
# tmp = c(count,
#         start,#start index
#         390,#stop index
#         mean(new_data, na.rm=TRUE),
#         sd(new_data, na.rm=TRUE))
# regs = rbind(regs, tmp)
# 
# }
# 

#create_regimes(t)
