#takes in the raw data file
#creates the processed data
#takes in 1 year of minute data

#how to download data:
# go to https://www.finam.ru/profile/akcii-usa-bats/facebook-inc_fb/export/?market=25&em=874399&token=&code=FB&apply=0&df=1&mf=0&yf=2020&from=01.01.2020&dt=31&mt=11&yt=2020&to=31.12.2020&p=2&f=FB_200101_201231&e=.csv&cn=FB&dtf=5&tmf=3&MSOR=1&mstime=on&mstimever=1&sep=1&sep2=1&datf=1&at=1
#   CHANGE ONLY YEAR AND COMPANY!
# download the file, place it in the raw_data under the folder for that company
# then run the below function, it will give the correct format, but will not check integrity
#   the format is consistant and (assuming the data is downloaded correctly) needs no cleaning



#dynamic cwd set
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

parse_ticker_data <- function(ticker){
  #ticker = 'FB'
  rd = paste(getwd(), "/raw_data/", ticker, sep = '')
  wd = paste(getwd(), "/proc_data/", ticker, sep = '')

  if(!dir.exists(wd)){
    dir.create(wd)
  }
  
  dir_files = list.files(rd)
  
  for (file in dir_files){
    #file = dir_files
    data = read.csv(paste(rd, '/', file, sep = ''))
    data['X.DATETIME.'] = paste(data$X.DATE., data$X.TIME.)
    data$X.DATE. = NULL
    data$X.TIME. = NULL
    data$X.PER. = NULL
    colnames(data) = c('ticker', 'open', 'high', 'low', 'close', 'volume', 'datetime')
    
    #get the year of the data
    #extracts the year of the first row
    yr = substr(data$datetime[1], 7,8) 
    cat(paste(wd, '/', ticker, '_', yr, sep = ''))
    
    write.csv(data, paste(wd, '/', ticker, '_', yr, sep = ''))
  }
  
}

#run the function
parse_ticker_data('FB')




############ TESTING


library(lubridate)

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

