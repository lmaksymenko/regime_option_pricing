##HOW TO DOWNLOAD DATA
# Go to https://www.finam.ru/profile/akcii-usa-bats/facebook-inc_fb/export/?market=25&em=874399&token=&code=FB&apply=0&df=1&mf=0&yf=2020&from=01.01.2020&dt=31&mt=11&yt=2020&to=31.12.2020&p=2&f=FB_200101_201231&e=.csv&cn=FB&dtf=5&tmf=3&MSOR=1&mstime=on&mstimever=1&sep=1&sep2=1&datf=1&at=1
# Exhange / Company: 'Stocks USA(BATS)' and <COMPANY>
# Dates: start date: 01.01.<YEAR> end date: 31.12.<YEAR>
#    IE: the first and last day of the year (data is given inclusively)
# Frequencey: 1 min
# File: .csv, 
# Date format: mm/dd/yy ; HH:MM
# Data: "candle end", Moscow time should NOT be checked
# FILE DELIMITERS: leave as default ('comma', 'no')
# FORMAT: leave as default (longest option)
# BOX 1: Checked (get headers)
# Box 2: Unchecked (dont fill transactions)
#
# Download the file, place it in the 'raw_data' under the folder for that company
# Run the below function, it will give the correct format, but will not check integrity
#   the format is consistent and (assuming the data is downloaded correctly) needs no cleaning
#   Data must be not need cleaning, and must be exactly 1 year Jan 1 to Dec 31


#dynamic cwd set
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

parse_ticker_data <- function(ticker){
  
  rd = paste(getwd(), "/raw_data/", ticker, sep = '')
  dir_files = list.files(rd)
  
  final_df = data.frame()
  
  for (file in dir_files){
    #read in and clean the data
    data = read.csv(paste(rd, '/', file, sep = ''))
    data['X.DATETIME.'] = paste(data$X.DATE., data$X.TIME.) #combining datetime
    data$X.DATETIME. = strptime(data$X.DATETIME., '%m/%d/%y %H:%M')
    data$X.DATE. = NULL
    data$X.TIME. = NULL
    data$X.PER. = NULL
    colnames(data) = c('ticker', 'open', 'high', 'low', 'close', 'volume', 'datetime')
    
    ##combining data
 
    if (length(final_df) == 0){
      final_df = rbind(final_df, data)
    }else{
      #getting the dates 
      f_d = final_df[1,'datetime']$year
      c_d = data[1,'datetime']$year

      #order to attatch dfs
      if(c_d > f_d){
        #add below final
        final_df = rbind(final_df, data)
      }else{
        #add above final
        final_df = rbind(data, final_df)
      }
    }
    
  }
  write.csv(final_df, paste(getwd(), "/proc_data/", ticker, sep = ''))

}

#run the function
parse_ticker_data('FB')

