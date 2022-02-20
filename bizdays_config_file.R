#creates the trading day calendar

library(bizdays)
library(libridate)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df = read.csv(paste(getwd(), 
                    "/utils/", "market_days_1980_2030.csv", sep = ''), 
                    header = FALSE)
hol = c(paste(df[,'V3'], '-', df[,'V2'], '-', df[,'V4'], sep = ''))

create.calendar(name = 'USA 1980-2030',
                holidays = hol,
                weekdays=c("saturday", "sunday"),
                start.date = '1980-1-1',
                end.date = '2031-1-1')

calendars()
