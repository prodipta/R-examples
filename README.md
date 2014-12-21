IntraDay_R
==========

R codes for fetching intra-day data from publicly available sources

the main function is getIntradayPrice<- function(ticker,src='google',period=1,interval=5,tz=NULL).
Ticker is the exact string used by the data provider (either yahoo finance or google finance). Second argument can be either 
"yahoo" or "google". Period specifiies number of days of data. Usually google provides around 20 days of intraday data, for e.g.
Interval is time-stamp interval (in minutes). Google provides aggregated data, so we can query directly with the specified
interval. For yahoo, we have one minute interval data provided. I have used XTS routine ToPeriod() to aggregate. (Hence the 
time stamp other than 1 minute can be not so nice).Tz is the timezone, defualts to system time zone. By default, the reutrned data will be stored a variable in the global environment, with the same name as <ticker> with all non-alphanumeric characters removed (if auto.assign is TRUE).


The rest of the functions are helper functions. I will extend these in future. Required package is xts and Rcurl. Recommended
package is quantmod.

Example:

#1
require(quantmod)
source("GetIntradayPrice.R")
getIntradayPrice("^NSEI", src="yahoo")
chart_Series(NSEI)

#2
require(quantmod)
source("GetIntradayPrice.R")
x <- getIntradayPrice("SPY",period=10,interval=30,auto.assign=FALSE)
chart_Series(x)


