# Script for SPX DTE0 analysis and trade log
# Author: Johannes Emmerling



# get trades from flex query (includes expired options!)
#install.packages('IButils', type = 'source',repos = c('http://enricoschumann.net/R', getOption('repos')))
#setup in IB: https://webhelp.tradingdiarypro.com/create_flex_querries_interactive_brokers.htm
require_package <- function(package){
  if(!is.element(package, .packages(all.available = TRUE))){
    try(install.packages(package, repos="http://cran.rstudio.com"), silent = TRUE)
  }
  suppressPackageStartupMessages(library(package,character.only=T, quietly = TRUE))  
}
pkgs <- c('data.table', 'tidyverse', 'ggrepel', 'ggpubr', 'quantmod', 'tidyquant', 'plotly', 'pingr', 'lubridate', 'XML', 'IBrokers', "IBUtils")
res <- lapply(pkgs, require_package)


#get and update 1min SPX data
load("spx_1min.Rdata"); load("vix_1min.Rdata"); 
print(str_glue("Last data SPX from {last(spx_1min$date)}"))
# Connect to TWS and download SPX minute data if available
library(IBrokers)
if(!is.na(ping_port(port=7496,destination = "localhost", count = 1))){
  tws <- twsConnect(verbose = F)
  contract <- twsIndex('SPX','CBOE')
  #reqRealTimeBars # by default retreives 30 days of daily data
  spx_last_month_1min <- reqHistoricalData(tws, Contract=contract, barSize = "1 min", useRTH = "1", duration = "1 M")
  tzone(spx_last_month_1min) <- "America/New_York"
  spx_last_month_1min <- data.frame(DateTime=index(spx_last_month_1min),coredata(spx_last_month_1min[,1:4]))
  names(spx_last_month_1min) <- c("DateTime", "Open", "High", "Low", "Close")
  spx_last_month_1min <- spx_last_month_1min %>% mutate(timestamp=as.POSIXct(DateTime, tz = "America/New_York")) %>% separate(DateTime, c("date", "time"), sep = " ")
  #now update spx_1min
  spx_1min_new <- spx_last_month_1min %>% filter(timestamp>max(spx_1min$timestamp))
  spx_1min <- rbind(spx_1min, spx_1min_new)
  #now get VIX also
  contract <- twsIndex('VIX','CBOE')
  #reqRealTimeBars # by default retreives 30 days of daily data
  vix_last_month_1min <- reqHistoricalData(tws, Contract=contract, barSize = "1 min", useRTH = "1", duration = "1 M")
  tzone(vix_last_month_1min) <- "America/New_York"
  vix_last_month_1min <- data.frame(DateTime=index(vix_last_month_1min),coredata(vix_last_month_1min[,1:4]))
  names(vix_last_month_1min) <- c("DateTime", "Open", "High", "Low", "Close")
  vix_last_month_1min <- vix_last_month_1min %>% mutate(timestamp=as.POSIXct(DateTime, tz = "America/New_York")) %>% separate(DateTime, c("date", "time"), sep = " ")
  #now update spx_1min
  vix_1min_new <- vix_last_month_1min %>% filter(timestamp>max(vix_1min$timestamp))
  vix_1min <- rbind(vix_1min, vix_1min_new)
  twsDisconnect(tws)
  print(str_glue("New data added on SPX from {first(spx_1min_new$date)} to {last(spx_1min_new$date)}"))
  save(spx_1min, file = "spx_1min.Rdata"); save(vix_1min, file = "vix_1min.Rdata"); 
}
#compute daily data
spx_day <- spx_1min %>% group_by(date) %>% summarize(Open=first(Open), High=max(High), Low=min(Low), Close=last(Close)) %>% mutate(range=High-Low, closediff=Close-lag(Close))
ggplot(spx_day) + geom_histogram(aes(range))
ggplot(spx_day) + geom_histogram(aes(closediff))
spx_day %>% plot_ly(x = ~date, type="candlestick",open = ~Open, close = ~Close,high = ~High, low = ~Low) %>% layout(title="SPX Daily")










# Pricing options (no Div, European Style)
BlackScholes <- function(S, K, r, T, sig, type, return="value"){
  
  if(type=="C"){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    
    value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
    delta <- pnorm(d1)
    if(return=="value") return(value) else return(delta)
  }
  
  if(type=="P"){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    
    value <-  (K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1))
    delta <- -pnorm(-d1)
    if(return=="value") return(value) else return(delta)
  }
}

BlackScholes(2940,3000,0.00,(1/250)*(6.5/6.5),0.35,"C")

