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
pkgs <- c('data.table', 'tidyverse', 'ggrepel', 'ggpubr', 'quantmod', 'tidyquant', 'plotly', 'pingr', 'lubridate', 'XML', 'IBrokers', "IButils")
res <- lapply(pkgs, require_package)



#read personal account details for flexquery and size
account_data <- fread("account_data.txt", header = T, skip = 1)
personal_flexquery_id <- as.numeric(account_data[variable=="flexquery_id",2])
personal_flexquery_token <- as.character(account_data[variable=="flexquery_token",2])
personal_account_size <- as.numeric(account_data[variable=="acount_size",2])



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







#Get trades from Interactive Brokers through flexquery
flex_web_service(file = "flexquerytemp.xml", token = personal_flexquery_token, query = personal_flexquery_id, version = 3, delay = 2, no.write.msg = TRUE, no.write.warn = TRUE, verbose = TRUE)
flexquery <- xmlParse("flexquerytemp.xml")
flexquery_list <- xmlToList(flexquery)
trades <- as.data.frame(flexquery_list[["FlexStatements"]][["FlexStatement"]][["Trades"]])
trades <- as.data.frame(t(trades))
trades$rowname <- rownames(trades)
trades <- trades %>% filter(str_detect(symbol, "SPXW") & str_detect(rowname, "Trade"))
trades <- trades %>% select(description, strike, expiry, putCall, dateTime, quantity, tradePrice, ibCommission, openCloseIndicator, fifoPnlRealized)
#get right date
trades <- trades %>% mutate(timestamp=paste0(substr(dateTime,1,4),"-",substr(dateTime,5,6),"-",substr(dateTime,7,8)," ",substr(dateTime,10,11),":",substr(dateTime,12,13),":",substr(dateTime,14,15))) %>% mutate(timestamp = as.POSIXct(timestamp, tz = "America/New_York"))
#format data
trades <- trades %>% select(-dateTime) %>% mutate(description=as.character(description), strike=as.numeric(as.character(strike)), expiry=as.numeric(as.character(expiry)), putCall=as.character(putCall), quantity=as.numeric(as.character(quantity)), tradePrice=as.numeric(as.character(tradePrice)), ibCommission=as.numeric(as.character(ibCommission)), openCloseIndicator=as.character(openCloseIndicator), fifoPnlRealized=as.numeric(as.character(fifoPnlRealized)))


#start from 8th of June serious strategy 
trades <- trades %>% filter(timestamp>="2020-06-08 08:00:00")
spx_1min_fortrades <- spx_1min %>% filter(timestamp>="2020-06-08 08:00:00")

#create traded strategies aggregated data
trades_opened <- trades %>% mutate(minute=round_date(timestamp,unit = "1minute")) %>% filter(openCloseIndicator=="O") %>% group_by(expiry,minute,putCall) %>% summarize(date_opened=min(timestamp), strike_short=ifelse(putCall[1]=="C",min(strike),max(strike)), strike_long=ifelse(putCall[1]=="C",max(strike),min(strike)), wing=max(strike)-min(strike), strategy=ifelse(putCall[1]=="C", paste0(putCall[1], "", min(strike),"/",max(strike)), paste0(putCall[1], "", max(strike),"/",min(strike))), splittrades=length(quantity)/2, contracts=sum(quantity[quantity>0]), credit=format(sum(tradePrice*sign(quantity)/splittrades*(-1)), digits = 2, nsmall = 2), commissions_bot=sum(ibCommission)) %>% select(-splittrades)

#now take closing trades to get PnL, based on FiFo!
trades_closed <- trades %>% filter(openCloseIndicator=="C") %>% group_by(expiry,strike,putCall) %>% summarize(date_closed=min(timestamp), commissions_sld=sum(ibCommission),contracts=sum(quantity), debit=format(sum(tradePrice*quantity)/sum(quantity)*sign(sum(quantity)), digits = 2, nsmall = 2), PnL_IB=sum(fifoPnlRealized), strike_short=ifelse(contracts>0,strike,NA), strike_long=ifelse(contracts<0,strike,NA)) %>% ungroup()

trades_aggregated <- trades_opened %>% mutate(strike=strike_short) %>% left_join(trades_closed %>% filter(!is.na(strike_short)) %>% select(-strike,-contracts, -strike_long) %>% rename(commissions_sld_short=commissions_sld, debit_short=debit, PnL_IB_short=PnL_IB)) %>% left_join(trades_closed %>% filter(!is.na(strike_long)) %>% select(-strike,-contracts, -strike_short, -date_closed) %>% rename(commissions_sld_long=commissions_sld, debit_long=debit, PnL_IB_long=PnL_IB)) 
#add 0 expiry close out for yesterdays' trades assuming they all expired worthless
trades_aggregated <- trades_aggregated %>% mutate(commissions_sld_short=replace_na(commissions_sld_short,0), commissions_sld_long=replace_na(commissions_sld_long,0), debit_short=replace_na(debit_short,0), debit_long=replace_na(debit_long,0))
#aggregates combining opening and closing trades
trades_aggregated <- trades_aggregated %>% mutate(commissions_sld=commissions_sld_short+commissions_sld_long, debit=format(as.numeric(debit_short)+as.numeric(debit_long), digits = 2, nsmall = 2), PnL_IB=PnL_IB_short+PnL_IB_long) %>% mutate(PnL_recomputed=(as.numeric(credit)-as.numeric(debit))*contracts*100 + commissions_bot + commissions_sld) 
trades_aggregated <- trades_aggregated %>% select(-ends_with("long"), -ends_with("short"))


#Daily PnL in Dollars to see if computed correctly and consistent with broker data
print(trades %>% group_by(expiry) %>% summarize(PnL=sum(as.numeric(as.character(fifoPnlRealized)))) %>% as.data.frame())
#compare with data from our self created trades_aggregates
print(trades_aggregated %>% group_by(expiry) %>% summarize(PnL_IB=sum(PnL_IB), PnL_recomputed=sum(PnL_recomputed)) %>% as.data.frame())






#Figure 
x_min <- min(spx_1min_fortrades$timestamp)
x_max <- max(spx_1min_fortrades$timestamp)
#candle chart
#tail(spx_1min, n = 60*6.5) %>% plot_ly(x = ~time, type="candlestick",open = ~Open, close = ~Close,high = ~High, low = ~Low) %>% layout(title="SPX 1min Chart")
#in ggplot: Candlestick Chart
p_candlestick_trades <- ggplot(data = spx_1min_fortrades, aes(x = timestamp, y = Close)) +
  geom_candlestick(aes(open = Open, high = High, low = Low, close = Close)) +
  geom_segment(data=trades_aggregated, aes(x = date_opened, y = strike, xend = date_closed, yend = strike, colour = putCall), size=2) + theme_tq() + scale_x_datetime(date_labels = '%e %b %y',date_breaks = '1 day', limits = c(x_min, x_max)) + geom_label(data=trades_aggregated %>% filter(PnL_IB>0), aes(x = date_opened, y = strike + ifelse(putCall=="P", -20,+20), label=str_glue("{contracts}x{credit}:+{round(PnL_IB)}$")), size=3, color="darkgreen") + geom_label(data=trades_aggregated %>% filter(PnL_IB<0), aes(x = date_opened, y = strike + ifelse(putCall=="P", -20,+20), label=str_glue("{contracts}x{credit}:{round(PnL_IB)}$")), size=3, color="darkred")  + xlab("") + ylab("SPX")
print(p_candlestick_trades)

#Bar chart per day
p_PnL_bar <- ggplot(data = trades_aggregated %>% mutate(daydate=round_date(date_opened,unit = "12hour")) %>% group_by(daydate) %>% summarize(PnL=sum(PnL_IB))) +
  geom_line(data = spx_1min_fortrades, aes(x = timestamp, y = Close), color="white", alpha=0) + theme_tq() +  scale_x_datetime(date_labels = '%e %b %y',date_breaks = '1 day', limits = c(x_min, x_max)) +
  geom_bar(aes(x = daydate, y = PnL, fill=as.character(sign(PnL))), width = 5e4, stat = "identity") + xlab("") + ylab("PnL")  + scale_fill_manual(breaks = c(-1,+1), values = c("darkred", "darkgreen")) + theme(legend.position = "none") + geom_text(aes(x = daydate, y = sign(PnL)*(abs(PnL)+200), label=sprintf('%+.0f$', PnL)))
print(p_PnL_bar)

#Line chart total
p_total_line <- ggplot(data = trades_aggregated %>% mutate(daydate=round_date(date_opened,unit = "12hour")) %>% group_by(daydate) %>% summarize(PnL=sum(PnL_IB))) +
  geom_line(data = spx_1min_fortrades, aes(x = timestamp, y = Close), color="white", alpha=0) + theme_tq() +  scale_x_datetime(date_labels = '%e %b %y',date_breaks = '1 day', limits = c(x_min, x_max)) +
  geom_line(aes(x = daydate, y = cumsum(PnL)), color="blue", size=2) + xlab("") + ylab("Total [$]") + theme(legend.position = "none") + geom_text(aes(x = daydate, y = cumsum(PnL)+200, label=sprintf('%+.0f$', cumsum(PnL))), color="blue")
print(p_total_line)

ggarrange(p_candlestick_trades, p_PnL_bar, p_total_line, nrow = 3, heights = c(.6,.2,.2), common.legend = T, legend = "bottom")
ggsave("Trading Log.pdf", device = "pdf", width = 15, height = 10)
















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

