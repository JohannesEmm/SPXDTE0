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
pkgs <- c('data.table', 'tidyverse', 'ggrepel', 'ggpubr', 'quantmod', 'tidyquant', 'plotly', 'pingr', 'lubridate', 'XML', 'IBrokers', 'IButils', 'zoo', 'scales', 'rstudioapi')
res <- lapply(pkgs, require_package)

#set current directory
path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))


#read personal account details for flexquery and size
account_data <- fread("account_data.txt", header = T, skip = 1)
personal_flexquery_id <- as.numeric(account_data[variable=="flexquery_id",2])
personal_flexquery_token <- as.character(account_data[variable=="flexquery_token",2])
personal_account_size <- as.numeric(account_data[variable=="acount_size",2])
personal_start_date <- as.character(account_data[variable=="personal_start_date",2])



#get and update 1min SPX data
load("spx_1min.Rdata"); load("vix_1min.Rdata"); 
print(str_glue("Last data SPX from {last(spx_1min$date)}"))
# Connect to TWS and download SPX minute data if available
if(!is.na(ping_port(port=7496,destination = "localhost", count = 1))){
  tws <- twsConnect(verbose = T)
  contract <- twsIndex('SPX','CBOE')
  #reqRealTimeBars # by default retreives 30 days of daily data
  spx_last_month_1min <- reqHistoricalData(tws, Contract=contract, barSize = "1 min", useRTH = "1", duration = "10 d")
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
  vix_last_month_1min <- reqHistoricalData(tws, Contract=contract, barSize = "1 min", useRTH = "1", duration = "6 d")
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
#ggplot(spx_day) + geom_histogram(aes(range))
#ggplot(spx_day) + geom_histogram(aes(closediff))
#spx_day %>% plot_ly(x = ~date, type="candlestick",open = ~Open, close = ~Close,high = ~High, low = ~Low) %>% layout(title="SPX Daily")







#Get trades from Interactive Brokers through flexquery
if(flex_web_service(file = "flexquerytemp.xml", token = personal_flexquery_token, query = personal_flexquery_id, version = 3, delay = 5, no.write.msg = TRUE, no.write.warn = TRUE, verbose = TRUE)==1) stop("Flexquery not succesfully retrieved, please try again shortly.")
flexquery <- xmlParse("flexquerytemp.xml")
flexquery_list <- xmlToList(flexquery)
trades <- as.data.frame(flexquery_list[["FlexStatements"]][["FlexStatement"]][["Trades"]])
trades <- as.data.frame(t(trades))
trades$rowname <- rownames(trades)
trades <- trades %>% filter(str_detect(symbol, "SPX") & str_detect(rowname, "Trade"))
trades <- trades %>% select(description, strike, expiry, putCall, dateTime, quantity, tradePrice, ibCommission, openCloseIndicator, fifoPnlRealized)
#get right date
trades <- trades %>% mutate(timestamp=paste0(substr(dateTime,1,4),"-",substr(dateTime,5,6),"-",substr(dateTime,7,8)," ",substr(dateTime,10,11),":",substr(dateTime,12,13),":",substr(dateTime,14,15))) %>% mutate(timestamp = as.POSIXct(timestamp, tz = "America/New_York"))
#format data
trades <- trades %>% select(-dateTime) %>% mutate(description=as.character(description), strike=as.numeric(as.character(strike)), expiry=as.numeric(as.character(expiry)), putCall=as.character(putCall), quantity=as.numeric(as.character(quantity)), tradePrice=as.numeric(as.character(tradePrice)), ibCommission=as.numeric(as.character(ibCommission)), openCloseIndicator=as.character(openCloseIndicator), fifoPnlRealized=as.numeric(as.character(fifoPnlRealized)))


#start from personal start date
trades <- trades %>% filter(timestamp>=personal_start_date)

#remove error trades!
trades_complete <- trades
if(file.exists("error_trades.txt")){
  error_trades <- fread("error_trades.txt", header = T, skip = 0)
  for(i in 1:nrow(error_trades)){
    if(is.na(error_trades$timestamp[i])) trades <- trades %>% filter(description!=as.character(error_trades$contract[i]))
    if(!is.na(error_trades$timestamp[i])) trades <- trades %>% filter(!(description==as.character(error_trades$contract[i]) & as.character(timestamp)==as.character(error_trades$timestamp[i]) & as.character(openCloseIndicator)==as.character(error_trades$opened_closed[i])))
  }}


####### FIFO:  combine opened and closed individual positions and options using FiFo trades > trades_opened_closed ######
trades_opened <- trades %>% filter(openCloseIndicator=="O") 
trades_opened_closed <- trades_opened %>% mutate(tradePrice_closed=NA, ibCommission_closed=NA, fifoPnlRealized_closed=NA, timestamp_closed=as.POSIXct(NA, tz = "America/New_York"))
trades_opened_closed$timestamp_closed <- force_tz(trades_opened_closed$timestamp_closed, "America/New_York")
#for closing trades compute all relevant variables in per unit values (price, commissions, fifoPnL, and date (earliest))
trades_closed_original <- trades %>% filter(openCloseIndicator=="C")
trades_closed <- trades %>% filter(openCloseIndicator=="C") %>% mutate(ibCommission=ibCommission/abs(quantity), fifoPnlRealized=fifoPnlRealized/abs(quantity)) %>% mutate(quantity_original=quantity, close_id=row_number())
for(i in 1:nrow(trades_opened)){
  #print(i) #i=19
  exp=trades_opened$expiry[i]
  strk=trades_opened$strike[i]
  pc=trades_opened$putCall[i]
  num=trades_opened$quantity[i]
  #print(trades_closed %>% filter(expiry==exp & putCall==pc & strike==strk) %>% arrange(timestamp))
  #get all closing trades
  current_closing_trade_all <- trades_closed %>% filter(expiry==exp & putCall==pc & strike==strk) %>% arrange(timestamp)
  #remove ones that have already been applied with quantity==0
  current_closing_trade_all <- current_closing_trade_all %>% filter(quantity!=0)
  if(nrow(current_closing_trade_all)>0){
  timestamp = as.POSIXct(first(current_closing_trade_all$timestamp), tz = "America/New_York")
  tradePrice = 0; ibCommission = 0; fifoPnlRealized = 0 #initialize close at zero
  #now close one item/contract at a time
  closing_trade_num = 1; current_closing_trade <- current_closing_trade_all[closing_trade_num,]
  for(item in 1:abs(num)){
    current_closing_trade$quantity = current_closing_trade$quantity + sign(num)*1
    tradePrice = tradePrice + current_closing_trade$tradePrice
    ibCommission = ibCommission + current_closing_trade$ibCommission
    fifoPnlRealized = fifoPnlRealized + current_closing_trade$fifoPnlRealized
    if(current_closing_trade$quantity==0){ #if full closing trade has been used update trades_closed data frame (no more closing contracts at the current closing trade)
      trades_closed <- trades_closed %>% mutate(quantity = ifelse(expiry==exp & putCall==pc & strike==strk & close_id==current_closing_trade$close_id, 0, quantity))
      #if not yet fully closed, move to next closing trade (FiFo)
      if(item<abs(num)){
      closing_trade_num <- closing_trade_num + 1
      current_closing_trade <- current_closing_trade_all[closing_trade_num,]
      }
      }
    if(item==abs(num)) { #last contract has been closed
      trades_closed <- trades_closed %>% mutate(quantity = ifelse(expiry==exp & putCall==pc & strike==strk & close_id==current_closing_trade$close_id, current_closing_trade$quantity, quantity)) #write renaming closing items after fifo application
    }
    
    }
  #print(paste(i, current_closing_trade$quantity))
  #now write these values to trades_opened_closed
  trades_opened_closed$tradePrice_closed[i] =  tradePrice/abs(num)
  trades_opened_closed$ibCommission_closed[i] = ibCommission
  trades_opened_closed$fifoPnlRealized_closed[i] = fifoPnlRealized
  trades_opened_closed$timestamp_closed[i] = timestamp
  }
}
####### END FIFO ######





#add 0 expiry close out for yesterdays' trades assuming they all expired worthless
trades_opened_closed <- trades_opened_closed %>% mutate(ibCommission_closed=replace_na(ibCommission_closed,0), tradePrice_closed=replace_na(tradePrice_closed,0))
trades_opened_closed$timestamp_closed <- as.POSIXct(ifelse(is.na(trades_opened_closed$timestamp_closed), as.POSIXct(paste0(date(trades_opened_closed$timestamp), " 16:20:00"), tz = "America/New_York"), trades_opened_closed$timestamp_closed), tz = "America/New_York", origin = lubridate::origin)

#Combine splittrades
trades_opened_closed <- trades_opened_closed %>% mutate(time_opened_round=round_date(timestamp,unit = "2minute")) %>% group_by(description,strike,expiry,putCall,time_opened_round) %>% summarize(tradePrice=weighted.mean(tradePrice,w = quantity), ibCommission=sum(ibCommission), fifoPnlRealized=sum(fifoPnlRealized), timestamp = weighted.mean(timestamp, w = quantity), tradePrice_closed=weighted.mean(tradePrice_closed,w = quantity), ibCommission_closed=sum(ibCommission_closed), fifoPnlRealized_closed=sum(fifoPnlRealized_closed), timestamp_closed=weighted.mean(timestamp_closed, w = quantity), quantity = sum(quantity))
#Recompute PnL based on individual positions
trades_opened_closed <- trades_opened_closed %>% mutate(Pnl_recomputed=-(quantity)*(tradePrice - tradePrice_closed)*100+ibCommission+ibCommission_closed)                                                          

#create aggregated data combining splittrades for: CREDIT SPREADS
trades_strategies <- trades_opened_closed %>% group_by(expiry,time_opened_round,putCall) %>% summarize(date_opened=min(timestamp), strike_short=ifelse(putCall[1]=="C",min(strike),max(strike)), strike_long=ifelse(putCall[1]=="C",max(strike),min(strike)), wing=max(strike)-min(strike), strategy=ifelse(putCall[1]=="C", paste0(putCall[1], "", min(strike),"/",max(strike)), paste0(putCall[1], "", max(strike),"/",min(strike))), contracts=mean(abs(quantity)), credit=format(sum(tradePrice*sign(quantity)*(-1)), digits = 2, nsmall = 2), commissions_opened=sum(ibCommission), date_closed=min(timestamp_closed), debit=format(sum(tradePrice_closed*sign(quantity)*(-1)), digits = 2, nsmall = 2), commissions_closed=sum(ibCommission_closed), PnL_strategies_IB=sum(fifoPnlRealized_closed), PnL_strategies_recomputed=(as.numeric(credit)-as.numeric(debit))*contracts*100 + commissions_opened + commissions_closed)  %>% ungroup()

#if the short option is rolled, replace long leg based on existing open contracts at zero costs
trades_strategies <- trades_strategies %>% group_by(expiry,putCall) %>% mutate(strike_long = ifelse(wing==0, lag(strike_long), strike_long), strategy = ifelse(wing==0, paste0(putCall, "", strike_short,"/",strike_long), strategy), debit = ifelse(wing==0, 0, debit), commissions_closed = ifelse(wing==0, 0, commissions_closed), wing = ifelse(wing==0, abs(strike_long-strike_short), wing))


#define final PnL and strike (=strike_short) and add SPX at open  
trades_strategies <- trades_strategies %>% mutate(day_opened=as.POSIXct(paste0(date(date_opened), " 12:00:00"), tz = "America/New_York")) %>% mutate(minute=round_date(date_opened,unit = "1minute")) %>% left_join(spx_1min %>% select(timestamp, Close) %>% rename(SPX_at_open=Close, minute=timestamp)) %>% mutate(PnL=PnL_strategies_recomputed, strike=strike_short)

#verify consistency of fifoPnl
print(trades %>% group_by(expiry) %>% summarize(PnL_trades_original_IB=sum(fifoPnlRealized, na.rm=T)) %>% full_join(trades_opened_closed %>% group_by(expiry) %>% summarize(PnL_trades_opened_close=sum(fifoPnlRealized_closed, na.rm=T), PnL_opened_closed_recomputed=sum(Pnl_recomputed, na.rm=T))) %>% full_join(trades_strategies %>% group_by(expiry) %>% summarize(PnL_strategies_IB=sum(PnL_strategies_IB, na.rm=T), PnL_strategies_recomputed=sum(PnL_strategies_recomputed, na.rm=T))) %>% as.data.frame())


#Some basic stats
performance_stats <- trades_strategies %>% group_by() %>% summarize(AvgGain=mean(PnL[PnL>0]), AvgLoss=mean(PnL[PnL<0]), PoP=sum(sign(PnL[PnL>0]))/length((PnL))*100, NumTrades=length(PnL), Total=sum(PnL), Commissions=sum(commissions_opened+commissions_closed)) %>% as.data.frame()
print(performance_stats)


#Figures
spx_1min_fortrades <- spx_1min %>% filter(timestamp>=personal_start_date)

x_min <- as.POSIXct(paste0(date(min(spx_1min_fortrades$timestamp)), " 00:00:00"), tz = "America/New_York")
x_max <- as.POSIXct(paste0(today(), " 23:59:59"), tz = "America/New_York")
#create empty data for SPX until x_max
for(.day in seq(date(last(spx_1min_fortrades$timestamp))+1, date(x_max), by = "day")){
  spx_empty <- spx_1min_fortrades %>% filter(date==date(x_min)) %>% mutate(Open=as.numeric(NA),High=as.numeric(NA),Low=as.numeric(NA),Close=as.numeric(NA)) %>% mutate(date=as.character(as.Date(.day)), timestamp=as.POSIXct(paste0(date, " ",time), tz = "America/New_York"))
  spx_1min_fortrades <- rbind(spx_1min_fortrades, spx_empty)
}

#For plotting now only trading hours
spx_1min_fortrades <- spx_1min_fortrades %>%
  mutate(timestamp_chr = as.character(timestamp), day = lubridate::day(timestamp),hour = lubridate::hour(timestamp),minute = lubridate::minute(timestamp),new_day = if_else(day != lag(day) | is.na(lag(day)), 1, 0))
trades_strategies <- trades_strategies %>%
  mutate(timestamp_chr = as.character(date_opened), day = lubridate::day(timestamp),hour = lubridate::hour(timestamp),minute = lubridate::minute(timestamp),timestamp_chr_closed = as.character(date_closed), timestamp_chr_middle=as.character(as.POSIXct((as.numeric(date_closed) + as.numeric(date_opened)) / 2, origin = '1970-01-01', tz="America/New_York")))

my_breaks <-spx_1min_fortrades$timestamp_chr[seq.int(1,length(spx_1min_fortrades$timestamp_chr) , by = 60*6.5)] #every day 6.5 trading hours!
my_breaks <- paste(seq.Date(date(x_min), date(x_max), by = "day"), "09:30:00")

#in ggplot: Candlestick Chart
shiftlabel=20;
p_candlestick_trades <- ggplot(spx_1min_fortrades %>% mutate(Close=ifelse(new_day==1,NA,Close)), aes(x = timestamp_chr, y = Close, group = 1)) +  
  geom_line(na.rm = F) +
  theme_tq() + scale_x_discrete(breaks = my_breaks, drop=F, labels = format(as.POSIXct(my_breaks, tz="America/New_York"), "%e %b"), expand = c(0, 0)) +
  geom_segment(data=trades_strategies, aes(x = timestamp_chr, y = strike, xend = timestamp_chr_closed, yend = strike, colour = putCall), size=2) +
  #  theme_tq() + scale_x_datetime(date_labels = '%e %b',date_breaks = '1 day', limits = c(x_min, x_max)) + 
  geom_label(data=trades_strategies %>% filter(PnL>0), aes(x = timestamp_chr_middle, y = strike + ifelse(putCall=="P", -shiftlabel,+shiftlabel), label=str_glue("{contracts}x{credit} \n +{round(PnL)}$")), size=3, color="darkgreen", alpha=0.5) + geom_label(data=trades_strategies %>% filter(PnL<0), aes(x = timestamp_chr_middle, y = strike + ifelse(putCall=="P", -shiftlabel,+shiftlabel), label=str_glue("{contracts}x{credit} \n {round(PnL)}$")), size=3, color="darkred", alpha=0.5)  + xlab("") + ylab("SPX") + 
  geom_segment(data=trades_strategies, aes(x = timestamp_chr, y = SPX_at_open, xend = timestamp_chr_closed, yend = strike), size=0.1, color="grey", alpha=0.4) + scale_colour_manual(values = c("P"="blue", "C"="orange"), labels=c("P"="Put Credit Spread", "C"="Call Credit Spread"), name="Strategy")# + theme(legend.position = "none")
print(p_candlestick_trades)

#PnL
my_breaks_no_weekends <- paste(unique(spx_1min_fortrades$date), "09:30:00")
trades_strategies_for_pnl <- trades_strategies %>% mutate(day_opened= paste(as.Date(day_opened),"09:30:00")) %>% group_by(day_opened) %>% summarize(PnL=sum(PnL)) %>% mutate(Total=cumsum(PnL)) %>% complete(day_opened=my_breaks_no_weekends) %>% mutate(day_opened_midday=paste(date(day_opened), "12:45:00")) %>% mutate(PnL=ifelse(day_opened==paste(as.Date(today()),"09:30:00"), 0, PnL), Total=zoo::na.locf(Total))
#Bar chart per day
shiftlabel_dollar=250
p_PnL_bar <- ggplot(data =  trades_strategies_for_pnl) +
  theme_tq() + scale_x_discrete(breaks = my_breaks, drop=F, labels = format(as.POSIXct(my_breaks, tz="America/New_York"), "%e %b"), expand = c(0, 0)) + scale_y_continuous(labels = dollar) +
  geom_line(data = last(spx_1min_fortrades), aes(x = timestamp_chr, y = Close*0, group=1), color="white", alpha=0) + 
  geom_bar(aes(x = day_opened_midday, y = PnL, fill=as.character(sign(PnL))), width = 1, stat = "identity", na.rm = F) + xlab("") + ylab("Daily PnL")  + scale_fill_manual(breaks = c(-1,+1,0), values = c("darkred", "darkgreen", "black")) + theme(legend.position = "none") + geom_text(aes(x = day_opened_midday, y = sign(PnL)*(abs(PnL)+shiftlabel_dollar), label=sprintf('%+.0f$', PnL))) +
  geom_text(aes(x = day_opened, y = 100), label="   ")
print(p_PnL_bar)

#Line chart total
p_total_line <- ggplot(data = trades_strategies_for_pnl) +
    theme_tq() + scale_x_discrete(breaks = my_breaks, drop=F, labels = format(as.POSIXct(my_breaks, tz="America/New_York"), "%e %b"), expand = c(0, 0)) + scale_y_continuous(labels = dollar) +
  geom_line(data = last(spx_1min_fortrades), aes(x = timestamp_chr, y = Close*0, group=1), color="white", alpha=0) + 
    geom_line(aes(x = day_opened_midday, y = Total, group=1), color="blue", size=2) + xlab("") + geom_text(aes(x = day_opened, y = 100), label="   ") + 
    ylab("Total") + theme(legend.position = "none") + geom_text(data = trades_strategies_for_pnl %>% filter(!is.na(PnL)), aes(x = day_opened_midday, y = Total+ifelse(PnL<0,-1,+1)*shiftlabel_dollar*2, label=sprintf('%+.0f$', Total)), color="blue") + 
    geom_label(aes(x = my_breaks_no_weekends[2], y = performance_stats$Total*.7, hjust = "left", label=paste0("PoP = ", sprintf(performance_stats$PoP, fmt = "%.1f%%") ,"\n", "Avg. Gain = ", sprintf(performance_stats$AvgGain, fmt = "%.0f$"),"\n", "Avg. Loss = ", sprintf(performance_stats$AvgLoss, fmt = "%.0f$") ,"\n", "Commissions = ", sprintf(performance_stats$Commissions, fmt = "%.2f$"))), size=3, fill="grey90", alpha=0.6)
print(p_total_line)

#Arranged combined figure
print(ggarrange(p_candlestick_trades + theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))), p_PnL_bar + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()), p_total_line, nrow = 3, heights = c(.6,.2,.2), common.legend = T, legend = "bottom", hjust=c(0, 0, 0)))
ggsave("Trading_log.pdf", device = "pdf", width = 15, height = 10)
#now full also (wide scaling the width by months in the data)
ggsave("Trading_log_full.pdf", device = "pdf", width = 15*interval(personal_start_date, now()) / months(1), height = 10)



#IB API get SPXW options
#option1 <- twsOption("SPXW  200708C03130000")
#option1_data <- reqMktData(tws, option1, snapshot = T) 






