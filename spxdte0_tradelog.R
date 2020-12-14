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
pkgs <- c('data.table', 'tidyverse', 'ggrepel', 'ggpubr', 'quantmod', 'tidyquant', 'plotly', 'pingr', 'lubridate', 'XML', 'IBrokers', 'IButils', 'zoo', 'scales', 'rstudioapi', 'tis')
res <- lapply(pkgs, require_package)

#set current directory
path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))


#read personal account details for flexquery and size
account_data <- fread("account_data.txt", header = T, skip = 1)
personal_flexquery_id <- as.numeric(account_data[variable=="flexquery_id",2])
personal_flexquery_token <- as.character(account_data[variable=="flexquery_token",2])
personal_account_size <- as.numeric(account_data[variable=="account_size",2])
personal_start_date <- as.character(account_data[variable=="personal_start_date",2])
daily_goal <- as.character(account_data[variable=="daily_goal",2])

include_complex_strategies <- F

load("spx_1min.Rdata"); load("vix_1min.Rdata");
print(str_glue("Last data SPX from {last(spx_1min$date)}"))
# Connect to TWS and download SPX minute data if available
#if(as.Date(previousBusinessDay(Sys.Date()))>last(spx_1min$date)) source("get_tws_data.R")







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
    if(current_closing_trade$quantity==0){ #if full closing trade has been used => update trades_closed data frame (no more closing contracts at the current closing trade)
      trades_closed <- trades_closed %>% mutate(quantity = ifelse(expiry==exp & putCall==pc & strike==strk & close_id==current_closing_trade$close_id, 0, quantity))
      #if not yet fully closed, move to next closing trade (FiFo)
      if(item<abs(num)){
      closing_trade_num <- closing_trade_num + 1
      if(nrow(current_closing_trade_all)<closing_trade_num){
        #no further closed trade, so set all other closes as zero expired ones
        item = abs(num)
      }else{
      current_closing_trade <- current_closing_trade_all[closing_trade_num,]
      }
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


#manually overwrite cash-settlted assignments
trades_opened_closed <- trades_opened_closed %>% mutate(tradePrice_closed=ifelse(description=="SPX 26AUG20 3475.0 C", (3478.73-3475) , tradePrice_closed))


#add 0 expiry close out for yesterdays' trades assuming they all expired worthless
trades_opened_closed <- trades_opened_closed %>% mutate(ibCommission_closed=replace_na(ibCommission_closed,0), tradePrice_closed=replace_na(tradePrice_closed,0))
trades_opened_closed$timestamp_closed <- as.POSIXct(ifelse(is.na(trades_opened_closed$timestamp_closed), as.POSIXct(paste0(date(trades_opened_closed$timestamp), " 16:20:00"), tz = "America/New_York"), trades_opened_closed$timestamp_closed), tz = "America/New_York", origin = lubridate::origin)

#If trade opened before regular trading hours, assume it was done at the open for simplicity for now
trades_opened_closed <- trades_opened_closed %>% rowwise() %>% mutate(timestamp = as.POSIXct(ifelse(format(timestamp, "%H:%M:%S") < "09:30:00",  as.POSIXct(paste0(date(timestamp), " 09:30:", sprintf("%02d", as.integer(runif(1, min=0, max=59)))), tz = "America/New_York", origin = lubridate::origin), timestamp), tz = "America/New_York", origin = lubridate::origin))


#Combine splittrades
trades_opened_closed <- trades_opened_closed %>% mutate(time_opened_round=round_date(timestamp,unit = "10minute")) %>% group_by(description,strike,expiry,putCall,time_opened_round) %>% summarize(tradePrice=weighted.mean(tradePrice,w = quantity), ibCommission=sum(ibCommission), fifoPnlRealized=sum(fifoPnlRealized), timestamp = weighted.mean(timestamp, w = quantity), tradePrice_closed=weighted.mean(tradePrice_closed,w = quantity), ibCommission_closed=sum(ibCommission_closed), fifoPnlRealized_closed=sum(fifoPnlRealized_closed), timestamp_closed=weighted.mean(timestamp_closed, w = quantity), quantity = sum(quantity))
#Recompute PnL based on individual positions
trades_opened_closed <- trades_opened_closed %>% mutate(Pnl_recomputed=-(quantity)*(tradePrice - tradePrice_closed)*100+ibCommission+ibCommission_closed)     
#write trades_opened_closed.Rdata
trades_for_models = as.data.frame(trades_opened_closed %>% mutate(timestamp=round_date(timestamp,unit = "1minute"), timestamp_closed=round_date(timestamp_closed,unit = "1minute"), timestamp_closed=as.POSIXct(gsub("16:20:00", "15:59:00", timestamp_closed), tz = "America/New_York", origin = lubridate::origin)) %>% left_join(spx_1min %>% select(timestamp, Close) %>% rename(SPX_at_open=Close)) %>% left_join(vix_1min %>% select(timestamp, Close) %>% rename(VIX_at_open=Close)))  %>% left_join(spx_1min %>% select(timestamp, Close) %>% rename(timestamp_closed=timestamp, SPX_at_closed=Close)) %>% left_join(vix_1min %>% select(timestamp, Close) %>% rename(timestamp_closed=timestamp, VIX_at_closed=Close))
save(trades_for_models, file = "trades_for_models.RData")

#create aggregated strategies (going from complex to simple:
trades_opened_closed_complex_strategies <- trades_opened_closed %>% group_by(timestamp) %>% mutate(num_diff_strikes=length(unique(strike))) %>% filter(num_diff_strikes==3)
trades_opened_closed_without_complex_strategies <- trades_opened_closed %>% group_by(timestamp) %>% mutate(num_diff_strikes=length(unique(strike))) %>% filter(num_diff_strikes!=3)
#1) Iron butterflies
if(nrow(trades_opened_closed_complex_strategies)>0) trades_strategies_complex <- trades_opened_closed_complex_strategies %>% group_by(expiry,time_opened_round) %>% summarize(putCall="Both", date_opened=min(timestamp), strike_short=mean(strike[quantity<0]), strike_long=min(strike), strike_long2=max(strike), wing=strike_short-min(strike), strategy=paste0("IF",min(strike),"/",strike_short,"/",max(strike)), contracts=mean(abs(quantity)), credit=sum(tradePrice*sign(quantity)*(-1)), commissions_opened=sum(ibCommission), date_closed=min(timestamp_closed), debit=sum(tradePrice_closed*sign(quantity)*(-1)), commissions_closed=sum(ibCommission_closed), PnL_strategies_IB=sum(fifoPnlRealized_closed), PnL_strategies_recomputed=(as.numeric(credit)-as.numeric(debit))*contracts*100 + commissions_opened + commissions_closed)  %>% mutate(type="Iron Butterfly") %>% ungroup()

#2) CREDIT SPREADS for remaining trades!
trades_strategies_spreads <- trades_opened_closed_without_complex_strategies %>% group_by(expiry,time_opened_round,putCall) %>% summarize(date_opened=min(timestamp), strike_short=ifelse(putCall[1]=="C",min(strike),max(strike)), strike_long=ifelse(putCall[1]=="C",max(strike),min(strike)), strike_long2=NA, wing=max(strike)-min(strike), strategy=ifelse(putCall[1]=="C", paste0(putCall[1], "", min(strike),"/",max(strike)), paste0(putCall[1], "", max(strike),"/",min(strike))), contracts=mean(abs(quantity)), credit=sum(tradePrice*sign(quantity)*(-1)), commissions_opened=sum(ibCommission), date_closed=min(timestamp_closed), debit=sum(tradePrice_closed*sign(quantity)*(-1)), commissions_closed=sum(ibCommission_closed), PnL_strategies_IB=sum(fifoPnlRealized_closed), PnL_strategies_recomputed=(as.numeric(credit)-as.numeric(debit))*contracts*100 + commissions_opened + commissions_closed) %>% ungroup()

#if the short option is rolled, replace long leg based on existing open contracts at zero costs
trades_strategies_spreads <- trades_strategies_spreads %>% group_by(expiry,putCall) %>% mutate(strike_long = ifelse(wing==0, lag(strike_long), strike_long), strategy = ifelse(wing==0, paste0(putCall, "", strike_short,"/",strike_long), strategy), debit = ifelse(wing==0, 0, debit), commissions_closed = ifelse(wing==0, 0, commissions_closed), wing = ifelse(wing==0, abs(strike_long-strike_short), wing)) %>% mutate(type=ifelse(putCall=="C", "Call Credit Spread", "Put Credit Spread")) %>% ungroup()

#combine all strategies
if(include_complex_strategies) trades_strategies <- rbind(trades_strategies_spreads, trades_strategies_complex) else trades_strategies <- trades_strategies_spreads


#define final PnL and strike (=strike_short) and add SPX at open  
trades_strategies <- trades_strategies %>% mutate(day_opened=as.POSIXct(paste0(date(date_opened), " 12:00:00"), tz = "America/New_York")) %>% mutate(minute=round_date(date_opened,unit = "1minute")) %>% left_join(spx_1min %>% select(timestamp, Close) %>% rename(SPX_at_open=Close, minute=timestamp)) %>% mutate(PnL=PnL_strategies_recomputed, strike=strike_short) %>% left_join(vix_1min %>% select(timestamp, Close) %>% rename(VIX_at_open=Close, minute=timestamp))

#verify consistency of fifoPnl
print(trades %>% group_by(expiry) %>% summarize(PnL_trades_original_IB=sum(fifoPnlRealized, na.rm=T)) %>% full_join(trades_opened_closed %>% group_by(expiry) %>% summarize(PnL_trades_opened_close=sum(fifoPnlRealized_closed, na.rm=T), PnL_opened_closed_recomputed=sum(Pnl_recomputed, na.rm=T))) %>% full_join(trades_strategies %>% group_by(expiry) %>% summarize(PnL_strategies_IB=sum(PnL_strategies_IB, na.rm=T), PnL_strategies_recomputed=sum(PnL_strategies_recomputed, na.rm=T))) %>% as.data.frame())

#Some basic statistics
performance_stats_type <- trades_strategies %>% group_by(type) %>% summarize(AvgGain=mean(PnL[PnL>0]), AvgLoss=mean(PnL[PnL<0]), PoP=sum(sign(PnL[PnL>0]))/length((PnL))*100, NumTrades=length(PnL), Total=sum(PnL), CaptureRate=100*Total/sum(credit*contracts*100), Commissions=sum(commissions_opened+commissions_closed)) %>% as.data.frame()
performance_stats <- trades_strategies %>% summarize(AvgGain=mean(PnL[PnL>0]), AvgLoss=mean(PnL[PnL<0]), PoP=sum(sign(PnL[PnL>0]))/length((PnL))*100, NumTrades=length(PnL), Total=sum(PnL), CaptureRate=100*Total/sum(credit*contracts*100), Commissions=sum(commissions_opened+commissions_closed), AvgGainContract=mean(PnL[PnL>0]/contracts[PnL>0]), AvgLossContract=mean(PnL[PnL<0]/contracts[PnL<0])) %>% as.data.frame()
print(performance_stats_type)









########### Figures ##############
spx_1min_fortrades <- spx_1min %>% filter(timestamp>=personal_start_date)

x_min <- as.POSIXct(paste0(date(min(spx_1min_fortrades$timestamp)), " 00:00:00"), tz = "America/New_York")
x_max <- as.POSIXct(paste0(as.Date(today()), " 23:59:59"), tz = "America/New_York")
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
  #Credit Spreads
  geom_segment(data=trades_strategies %>% filter(str_detect(type, "Credit Spread")), aes(x = timestamp_chr, y = strike, xend = timestamp_chr_closed, yend = strike, colour = type), size=2) +
  geom_segment(data=trades_strategies %>% filter(str_detect(type, "Credit Spread")), aes(x = timestamp_chr, y = strike_long, xend = timestamp_chr_closed, yend = strike_long, colour = type), size=0.5) + 
  xlab("") + ylab("SPX") + 
  scale_colour_manual(values = c("Put Credit Spread"="blue", "Call Credit Spread"="orange", "Iron Butterfly"="green"), labels=c("Put Credit Spread"="Put Credit Spread", "Call Credit Spread"="Call Credit Spread", "Iron Butterfly"="Iron Butterfly"), name="Strategy")
#Add labels if gains or losses there
if(nrow(trades_strategies %>% filter(str_detect(type, "Credit Spread")) %>% filter(PnL>0))>0) p_candlestick_trades <- p_candlestick_trades + geom_label(data=trades_strategies %>% filter(str_detect(type, "Credit Spread")) %>% filter(PnL>0), aes(x = timestamp_chr_middle, y = strike + ifelse(putCall=="P", -shiftlabel,+shiftlabel), label=str_glue("{contracts}x{format(credit, digits = 2, nsmall = 2)}{ifelse(as.numeric(debit)>0,paste0('>',format(debit, digits = 2, nsmall = 2)),'')} \n +{round(PnL)}$")), size=3, color="darkgreen", alpha=0.5)
if(nrow(trades_strategies %>% filter(str_detect(type, "Credit Spread")) %>% filter(PnL<0))>0) p_candlestick_trades <- p_candlestick_trades + geom_label(data=trades_strategies %>% filter(str_detect(type, "Credit Spread")) %>% filter(PnL<0), aes(x = timestamp_chr_middle, y = strike + ifelse(putCall=="P", -shiftlabel,+shiftlabel), label=str_glue("{contracts}x{format(credit, digits = 2, nsmall = 2)}{ifelse(as.numeric(format(debit, digits = 2, nsmall = 2))>0,paste0('>',format(debit, digits = 2, nsmall = 2)),'')} \n {round(PnL)}$")), size=3, color="darkred", alpha=0.5)
  #Add Iron Butterflys
if(nrow(trades_strategies %>% filter(type=="Iron Butterfly"))>0) p_candlestick_trades <- p_candlestick_trades + geom_segment(data=trades_strategies %>% filter(type=="Iron Butterfly"), aes(x = timestamp_chr, y = strike, xend = timestamp_chr_closed, yend = strike, color = type), size=2)
  if(nrow(trades_strategies %>% filter(type=="Iron Butterfly") %>% filter(PnL>0))>0) p_candlestick_trades <- p_candlestick_trades + geom_label(data=trades_strategies %>% filter(type=="Iron Butterfly") %>% filter(PnL>0), aes(x = timestamp_chr_middle, y = strike+shiftlabel, label=str_glue("IF:{contracts}x{format(credit, digits = 2, nsmall = 2)}>{format(debit, digits = 2, nsmall = 2)} \n +{round(PnL)}$")), size=3, color="darkgreen", alpha=0.5)
  if(nrow(trades_strategies %>% filter(type=="Iron Butterfly") %>% filter(PnL<0))>0) p_candlestick_trades <- p_candlestick_trades + geom_label(data=trades_strategies %>% filter(type=="Iron Butterfly") %>% filter(PnL<0), aes(x = timestamp_chr_middle, y = strike +shiftlabel, label=str_glue("IF:{contracts}x{format(credit, digits = 2, nsmall = 2)}>{format(debit, digits = 2, nsmall = 2)} \n {round(PnL)}$")), size=3, color="darkred", alpha=0.5)
print(p_candlestick_trades)

#PnL
my_breaks_no_weekends <- paste(unique(spx_1min_fortrades$date), "09:30:00")
trades_strategies_for_pnl <- trades_strategies %>% mutate(day_opened= paste(as.Date(day_opened),"09:30:00")) %>% group_by(day_opened) %>% summarize(PnL=sum(PnL)) %>% mutate(Total=cumsum(PnL)) %>% complete(day_opened=my_breaks_no_weekends) %>% mutate(day_opened_midday=paste(date(day_opened), "12:45:00")) %>% mutate(PnL=ifelse(day_opened==paste(as.Date(today()),"09:30:00"), 0, PnL)) %>% mutate(Total=zoo::na.locf(Total, na.rm=F))
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
    geom_label(aes(x = my_breaks_no_weekends[2], y = performance_stats$Total*.7, hjust = "left", label=paste0("PoP = ", sprintf(performance_stats$PoP, fmt = "%.1f%%") ,"\n", "Avg. Gain = ", sprintf(performance_stats$AvgGainContract, fmt = "%.0f$"),"\n", "Avg. Loss = ", sprintf(performance_stats$AvgLossContract, fmt = "%.0f$") ,"\n", "Capture = ", sprintf(performance_stats$CaptureRate, fmt = "%.1f%%"), "\n", "Commissions = ", sprintf(performance_stats$Commissions, fmt = "%.2f$"))), size=3, fill="grey90", alpha=0.6)
 #+ geom_line(data=trades_strategies_for_pnl %>% mutate(planned_PnL=ifelse(is.na(PnL),0,daily_goal), planned_Total=cumsum(planned_PnL)), aes(x = day_opened_midday, y = planned_Total, group=1), size=0.5, color="blue", linetype="dashed")
print(p_total_line)

#Line chart relative
trades_strategies_for_pnl_relative <- (trades_strategies_for_pnl %>% mutate(date=as.character(date(day_opened))) %>% left_join(spx_1min %>% filter(date>=date(personal_start_date)) %>% group_by(date) %>% summarize(SPX_close=last(Close)))) %>% mutate(relative_gain=(Total/personal_account_size), SPX=SPX_close/SPX_close[1]-1)
p_total_relative <- ggplot(data = trades_strategies_for_pnl_relative) +
  theme_tq() + scale_x_discrete(breaks = my_breaks, drop=F, labels = format(as.POSIXct(my_breaks, tz="America/New_York"), "%e %b"), expand = c(0, 0)) + scale_y_continuous(labels = scales::percent_format(accuracy=.01)) +
  geom_line(data = last(spx_1min_fortrades), aes(x = timestamp_chr, y = Close*0, group=1), color="white", alpha=0) + 
  geom_line(aes(x = day_opened_midday, y = relative_gain, group=1), color="blue", size=2) +
  geom_line(aes(x = day_opened_midday, y = SPX, group=1), color="steelblue2", size=1) + xlab("") +
  ylab("Relative Performance") + geom_text(aes(x = day_opened, y = 0), label="   ") + annotate("text", x = 2, y=0.06, hjust = 0, label="Portfolio", color="blue") + annotate("text", x = 2, y=0.04, hjust = 0, label="S&P500", color="steelblue2")
print(p_total_relative)

#VIX plot
vix_1min_fortrades <- vix_1min %>% filter(timestamp>=personal_start_date)
#now only 9:30 - 15:59 for plotting
vix_1min_fortrades <- vix_1min_fortrades[vix_1min_fortrades$time >= "09:30:00" & vix_1min_fortrades$time <= "15:59:00",]
#create empty data for SPX until x_max
for(.day in seq(date(last(vix_1min_fortrades$timestamp))+1, date(x_max), by = "day")){
  vix_empty <- vix_1min_fortrades %>% filter(date==date(x_min)) %>% mutate(Open=as.numeric(NA),High=as.numeric(NA),Low=as.numeric(NA),Close=as.numeric(NA)) %>% mutate(date=as.character(as.Date(.day)), timestamp=as.POSIXct(paste0(date, " ",time), tz = "America/New_York"))
  vix_1min_fortrades <- rbind(vix_1min_fortrades, vix_empty)
}
#For plotting now only trading hours
vix_1min_fortrades <- vix_1min_fortrades %>%
  mutate(timestamp_chr = as.character(timestamp), day = lubridate::day(timestamp),hour = lubridate::hour(timestamp),minute = lubridate::minute(timestamp),new_day = if_else(day != lag(day) | is.na(lag(day)), 1, 0))
vix_plot <- ggplot(vix_1min_fortrades %>% mutate(Close=ifelse(new_day==1,NA,Close)), aes(x = timestamp_chr, y = Close, group = 1)) + geom_line(na.rm = F) + theme_tq() + scale_x_discrete(breaks = my_breaks, drop=F, labels = format(as.POSIXct(my_breaks, tz="America/New_York"), "%e %b"), expand = c(0, 0)) + xlab("") + ylab("VIX")

#Arranged combined figure
#print(ggarrange(p_candlestick_trades + theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))), p_PnL_bar + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()), p_total_line, nrow = 3, heights = c(.6,.2,.2), common.legend = T, legend = "bottom", hjust=c(0, 0, 0)))
print(ggarrange(p_candlestick_trades + theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))), vix_plot, p_PnL_bar + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()), p_total_line + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()), nrow = 4, heights = c(.55,.15,.15,.15), common.legend = T, legend = "bottom", hjust=c(0, 0, 0, 0)))
#now full also (wide scaling the width by months in the data)
ggsave("Trading_log_full.pdf", device = "pdf", width = 15*interval(personal_start_date, now()) / months(1), height = 10)
################ END FIGURE ##################
trades_strategies_full <- trades_strategies















############################ loop over monthly figures #######################################
#get list of all months
month_list <- seq(as.yearmon(personal_start_date),as.yearmon(now()), 1/12)
current_month <- month_list[1]

for(current_month in month_list){

spx_1min_fortrades <- spx_1min %>% filter(timestamp>=personal_start_date) %>% filter(as.yearmon(date)==current_month)

x_min <- as.POSIXct(paste0(date(min(spx_1min_fortrades$timestamp)), " 00:00:00"), tz = "America/New_York")
x_max <- as.POSIXct(paste0(date(max(spx_1min_fortrades$timestamp)), " 23:59:59"), tz = "America/New_York")
#For plotting now only trading hours
spx_1min_fortrades <- spx_1min_fortrades %>%
  mutate(timestamp_chr = as.character(timestamp), day = lubridate::day(timestamp),hour = lubridate::hour(timestamp),minute = lubridate::minute(timestamp),new_day = if_else(day != lag(day) | is.na(lag(day)), 1, 0))
trades_strategies <- trades_strategies_full %>% filter(as.yearmon(date_opened)==current_month) %>% 
  mutate(timestamp_chr = as.character(date_opened), day = lubridate::day(timestamp),hour = lubridate::hour(timestamp),minute = lubridate::minute(timestamp),timestamp_chr_closed = as.character(date_closed), timestamp_chr_middle=as.character(as.POSIXct((as.numeric(date_closed) + as.numeric(date_opened)) / 2, origin = '1970-01-01', tz="America/New_York")))

my_breaks <-spx_1min_fortrades$timestamp_chr[seq.int(1,length(spx_1min_fortrades$timestamp_chr) , by = 60*6.5)] #every day 6.5 trading hours!
my_breaks <- paste(seq.Date(date(x_min), date(x_max), by = "day"), "09:30:00")

#in ggplot: Candlestick Chart
shiftlabel=20;
p_candlestick_trades <- ggplot(spx_1min_fortrades %>% mutate(Close=ifelse(new_day==1,NA,Close)), aes(x = timestamp_chr, y = Close, group = 1)) +  
  geom_line(na.rm = F) +
  theme_tq() + scale_x_discrete(breaks = my_breaks, drop=F, labels = format(as.POSIXct(my_breaks, tz="America/New_York"), "%e %b"), expand = c(0, 0)) +
  #Credit Spreads
  geom_segment(data=trades_strategies %>% filter(str_detect(type, "Credit Spread")), aes(x = timestamp_chr, y = strike, xend = timestamp_chr_closed, yend = strike, colour = type), size=2) +
  geom_segment(data=trades_strategies %>% filter(str_detect(type, "Credit Spread")), aes(x = timestamp_chr, y = strike_long, xend = timestamp_chr_closed, yend = strike_long, colour = type), size=0.5) + 
  xlab("") + ylab("SPX") + 
  scale_colour_manual(values = c("Put Credit Spread"="blue", "Call Credit Spread"="orange", "Iron Butterfly"="green"), labels=c("Put Credit Spread"="Put Credit Spread", "Call Credit Spread"="Call Credit Spread", "Iron Butterfly"="Iron Butterfly"), name="Strategy")
#Add labels if gains or losses there
if(nrow(trades_strategies %>% filter(str_detect(type, "Credit Spread")) %>% filter(PnL>0))>0) p_candlestick_trades <- p_candlestick_trades + geom_label(data=trades_strategies %>% filter(str_detect(type, "Credit Spread")) %>% filter(PnL>0), aes(x = timestamp_chr_middle, y = strike + ifelse(putCall=="P", -shiftlabel,+shiftlabel), label=str_glue("{contracts}x{format(credit, digits = 2, nsmall = 2)}{ifelse(as.numeric(debit)>0,paste0('>',format(debit, digits = 2, nsmall = 2)),'')} \n +{round(PnL)}$")), size=3, color="darkgreen", alpha=0.5)
if(nrow(trades_strategies %>% filter(str_detect(type, "Credit Spread")) %>% filter(PnL<0))>0) p_candlestick_trades <- p_candlestick_trades + geom_label(data=trades_strategies %>% filter(str_detect(type, "Credit Spread")) %>% filter(PnL<0), aes(x = timestamp_chr_middle, y = strike + ifelse(putCall=="P", -shiftlabel,+shiftlabel), label=str_glue("{contracts}x{format(credit, digits = 2, nsmall = 2)}{ifelse(as.numeric(format(debit, digits = 2, nsmall = 2))>0,paste0('>',format(debit, digits = 2, nsmall = 2)),'')} \n {round(PnL)}$")), size=3, color="darkred", alpha=0.5)
#Add Iron Butterflys
if(nrow(trades_strategies %>% filter(type=="Iron Butterfly"))>0) p_candlestick_trades <- p_candlestick_trades + geom_segment(data=trades_strategies %>% filter(type=="Iron Butterfly"), aes(x = timestamp_chr, y = strike, xend = timestamp_chr_closed, yend = strike, color = type), size=2)
if(nrow(trades_strategies %>% filter(type=="Iron Butterfly") %>% filter(PnL>0))>0) p_candlestick_trades <- p_candlestick_trades + geom_label(data=trades_strategies %>% filter(type=="Iron Butterfly") %>% filter(PnL>0), aes(x = timestamp_chr_middle, y = strike+shiftlabel, label=str_glue("IF:{contracts}x{format(credit, digits = 2, nsmall = 2)}>{format(debit, digits = 2, nsmall = 2)} \n +{round(PnL)}$")), size=3, color="darkgreen", alpha=0.5)
if(nrow(trades_strategies %>% filter(type=="Iron Butterfly") %>% filter(PnL<0))>0) p_candlestick_trades <- p_candlestick_trades + geom_label(data=trades_strategies %>% filter(type=="Iron Butterfly") %>% filter(PnL<0), aes(x = timestamp_chr_middle, y = strike +shiftlabel, label=str_glue("IF:{contracts}x{format(credit, digits = 2, nsmall = 2)}>{format(debit, digits = 2, nsmall = 2)} \n {round(PnL)}$")), size=3, color="darkred", alpha=0.5)
print(p_candlestick_trades)

#PnL
my_breaks_no_weekends <- paste(unique(spx_1min_fortrades$date), "09:30:00")
trades_strategies_for_pnl <- trades_strategies %>% mutate(day_opened= paste(as.Date(day_opened),"09:30:00")) %>% group_by(day_opened) %>% summarize(PnL=sum(PnL)) %>% mutate(Total=cumsum(PnL)) %>% complete(day_opened=my_breaks_no_weekends) %>% mutate(day_opened_midday=paste(date(day_opened), "12:45:00")) %>% mutate(PnL=ifelse(day_opened==paste(as.Date(today()),"09:30:00"), 0, PnL)) %>% mutate(Total=zoo::na.locf(Total, na.rm=F))
trades_strategies_for_pnl <- trades_strategies_for_pnl %>% filter(as.yearmon(day_opened_midday)==current_month)
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
  ylab("Total") + theme(legend.position = "none") + geom_text(data = trades_strategies_for_pnl %>% filter(!is.na(PnL)), aes(x = day_opened_midday, y = Total+ifelse(PnL<0,-1,+1)*shiftlabel_dollar*2, label=sprintf('%+.0f$', Total)), color="blue") 
  # + geom_label(aes(x = my_breaks_no_weekends[2], y = performance_stats$Total*.7, hjust = "left", label=paste0("PoP = ", sprintf(performance_stats$PoP, fmt = "%.1f%%") ,"\n", "Avg. Gain = ", sprintf(performance_stats$AvgGainContract, fmt = "%.0f$"),"\n", "Avg. Loss = ", sprintf(performance_stats$AvgLossContract, fmt = "%.0f$") ,"\n", "Capture = ", sprintf(performance_stats$CaptureRate, fmt = "%.1f%%"), "\n", "Commissions = ", sprintf(performance_stats$Commissions, fmt = "%.2f$"))), size=3, fill="grey90", alpha=0.6) 
# + geom_line(data=trades_strategies_for_pnl %>% mutate(planned_PnL=ifelse(is.na(PnL),0,daily_goal), planned_Total=cumsum(planned_PnL)), aes(x = day_opened_midday, y = planned_Total, group=1), size=0.5, color="blue", linetype="dashed")
print(p_total_line)

#Line chart relative
trades_strategies_for_pnl_relative <-trades_strategies_for_pnl  %>% filter(as.yearmon(day_opened)==current_month)
trades_strategies_for_pnl_relative <- (trades_strategies_for_pnl_relative %>% mutate(date=as.character(date(day_opened))) %>% left_join(spx_1min %>% filter(as.yearmon(date)==current_month) %>% filter(date>=date(personal_start_date)) %>% group_by(date) %>% summarize(SPX_close=last(Close)))) %>% mutate(relative_gain=(Total/personal_account_size), SPX=SPX_close/SPX_close[1]-1)
p_total_relative <- ggplot(data = trades_strategies_for_pnl_relative) +
  theme_tq() + scale_x_discrete(breaks = my_breaks, drop=F, labels = format(as.POSIXct(my_breaks, tz="America/New_York"), "%e %b"), expand = c(0, 0)) + scale_y_continuous(labels = scales::percent_format(accuracy=.01)) +
  geom_line(data = last(spx_1min_fortrades), aes(x = timestamp_chr, y = Close*0, group=1), color="white", alpha=0) + 
  geom_line(aes(x = day_opened_midday, y = relative_gain, group=1), color="blue", size=2) +
  geom_line(aes(x = day_opened_midday, y = SPX, group=1), color="steelblue2", size=1) + xlab("") +
  ylab("Relative Performance") + geom_text(aes(x = day_opened, y = 0), label="   ") + annotate("text", x = 2, y=0.06, hjust = 0, label="Portfolio", color="blue") + annotate("text", x = 2, y=0.04, hjust = 0, label="S&P500", color="steelblue2")
print(p_total_relative)

#VIX plot
vix_1min_fortrades <- vix_1min %>% filter(timestamp>=personal_start_date) %>% filter(as.yearmon(date)==current_month)
#now only 9:30 - 15:59 for plotting
vix_1min_fortrades <- vix_1min_fortrades[vix_1min_fortrades$time >= "09:30:00" & vix_1min_fortrades$time <= "15:59:00",]
#For plotting now only trading hours
vix_1min_fortrades <- vix_1min_fortrades %>%
  mutate(timestamp_chr = as.character(timestamp), day = lubridate::day(timestamp),hour = lubridate::hour(timestamp),minute = lubridate::minute(timestamp),new_day = if_else(day != lag(day) | is.na(lag(day)), 1, 0))
vix_plot <- ggplot(vix_1min_fortrades %>% mutate(Close=ifelse(new_day==1,NA,Close)), aes(x = timestamp_chr, y = Close, group = 1)) + geom_line(na.rm = F) + theme_tq() + scale_x_discrete(breaks = my_breaks, drop=F, labels = format(as.POSIXct(my_breaks, tz="America/New_York"), "%e %b"), expand = c(0, 0)) + xlab("") + ylab("VIX")
#Arranged combined figure
#print(ggarrange(p_candlestick_trades + theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))), p_PnL_bar + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()), p_total_line, nrow = 3, heights = c(.6,.2,.2), common.legend = T, legend = "bottom", hjust=c(0, 0, 0)))
p_arranged <- ggarrange(p_candlestick_trades + theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))), p_PnL_bar + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()), p_total_line + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()), p_total_relative + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()), nrow = 4, heights = c(.55,.15,.15,.15), common.legend = T, legend = "bottom", hjust=c(0, 0, 0, 0))

#Some basic statistics
performance_stats_type <- trades_strategies %>% group_by(type) %>% summarize(AvgGain=mean(PnL[PnL>0]), AvgLoss=mean(PnL[PnL<0]), PoP=sum(sign(PnL[PnL>0]))/length((PnL))*100, NumTrades=length(PnL), Total=sum(PnL), CaptureRate=100*Total/sum(credit*contracts*100), Commissions=sum(commissions_opened+commissions_closed)) %>% as.data.frame()
performance_stats <- trades_strategies %>% summarize(AvgGain=mean(PnL[PnL>0]), AvgLoss=mean(PnL[PnL<0]), PoP=sum(sign(PnL[PnL>0]))/length((PnL))*100, NumTrades=length(PnL), Total=sum(PnL), CaptureRate=100*Total/sum(credit*contracts*100), Commissions=sum(commissions_opened+commissions_closed), AvgGainContract=mean(PnL[PnL>0]/contracts[PnL>0]), AvgLossContract=mean(PnL[PnL<0]/contracts[PnL<0])) %>% as.data.frame()

annotate_figure(p_arranged,
                top = text_grob(as.yearmon(current_month), color = "black", just = "left", face = "bold", size = 14),
                bottom = text_grob(paste0("PoP = ", sprintf(performance_stats$PoP, fmt = "%.1f%%") ," ", "Avg. Gain = ", sprintf(performance_stats$AvgGainContract, fmt = "%.0f$")," ", "Avg. Loss = ", sprintf(performance_stats$AvgLossContract, fmt = "%.0f$") ," ", "Capture = ", sprintf(performance_stats$CaptureRate, fmt = "%.1f%%"), " ", "Commissions = ", sprintf(performance_stats$Commissions, fmt = "%.2f$")), color = "black", just="center", size = 10)
                )
ggsave(str_glue("Trading_log_{gsub(' ','_',as.yearmon(current_month))}.pdf"), device = "pdf", width = 15, height = 10)
}
################ END FIGURE ##################

















