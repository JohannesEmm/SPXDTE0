#if(!is.na(ping_port(port=7496,destination = "localhost", count = 1))){
  tws <- twsConnect(verbose = T)
  contract <- twsIndex('SPX','CBOE')
  #reqRealTimeBars # by default retreives 30 days of daily data
  spx_last_month_1min <- reqHistoricalData(tws, Contract=contract, barSize = "1 min", useRTH = "1", duration = "60 d")
  tzone(spx_last_month_1min) <- "America/New_York"
  spx_last_month_1min <- data.frame(DateTime=index(spx_last_month_1min),coredata(spx_last_month_1min[,1:4]))
  names(spx_last_month_1min) <- c("DateTime", "Open", "High", "Low", "Close")
  spx_last_month_1min <- spx_last_month_1min %>% mutate(timestamp=as.POSIXct(DateTime, tz = "America/New_York")) %>% separate(DateTime, c("date", "time"), sep = " ")
  #now update spx_1min
  spx_1min_new <- spx_last_month_1min %>% filter(timestamp>max(spx_1min$timestamp))  
  
  #Clean day data, adding empty trading times, removing outside RTH dates
  fullday <- data.frame(time=format(seq(as.POSIXct("2001-01-01 09:30:00", tz="EST"), length.out=6.5*60, by='1 min'), '%H:%M:%S'))
  spx_1min_new <- spx_1min_new %>% filter(time %in% fullday$time) 
  #get days which are not complete
  partly_days <- spx_1min_new %>% group_by(date) %>% summarize(numrows=length(time)) %>% filter(numrows!=390)
  filled_partly_days <- spx_1min_new %>% filter(date %in% partly_days$date)
  filled_partly_days_new <- expand.grid(date=partly_days$date, time=fullday$time)
  filled_partly_days <- filled_partly_days %>% full_join(expand.grid(date=partly_days$date, time=fullday$time))
  filled_partly_days <- filled_partly_days %>% mutate(timestamp=as.POSIXct(paste(date, time), tz = "EST"))
  #replace old partly days by filled dates and sort again
  spx_1min_new <- spx_1min_new %>% filter(!(date %in% partly_days$date))
  spx_1min_new <- rbind(spx_1min_new, filled_partly_days)
  spx_1min_new <- spx_1min_new %>% arrange(timestamp)
  #combine with existin spx data
  spx_1min <- rbind(spx_1min, spx_1min_new)
  #now get VIX also
  contract <- twsIndex('VIX','CBOE')
  #reqRealTimeBars # by default retreives 30 days of daily data
  vix_last_month_1min <- reqHistoricalData(tws, Contract=contract, barSize = "1 min", useRTH = "1", duration = "60 d")
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
#}
