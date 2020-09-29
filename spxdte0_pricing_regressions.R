
require_package <- function(package){
  if(!is.element(package, .packages(all.available = TRUE))){
    try(install.packages(package, repos="http://cran.rstudio.com"), silent = TRUE)
  }
  suppressPackageStartupMessages(library(package,character.only=T, quietly = TRUE))  
}
pkgs <- c('data.table', 'tidyverse', 'ggrepel', 'ggpubr', 'quantmod', 'tidyquant', 'plotly', 'pingr', 'lubridate', 'XML', 'IBrokers', 'IButils', 'zoo', 'scales', 'rstudioapi')
res <- lapply(pkgs, require_package)

#get and update 1min SPX data
load("spx_1min.Rdata"); load("vix_1min.Rdata"); 
#compute daily data
spx_day <- spx_1min %>% group_by(date) %>% summarize(Open=first(Open), High=max(High), Low=min(Low), Close=last(Close)) %>% mutate(range=High-Low, closediff=Close-lag(Close), relchange_day=(Close/Open-1)*100, relchange_24h=(Close/lag(Close)-1)*100, range_rel=range/Open*100) %>% mutate(year=as.character(year(date)), weekday=wday(date, label=T), date_nice = format(as.POSIXct(date, tz="America/New_York"), "%d %b '%y"), dummy=1)


vix_1min <- vix_1min[vix_1min$time >= "09:30:00" & vix_1min$time <= "15:59:00",]
vix_day <- vix_1min %>% group_by(date) %>% summarize(vixOpen=first(Open), vixHigh=max(High), vixLow=min(Low), vixClose=last(Close)) %>% mutate(year=as.character(year(date)), weekday=wday(date, label=T), date_nice = format(as.POSIXct(date, tz="America/New_York"), "%d %b '%y"), dummy=1)


spx_day <- spx_day %>% right_join(vix_day)

summary(lm(formula = "range ~ vixOpen", data = spx_day %>% mutate(vix2=vixOpen^2)))
ggplot(spx_day) + geom_point(aes(vixOpen,range))






load("trades_for_models.RData") #loads trades_for_models
trades_for_models <- trades_for_models %>% select(strike, putCall, timestamp, SPX_at_open, VIX_at_open, tradePrice, timestamp_closed, SPX_at_closed, VIX_at_closed, tradePrice_closed)







#IB API get SPXW options
#option1 <- twsOption("SPXW  200708C03130000")
#option1_data <- reqMktData(tws, option1, snapshot = T) 





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


