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

