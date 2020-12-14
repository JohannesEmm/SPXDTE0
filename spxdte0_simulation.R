require(tidyverse)



rbinom(n = 1000, size = 1, prob = 1/6)


data.frame(replicate(10,rbinom(n = 1000, size = 1, prob = 1/6)))



n = 1 #number of contracts
slippage = 0.3 #USD
pi = 1.0 #average premium on either side
p = 1/6 #probability of loss
k = 2 #amount times premium for stop loss as max (k=2 = STP at 3x)

EX <- function(t=1) t*100*n*((1-p)*2*pi-2*p*(k*pi+slippage))

VARX <- function(t=1) t*((100*n)^2*(2*p*(1-p))*(((1-k)*pi-slippage)^2 + 2*2*pi*(k*pi+slippage)))

pnltable <- data.frame(t=seq(1,150)) %>% mutate(EX=EX(t), Xlo=EX-1.96*sqrt(VARX(t)), Xup=EX+1.96*sqrt(VARX(t)))

ggplot(pnltable) + geom_line(aes(t,EX), color="blue") + geom_ribbon(aes(t, ymin=Xlo, ymax=Xup), fill="blue", alpha=0.5) + xlab("1 Trading year") + ylab("Total PnL")




