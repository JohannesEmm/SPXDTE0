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

#get and update 1min SPX data
load("spx_1min.Rdata"); load("vix_1min.Rdata"); 
#compute daily data
spx_day <- spx_1min %>% group_by(date) %>% summarize(Open=first(Open), High=max(High), Low=min(Low), Close=last(Close)) %>% mutate(range=High-Low, closediff=Close-lag(Close), relchange_day=(Close/Open-1)*100, relchange_24h=(Close/lag(Close)-1)*100, range_rel=range/Open*100) %>% mutate(year=as.character(year(date)), weekday=wday(date, label=T), date_nice = format(as.POSIXct(date, tz="America/New_York"), "%d %b '%y"), dummy=1)
spx_day_stats <- spx_day %>% summarize(quantile = scales::percent(c(0.01, 0.05, 0.10, 0.90, 0.95,0.99)), relchange_day = quantile(relchange_day, c(0.01, 0.05, 0.10, 0.90, 0.95,0.99)), relchange_24h = quantile(relchange_24h, c(0.01, 0.05, 0.10, 0.90, 0.95,0.99), na.rm = T), closediff = quantile(closediff, c(0.01, 0.05, 0.10, 0.90, 0.95,0.99), na.rm = T), range = quantile(range, c(0.01, 0.05, 0.10, 0.90, 0.95,0.99)), range_rel = quantile(range_rel, c(0.01, 0.05, 0.10, 0.90, 0.95,0.99))) %>% as.data.table()
last_spx=tail(spx_day,1)
#ggplot(spx_day) + geom_histogram(aes(range))
#ggplot(spx_day) + geom_histogram(aes(closediff))
#spx_day %>% plot_ly(x = ~date, type="candlestick",open = ~Open, close = ~Close,high = ~High, low = ~Low) %>% layout(title="SPX Daily")


#weekday plots: boxplot, violin plot, or beeswarm
ggplot(data = spx_day, aes(x=weekday,y=range,fill=weekday)) + theme(legend.position = "none") + xlab("") + ylab("Daily range per weekday") + ylim(0,200) + geom_violin(draw_quantiles = c(0.05, 0.5, 0.95))#  + geom_boxplot()


#Daily replicate of facebook graph #library(beeswarm) #library(ggbeeswarm)

#Day change
p_day <- ggplot(data = spx_day, aes(y=dummy, x=relchange_day)) + xlab("") + ylab("Within day change")  + geom_violin(draw_quantiles = c(0.05, 0.5, 0.95), orientation = "y") + geom_point(data=spx_day %>% slice_tail(., n=5), aes(y=dummy, x=relchange_day, color=weekday), size=2) + theme(legend.position = "bottom") + geom_text_repel(data = spx_day %>% filter(abs(relchange_day)>5), aes(y=dummy, x=relchange_day, label=date_nice), direction = "y") + geom_point(data = spx_day %>% filter(abs(relchange_day)>5), aes(y=dummy, x=relchange_day)) +  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + guides(color=guide_legend(title="Last Five days")) + annotate("rect", xmin=spx_day_stats[quantile=="5.0%",]$relchange_day, xmax=spx_day_stats[quantile=="95.0%",]$relchange_day, ymin=-Inf, ymax=Inf, alpha=0.2, fill="blue") + annotate("text", size=3, x=spx_day_stats[quantile=="5.0%",]$relchange_day, y=1.4, color="blue", label=paste0("90% range:", round(spx_day_stats[quantile=="5.0%",]$relchange_day,1),"-",round(spx_day_stats[quantile=="95.0%",]$relchange_day,1), "%", "\n (",round(spx_day_stats[quantile=="5.0%",]$relchange_day/100*last_spx$Close,0)," - +",round(spx_day_stats[quantile=="95.0%",]$relchange_day/100*last_spx$Close,0)," Points)"), hjust=0) + scale_x_continuous(limits = c(min(spx_day$relchange_24h), max(spx_day$range_rel)), breaks = pretty_breaks(n = 20), labels = function(x) sprintf("%+.1f", x))

#24h change (previous close to close)
p_24h <- ggplot(data = spx_day, aes(y=dummy, x=relchange_24h)) + xlab("") + ylab("Close to Close Change")  + geom_violin(draw_quantiles = c(0.05, 0.5, 0.95), orientation = "y") + geom_point(data=spx_day %>% slice_tail(., n=5), aes(y=dummy, x=relchange_24h, color=weekday), size=2) + theme(legend.position = "bottom") + geom_text_repel(data = spx_day %>% filter(abs(relchange_24h)>5), aes(y=dummy, x=relchange_24h, label=date_nice), direction = "y") + geom_point(data = spx_day %>% filter(abs(relchange_24h)>5), aes(y=dummy, x=relchange_24h)) +  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + guides(color=guide_legend(title="Last Five days")) + annotate("rect", xmin=spx_day_stats[quantile=="5.0%",]$relchange_24h, xmax=spx_day_stats[quantile=="95.0%",]$relchange_24h, ymin=-Inf, ymax=Inf, alpha=0.2, fill="blue") + annotate("text", size=3, x=spx_day_stats[quantile=="5.0%",]$relchange_24h, y=1.4, color="blue", label=paste0("90% range:", round(spx_day_stats[quantile=="5.0%",]$relchange_24h,1),"-",round(spx_day_stats[quantile=="95.0%",]$relchange_24h,1), "%", "\n (",round(spx_day_stats[quantile=="5.0%",]$relchange_24h/100*last_spx$Close,0)," - +",round(spx_day_stats[quantile=="95.0%",]$relchange_24h/100*last_spx$Close,0)," Points)"), hjust=0) + scale_x_continuous(limits = c(min(spx_day$relchange_24h), max(spx_day$range_rel)), breaks = pretty_breaks(n = 20), labels = function(x) sprintf("%+.1f", x))

#daily range
p_range <- ggplot(data = spx_day, aes(y=dummy, x=range_rel)) + xlab("") + ylab("Daily range")  + geom_violin(draw_quantiles = c(0.05, 0.5, 0.95), orientation = "y") + geom_point(data=spx_day %>% slice_tail(., n=5), aes(y=dummy, x=range_rel, color=weekday), size=2) + theme(legend.position = "bottom") + geom_text_repel(data = spx_day %>% filter(abs(range_rel)>8), aes(y=dummy, x=range_rel, label=date_nice), direction = "y") + geom_point(data = spx_day %>% filter(abs(range_rel)>8), aes(y=dummy, x=range_rel)) +  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + guides(color=guide_legend(title="Last Five days")) + annotate("rect", xmin=0, xmax=spx_day_stats[quantile=="95.0%",]$range_rel, ymin=-Inf, ymax=Inf, alpha=0.2, fill="blue") + annotate("text", size=3, x=spx_day_stats[quantile=="5.0%",]$range_rel, y=1.4, color="blue", label=paste0("95% range:", "0 - ",round(spx_day_stats[quantile=="95.0%",]$range_rel,1), "%", "\n (0 - ",round(spx_day_stats[quantile=="95.0%",]$range_rel/100*last_spx$Close,0)," Points)"), hjust=0) + scale_x_continuous(limits = c(min(spx_day$relchange_24h), max(spx_day$range_rel)), breaks = pretty_breaks(n = 20), labels = function(x) sprintf("%+.1f", x))



#Finally, plots over the trading day time
spx_n_minute <- spx_1min %>% mutate(n_minute=floor_date(timestamp,unit = "15 minute")) %>% group_by(n_minute) %>% summarize(Open=first(Open), High=max(High), Low=min(Low), Close=last(Close)) %>% mutate(range=((High-Low)/Open)*100, closediff=Close-lag(Close), relchange_day=(Close/Open-1)*100, relchange_24h=(Close/lag(Close)-1)*100, range_rel=range/Open*100) %>% mutate(onlytime=format(n_minute, "%H:%M"), date=format(n_minute, "%d/%m/%Y")) %>% ungroup() %>% filter(onlytime!="16:00")
spx_n_minute <- spx_n_minute %>% group_by(date) %>% mutate(change_since_open=(Close/mean(Open[onlytime=="09:30"])-1)*100) %>% ungroup()
spx_n_minute_stats <- spx_n_minute %>% group_by(onlytime) %>% summarize(change_since_open_5pct=quantile(change_since_open, 0.05, na.rm = T), change_since_open_95pct=quantile(change_since_open, 0.95, na.rm = T), range_95pct=quantile(range, 0.95, na.rm = T)) %>% as.data.table()
#(1) box plot over (1,5,15) minute intervals, including last 5 days
p_overtime_range <- ggplot(data = spx_n_minute) + geom_violin(aes(x=onlytime, y=range, group=onlytime)) + xlab("") + ylab("Range per 15m") + scale_y_continuous(labels = function(x) paste0(sprintf("%+.1f", x),"%")) + geom_ribbon(data = spx_n_minute_stats, aes(x=onlytime, ymin=0, ymax=range_95pct, group=1), alpha=.2, fill="blue") + geom_line(data = spx_n_minute %>% slice_tail(., n=5*26) %>% mutate(weekday=wday(date, label=T)), aes(x=onlytime, y=range, color=weekday, group=weekday)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#(2) change from Open
p_overtime_change_from_close <- ggplot(data = spx_n_minute) + geom_violin(aes(x=onlytime, y=change_since_open, group=onlytime)) + xlab("") + ylab("Change since Open") + geom_ribbon(data = spx_n_minute_stats, aes(x=onlytime, ymin=change_since_open_5pct, ymax=change_since_open_95pct, group=1), alpha=.2, fill="blue")  + geom_line(data = spx_n_minute %>% slice_tail(., n=5*26) %>% mutate(weekday=wday(date, label=T)), aes(x=onlytime, y=change_since_open, color=weekday, group=weekday)) + guides(color=guide_legend(title="Last Five days")) + scale_y_continuous(labels = function(x) paste0(sprintf("%+.1f", x),"%"), breaks = pretty_breaks(n=10)) + 
  annotate("text", size=3, x="15:00", y=spx_n_minute_stats[onlytime=="15:45",]$change_since_open_95pct, color="blue", label=paste0(round(spx_n_minute_stats[onlytime=="15:45",]$change_since_open_95pct,1), "% [", round(spx_n_minute_stats[onlytime=="15:45",]$change_since_open_95pct/100*last_spx$Close,0)," Pts.]"), hjust=0) + 
  annotate("text", size=3, x="15:00", y=spx_n_minute_stats[onlytime=="15:45",]$change_since_open_5pct, color="blue", label=paste0(round(spx_n_minute_stats[onlytime=="15:45",]$change_since_open_5pct,1), "% [", round(spx_n_minute_stats[onlytime=="15:45",]$change_since_open_5pct/100*last_spx$Close,0)," Pts.]"), hjust=0) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#+ geom_hline(yintercept = round(spx_n_minute_stats[onlytime=="15:45",]$change_since_open_5pct), color="blue", alpha=0.2) + geom_hline(yintercept = round(spx_n_minute_stats[onlytime=="15:45",]$change_since_open_95pct), color="blue", alpha=0.2)

arranged_spx_analysis <- ggarrange(p_24h, p_day, p_range, 
          ggarrange(p_overtime_range, p_overtime_change_from_close, ncol=2, labels=c("d", "e"), legend = "none"),
nrow=4, common.legend = T, legend = "bottom", labels = c("a", "b", "c"))
#ggarrange(p_24h, p_day, p_range, nrow = 3, common.legend = T, legend = "bottom")
annotate_figure(arranged_spx_analysis, 
                bottom = text_grob(paste0("Data from 1 Jan '05 to ", last_spx$date_nice, ". S&P500 @ ", last_spx$Close), hjust = 1, x = 1, face = "italic", size = 12))
ggsave("SPX_Statistics.pdf", device = "pdf", width = 15, height = 10)
.
