library(shiny)
library(shinydashboard)


ui <- dashboardPage(skin = "black",

  dashboardHeader(title = "DTE0 Mood DashboaRd", titleWidth = 350),
  dashboardSidebar(width = 350,
  valueBoxOutput("SPX", width = 350),
  valueBoxOutput("VIX", width = 350),
  valueBoxOutput("TICK", width = 350),
  valueBoxOutput("TRIN", width = 350),
  valueBoxOutput("AD", width = 350),
  valueBoxOutput("WMI", width = 350)

  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    fluidRow(
      box(plotOutput("mainchart", height = 450))
      
    
  
)
)
)


server <- function(input, output, session) {
  # get trades from flex query (includes expired options!)
  # setup in IB: https://webhelp.tradingdiarypro.com/create_flex_querries_interactive_brokers.htm
  require_package <- function(package){
    if(!is.element(package, .packages(all.available = TRUE))){
      if (!"IButils" %in% rownames(installed.packages())) install.packages('IButils', type = 'source',repos = c('http://enricoschumann.net/R', getOption('repos')))
      try(install.packages(package, repos="http://cran.rstudio.com"), silent = TRUE)
    }
    suppressPackageStartupMessages(library(package,character.only=T, quietly = TRUE))  
  }
  pkgs <- c('data.table', 'tidyverse', 'ggrepel', 'ggpubr', 'quantmod', 'tidyquant', 'plotly', 'pingr', 'lubridate', 'XML', 'IBrokers', 'IButils', 'zoo', 'scales', 'rstudioapi', 'tis')
  res <- lapply(pkgs, require_package)
  
  autoInvalidate <- reactiveTimer(2000, session)
  
  # get real time data
  
  #tradingview webscraping
  #Packages
  library(rvest)
  library(tidyverse)
  require(quantmod)
  #markets_url <- read_html("https://www.tradingview.com/markets/indices/quotes-major/")
  #markets_table <- html_nodes(markets_url, css = "table")
  #markets_table <- html_table(markets_table, fill = T) %>% as.data.frame()
  
  if(!isConnected(tws)) tws <- twsConnect(verbose = T)
  SPX <- reqMktData(tws, Contract=twsIndex('SPX','CBOE'), snapshot = T)$lastPrice
  #VIX = markets_table[4,2]
  WMI = 0
  TICK <- reqMktData(tws, Contract=twsIndex('TICK-NYSE','NYSE'), snapshot = T)$lastPrice
  AD <- reqMktData(tws, Contract=twsIndex('AD-NYSE','NYSE'), snapshot = T)$lastPrice
  AD <- ifelse(is.na(AD),0,AD)
  TRIN <- reqMktData(tws, Contract=twsIndex('TRIN-NYSE','NYSE'), snapshot = T)$lastPrice
  
  
  
  output$SPX <- renderValueBox({
    autoInvalidate()
    SPX <- reqMktData(tws, Contract=twsIndex('SPX','CBOE'), snapshot = T)
    SPX = SPX$lastPrice
    infoBox(title = "SPX", value = SPX, color = "purple", fill = T)
  })
  
  output$VIX <- renderValueBox({
    autoInvalidate()
    infoBox(title = "VIX", value = VIX, color = ifelse(VIX<=25,"green","red"), fill = T)
  })
  
  output$VIX <- renderValueBox({
    autoInvalidate()
    infoBox(title = "TICK", value = TICK, color = ifelse(TICK<0,"green","red"), fill = T)
  })
  
  output$AD <- renderValueBox({
    autoInvalidate()
    infoBox(title = "AD", value = AD, color = ifelse(AD<0,"green","red"), fill = T)
  })
  
  output$TRIN <- renderValueBox({
    autoInvalidate()
    infoBox(title = "TRIN", value = TRIN, color = ifelse(TRIN<0,"green","red"), fill = T)
  })
  
  output$WMI <- renderValueBox({
    autoInvalidate()
    infoBox(title = "WMI", value = WMI, color = ifelse(WMI<0,"green","red"), fill = T)
  })
  
  
  #SPX Chart
  output$mainchart <- renderPlot({
      autoInvalidate()

    
    })

}

shinyApp(ui, server)