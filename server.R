#Copyright (c) 2013 david hilton shanabrook. All rights reserved.
library(shiny)
library(quantmod)
library(ggplot2)

#use command f3
doDebug <<- F
doProgress <<- F
source("source/yahooInput.R")
source("source/openIntFunctions.R")
source("source/plotFunctions.R")
source("source/getOptionChainPatch.R")

wasUpdatedToday <- function(stock,chain,expText) {
	correctStockExpiration <-(stock=="AAPL")&(expText==expirations[[1]])
	if (correctStockExpiration){
		currentOI <- sum(chain$callOI+chain$putOI)
		storedOI <- read.csv("storedOI.txt")
		today <- format.Date(Sys.time(), "%b %d")
		time <- format.Date(Sys.time())
		if (currentOI != storedOI$x){
			write.csv(currentOI, "storedOI.txt")
			write.csv(today, "storedDay.txt")
			write.csv(time, "time.txt")
			return("Open interest updated")
			}
		storedDay <- read.csv("storedDay.txt")
		#if (storedDay$x == today)
		#	return("OI updated today")
		#else
		#	return( "OI not updated today")
		phrase <- paste("OI updated ", storedDay$x )
		return(phrase)
		}
	else
		return("")
	}

shinyServer(function(input, output, session) {
	pinByStrikes <- reactive({input$pinByStrikes})
	graphType <- reactive({input$graphType})
	stockText <- reactive({toupper(input$stock)})
	expText <- reactive({input$yymmdd})
	strikes <- reactive({input$strikes})
	smoothOn <- reactive({input$smoothOn})
# withProgress(message = 'waiting for data', value = 0, {
	lastQuote <- reactive({getQuote(stockText())$Last})
	incProgress(0.2, detail = "got quote")
	chain <- reactive({getYahooDataReformatted(stockText(), expText())})
#	incProgress(0.4, detail = "got options")
	
	output$caption <- renderText({paste(stockText(), " $", lastQuote(), " Expiration ", expText())})
	output$subCaption <- reactive({wasUpdatedToday(stockText(),chain(),expText())})
	
	if (!is.null(chain)) 
	reactive({cat("good option chain ", stockText(), expText(), "\n")})
#	incProgress(0.8, detail = "ready to plot")		
#	})		
output$openIntPlot <- renderPlot({
	#withProgress(message = 'waiting for plot', value = 0, {
    if (!is.null(chain()))
    
    	p <- getPlot(graphType(),chain(), stockText(), strikes(), lastQuote(), smoothOn(), pinByStrikes())
	else
		plotError("No Data.  Either wrong date for this stock, or no prior data")	
#	if (doProgress)		incProgress(0.9, detail = "outputting plot")
#	browser()
	print(p)
#	})
	})
})
