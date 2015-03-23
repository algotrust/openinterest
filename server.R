#Copyright (c) 2013 david hilton shanabrook. All rights reserved.
library(shiny)
library(quantmod)
library(ggplot2)

#use command f3
doDebug <<- F
doP <<- T
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
	graphType <- reactive({input$graphType})
	stockText <- reactive({toupper(input$stock)})
	expText <- reactive({input$yymmdd})
	strikes <- reactive({input$strikes})
	smoothOn <- reactive({input$smoothOn})
    progress <- shiny::Progress$new()
    progress$set(message = "Computing data", value = 0)
	lastQuote <- reactive({getQuote(stockText())$Last})
	chain <- reactive({getYahooDataReformatted(stockText(), expText())})
	output$caption <- renderText({paste(stockText(), " $", lastQuote(), " Expiration ", expText())})
	output$subCaption <- reactive({wasUpdatedToday(stockText(),chain(),expText())})
	cleanChain <- reactive({cleanUpChain(chain())})
	strikePar <- reactive({setupStrikePar(chain(), stock(), strikes(), lastQuote(), smoothOn())})
	subChain <- reactive({truncChain(chain(), strikePar(), smoothOn())})
	output$pinCaption <- renderText({paste("Expiration Pin Range:", getPin(subChain()), "  (see notes)")})
    progress$set(message = "generate plot", value = .5)
output$openIntPlot <- renderPlot({
    if (!is.null(chain())) 
    	p <- switch(graphType(),
		"OI"=plotOpen(subChain(),strikePar()),
		"OIvol"= plotVolume(subChain(),strikePar()),
		"OIDiff"=plotDifference(subChain(),strikePar()),
		"cummulative"=plotCumm(subChain(),strikePar()),
		"cummDiff"=plotCummDiff(subChain(),strikePar()),
		"prettyPlot"=plotDensity(subChain(),strikePar()))
	 else
		plotError("No Data.  Either wrong date for this stock, or no prior data")	
		progress$set(message = "display plot", value = .8)
	p
	})
})
