#Copyright (c) 2013 david hilton shanabrook. All rights reserved.
library(shiny)
library(quantmod)
library(ggplot2)

source("source/yahooInput.R")
source("source/openIntFunctions.R")
source("source/plotFunctions.R")
source("source/getOptionChainPatch.R")

shinyServer(function(input, output, session) {
	pinByStrikes <- reactive({input$pinByStrikes})
	graphType <- reactive({input$graphType})
	stockText <- reactive({toupper(input$stock)})
	expText <- reactive({input$yymmdd})
	strikes <- reactive({input$strikes})
	smoothOn <- reactive({input$smoothOn})
   withProgress(message = 'waiting for data', value = 0, {
	incProgress(0.2, detail = "got quote")
	chain <- reactive({getYahooDataReformatted(stockText(), expText())})
	incProgress(0.4, detail = "got options")		
	})		
output$openIntPlot <- renderPlot({
	withProgress(message = 'waiting for plot', value = 0, {
		p <- ggplot(chain(), aes(x=strike))
	    + geom_area(aes(y = putOI,  fill = "1 put", colour = "1 put",   stat = "bin"), alpha = 0.5)
		+ geom_area(aes(y = callOI, fill = "2 call",colour = "2 call",  stat = "bin"), alpha = 0.5)
		+ geom_point(aes(y=callOI), size=1.5, alpha=.5, color="blue")
		+ geom_point(aes(y=putOI),  size=1.5, alpha=.5, color="red")
		#+ scale_x_continuous(breaks = pretty(sp$lower:sp$upper, n = theN))
	    + theme(legend.title = element_blank())
	    + theme(axis.text.x=element_text(angle=90,size=theSize))

	    + theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())

   	incProgress(0.9, detail = "outputting plot")
	print(p)
	})
	})
})
