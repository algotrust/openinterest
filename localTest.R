#local test 
library(shiny)
library(quantmod)
library(ggplot2)

#use command f3

setwd("~/ShinyApps/openinterest")
source("source/yahooInput.R")
source("source/openIntFunctions.R")
source("source/plotFunctions.R")
source("source/getOptionChainPatch.R")

doDebug <<- T
stock <- "AAPL"
yymmdd <- "150320"
strikes <- 16
smoothOn <- F
pinByStrikes <- F

lastQuote <- getQuote(stock)$Last
openInt <- getYahooDataReformatted(stock, yymmdd)
strikeParam <- setupStrikeParam(openInt, stock, strikes, lastQuote, smoothOn)
	openInt <- subsetOIforStrikes(openInt, strikeParam, smoothOn)
	openInt <- getRidofNA(openInt)
	openInt <- addCumm(openInt)
	openInt <- invertPutOrder(openInt)
getPin(openInt)
getPlot("prettyPlot",openInt,stock, strikes, lastQuote, smoothOn, pinByStrikes=)

plotDensity(openInt, stock, strikes,  lastQuote,smoothOn )
plotBW(openInt, stock, strikes, lastQuote, smoothOn)
plotVolume(openInt, stock, strikes,  lastQuote, smoothOn )
plotCummDiff(openInt, stock, strikes,  lastQuote, smoothOn )
plotCumm(openInt, stock, strikes,  lastQuote, smoothOn )
minCumDiff <- min(abs(openInt$cumDiff))
pin <- openInt[openInt$cumDiff==minCumDiff,"strike"]
#test update check
updatedYet <<- TRUE
oldOI <- 0
expText <- yymmdd
chain <- openInt

expirationDatesFuture <<- c(
                  #   "Oct 31" = "141031",
                     "Nov 07" = "141107",
                     "Nov 14" = "141114",
                     "Nov 22*" = "141122",
                     "Nov 28" = "141128",
                     "Dec 05" = "141205",
                     "Dec 12" = "141212",
                     "Dec 20*" = "141220",
                     "Dec 26" = "141226",
           
             		 "Jan 15* 2015"="150117",
                     "Jan 16* 2016"="160116"
                     )
                     
isitapple(stock, chain, expText)