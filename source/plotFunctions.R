#Copyright (c) 2013 david hilton shanabrook. All rights reserved. 
#fix bounds to always within 2 sd
library(ggthemes)
getStrikes <- function(openInt, stock, strikes, lastQuote){
	strikes <- as.numeric(strikes) %/% 2
	currentStrikeRow <- which.min(abs(openInt$strike - lastQuote)) 
	lowerRow <- currentStrikeRow - strikes
	upperRow <- currentStrikeRow + strikes

	if (lowerRow < 1)
		lowerRow <- 1
	if (upperRow > nrow(openInt))
    	upperRow <- nrow(openInt)
	upper <- openInt[upperRow,]$strike
	lower <-  openInt[lowerRow,]$strike
#setting interal to the interval of the nearest strike.  Should be for the smallest interval?  or the strike with max oi?
	maxStrikeInd <- which.max(openInt$callOI)
	interval <- openInt[maxStrikeInd+1,]$strike - openInt[maxStrikeInd,]$strike
	if (is.na(interval))
		interval <- 1
	return(data.frame(lower, upper, interval, strikes))	
}
adjustStrikesForSmoothing <- function(strikes, smoothOn) {
	if (smoothOn)
		return(strikes*2)
	else
		return(strikes)
}
subsetOIforStrikes <- function(openInt, strikePar, smoothOn) {
	sp <- strikePar
	openInt <- subset(openInt, (strike >= sp$lower) & (strike <= sp$upper))
	#remove points less than interval
	if (smoothOn){
		evenRows <- openInt[c(F,T),]
		oddRows <- openInt[c(T,F),]
		evenSum <- sum(evenRows$callOI) + sum(evenRows$putOI)
		oddSum <- sum(oddRows$callOI) + sum(oddRows$putOI)
		if (evenSum>oddSum){
			openInt <- evenRows
		}  else {
			openInt <- oddRows
	}}
	return(openInt)
}

useStrikesSetSize <- function(sp){
	maxN <-100
	theN <- round((sp$upper - sp$lower)/sp$interval)
	if (theN >maxN) 
		theN <- maxN
	if (theN > 50)
		theSize <- 7
	else
		theSize <- 12
	xAxisPar <- c(theN, theSize)
	names(xAxisPar) <- c("theN", "theSize")
	return(xAxisPar)
	
}

cummPlots <- function(openInt, sp){
	xAxisPar <- useStrikesSetSize(sp)
	p <- ggplot(openInt, aes(x=strike))
	p <- p + scale_x_continuous(breaks = pretty(sp$lower:sp$upper, n = xAxisPar["theN"]))
	p <- p + theme(legend.title = element_blank())
	p <- p + theme(axis.text.x=element_text(angle=90,size=xAxisPar["theSize"]))
	#hide horizontal gridlines
	p <- p + theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())
	p <- p + theme(legend.justification=c(1,1), legend.position=c(1,1))
	return(p)
}
putCallPlots <- function(openInt, sp) {
		xAxisPar <- useStrikesSetSize(sp)
		p <- ggplot(openInt, aes(x=strike))
		p <- p + geom_area(aes(y = putOI,  fill = "1 put", colour = "1 put",   stat = "bin"), alpha = 0.5)
		p <- p + geom_area(aes(y = callOI, fill = "2 call",colour = "2 call",  stat = "bin"), alpha = 0.5)
		p <- p + geom_point(aes(y=callOI), size=1.5, alpha=.5, color="blue")
		p <- p + geom_point(aes(y=putOI),  size=1.5, alpha=.5, color="red")
	p <- p + scale_x_continuous(breaks = pretty(sp$lower:sp$upper, n = xAxisPar["theN"]))
	p <- p + theme(legend.title = element_blank())
	p <- p + theme(axis.text.x=element_text(angle=90,size=xAxisPar["theSize"]))
	#hide horizontal gridlines
	p <- p + theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())
	p <- p + theme(legend.justification=c(1,1), legend.position=c(1,1))
	return(p)
}
setupStrikePar <- function(openInt, stock, strikes, lastQuote, smoothOn){
	strikePar <- getStrikes(openInt, stock, strikes, lastQuote)
	strikePar$strikes <- adjustStrikesForSmoothing(strikePar$strikes, smoothOn)
	if (doDebug) cat("setupStrikePar, interval, strikes", strikePar$interval, strikePar$strikes, "\n")
	return(strikePar)
}
plotDensity <- function(openInt, strikePar) {
	p <- putCallPlots(openInt, strikePar)
	p <- p + ylab("open interest")
	return(p)
}
plotCumm <- function(openInt, strikePar) {
	diff <- "blue"
	p <- cummPlots(openInt, strikePar)
	#modifications
	p <- p + geom_area(aes(y = cumPuts, fill = "1 put", colour = "1 put",stat="bin"),alpha=0.5)
	p <- p + geom_area(aes(y = cumCalls, colour = "2 call", fill = "2 call", stat = "bin"), 
		alpha = 0.5)
	p <- p + ylab("Cummulative")
	return(p)
}
plotCummDiff <- function(openInt, strikePar) {
	diff <- "green"
	p <- cummPlots(openInt, strikePar)
	p <- p + geom_area(aes(y = cumDiff, fill = "put", colour = "diff", stat = "bin"), 
		alpha = 0.5)
	p <- p + ylab("Cummulative difference ")
	return(p)
}
plotVolume <- function(openInt, strikePar) {
	p <- cummPlots(openInt, strikePar)
	p <- p + geom_line(aes(y = callsVol, colour = "2 call"))
	p <- p + geom_line(aes(y = putsVol, colour = "1 put"))
	p <- p + ylab("Volumes")
	return(p)
}
plotDifference <- function(openInt, strikePar) {
	title <- ""
	callPutDiff <- "blue"
	p <- cummPlots(openInt, strikePar)
	p <- p + geom_line(aes(y = callOI - putOI, colour = "callPutDiff"))
	p <- p + ylab("difference in call/put open interest")
	p <- p + geom_hline(yintercept = 0)
	return(p)
}
plotError <- function(message = "blank") {
	df <- data.frame(x = 1:10, y = 1:10)
	p <- ggplot(df) + geom_line(aes(y = y, x = x))
	p <- p + labs(title = message) + theme(plot.title = element_text(size = 20, colour = "red"))
	return(p)
}
getPin <- function(openInt){
	minPutCallDiff <- min(abs(openInt$cumDiff))
	withOutMin <- openInt[abs(openInt$cumDiff)!=minPutCallDiff,]
	min2PutCallDiff <- min(abs(withOutMin$cumDiff))
	pin <- openInt[abs(openInt$cumDiff)==minPutCallDiff,"strike"]
	pin2 <- openInt[abs(openInt$cumDiff)==min2PutCallDiff,"strike"]
	pinText <- paste(min(pin,pin2),":",max(pin,pin2))
	return(pinText)
}
cleanUpChain <- function(openInt){
	openInt <- getRidofNA(openInt)
	openInt <- addCumm(openInt)
	openInt <- invertPutOrder(openInt)
	return(openInt)
}
truncChain  <- function(openInt, strikePar, smoothOn){
	openInt <- subsetOIforStrikes(openInt,strikePar, smoothOn)
	openInt <- addCumm(openInt)
	openInt <- invertPutOrder(openInt)
	return(openInt)	
}