#Copyright (c) 2013 david hilton shanabrook. All rights reserved. 

getPrevWeeklyExp <- function(stock, yymmdd) {
  #the previous weekly expiration data.  
	if (doDebug) cat("getPrevWeeklyExp", stock, yymmdd)
	expDates <- c("131115", "131122", "131129", 
		"131206", "131221", "140118", "140719", "150117")
#	previous <- which(expDates == yymmdd) - 1
  previous <- 1
	previous <- as.character(expDates[previous])
  if (doDebug) cat("  previous:", previous, "\n")
	return(previous)
}

compareOpenInt <- function(OI, OIPast){
	if (doDebug) cat("difference \n")
	
	OICombined <- merge(OI, OIPast, by="strike")
	OICombined$callOI <- OICombined$callOI.x - OICombined$callOI.y
	OICombined$putOI <- OICombined$putOI.x - OICombined$putOI.y
	return(OICombined)
}
previousWeekDifference <- function(openInt, stock, yymmdd) {
  prevExp <- getPrevWeeklyExp(stock, yymmdd)
	if (doDebug) cat("previousWeekDifference", stock, yymmdd, prevExp,"\n")

	objectName <- paste(stock, "x", yymmdd, "on", prevExp, sep = "")
	fname = paste("./data/", objectName, ".RData", sep = "")
	openIntPrevious <- readRDS(fname)

	openInt <- merge(openInt, openIntPrevious, by = "strike")

	openInt$callOI <- openInt$callOI.x - openInt$callOI.y
	openInt$putOI <- openInt$putOI.x - openInt$putOI.y

	return(openInt)
}

appendRData <- function(x, file) {
  if (doDebug) cat("appendRData\n")
	tryCatch(old.objects <- load(file, new.env()), finally = old.objects <- NULL)

	save(list = c(old.objects, deparse(substitute(x))), file = file)
}

getRidofNA <- function(openInt) {
  if (doDebug) cat("getRidofNA\n")
	openInt[is.na(openInt$callOI), 1] <- 0
	openInt[is.na(openInt$putOI), 3] <- 0
	return(openInt)

}

addCumm <- function(openInt) {
  if (doDebug) cat("addCumm\n")
	openInt$cumCalls <- cumsum(openInt$callOI)
	openInt$cumPuts <- cumsumfromright(openInt$putOI)
	return(openInt)
}

invertPutOrder <- function(openInt) {
  if (doDebug) cat("invertPutOrder\n")
	
	openInt[nrow(openInt):1, ]$cumPuts <- openInt$cumPuts
	openInt$cumDiff <- openInt$cumCalls - openInt$cumPuts
	openInt$diff <- openInt$callOI - openInt$putOI
	openInt$sum <- openInt$callOI + openInt$putOI
	return(openInt)
}

mainWithFunctions <- function(stock, yymmdd) {
if (doDebug) cat("mainWithFunctions\n")

	openInt <- getYahooDataReformatted(stock, yymmdd)
	if (!is.null(openInt)) {
		openInt <- getRidofNA(openInt)
		openInt <- addCumm(openInt)
		openInt <- invertPutOrder(openInt)

		return(openInt)
	} else {
    if (doDebug) cat("main openInt<-0\n")
    return(0)
	}
}

cumsumfromright <- function(x) cumsum(rev(x))
