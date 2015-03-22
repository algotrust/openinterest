#Copyright (c) 2013 david hilton shanabrook. All rights reserved.
doNotCheckYahoo <- TRUE
#gets open interest data for the particular stock, for the particular yymmdd.  and get it in the right form, call it openInt

putInRightForm <- function(options) {
	if (F) 
		cat("putInRightForm\n")
	options <- cbind(rownames(options), options)
	names(options)[1] <- "option"
	rownames(options) <- NULL
	return(options)
}

truncateOptionCode <- function(optionCode, options) {
	if (F) 
		cat(optionCode,"truncateOptionCode\n")
	pattern <- paste(optionCode, ".*", sep = "")
	options$option <- gsub(pattern, optionCode, options$option)
	options <- subset(options, options$option == optionCode)
	return(options)
}

formatStockyymmdd <- function(stock, yymmdd) {

	return
}

saveObjectInFile <- function(object, stock, yymmdd) {
	if (F) 
		cat("saveObjectInFile")
	today <- format(Sys.Date(), "%y%m%d")

	objectName <- paste(stock, "x", yymmdd, "on", today, sep = "")
	fname = paste("./data/", objectName, ".RData", sep = "")

	if (nrow(object) != 0) {
		saveRDS(object, file = fname)
		#   close(Fname)
		if (F) 
			cat(fname, "\n")
	}}

putInDateFormat <- function(yymmdd) {
	#print optionDate in 2013-11-16 format
	if (F) 
		cat("putInDateFormat ", yymmdd)
	optionDate <- paste("20", substring(yymmdd, 1, 2), "-", substring(yymmdd, 3, 4), "-", substring(yymmdd, 5, 6), sep = "")
	optionDate <- as.Date(optionDate)
	if (F) 
	  cat(" to ", optionDate, "\n")
	return(optionDate)
}

parseOptionList <- function(stock, yymmdd, optionList){
		#	options <- data.frame(optionList[1])
		calls <- putInRightForm(data.frame(optionList[1]))
		puts <- putInRightForm(data.frame(optionList[2]))
		calls2 <- subset(calls, select = c(option, calls.Strike, calls.OI, calls.Vol))
		puts2 <- subset(puts, select = c(option, puts.Strike, puts.OI, puts.Vol))

		names(calls2) <- c("option", "strike", "callOI", "callsVol")
		names(puts2) <- c("option", "strike", "putOI", "putsVol")

		optionCode <- paste(stock, yymmdd, sep = "")
		calls3 <- truncateOptionCode(optionCode, calls2)
		puts3 <- truncateOptionCode(optionCode, puts2)
		
		if ((nrow(calls3)>0) & (nrow(puts3)>0)){
			openInt <- merge(calls3, puts3, by = "strike", all = F)
			openInt <- subset(openInt, select = -c(option.x, option.y))
			}
		else
			openInt <- NULL
	return(openInt)
}

getYahooDataReformatted <- function(sym, yymmdd){
		Exp <- putInDateFormat(yymmdd)
		optionList <- getOptionChain(sym, Exp)
		if (!is.null(optionList)) {
			openInt <- parseOptionList(sym, yymmdd, optionList)
			return(openInt)
		}
}
getYahooDataReformattedOld <- function(stock, yymmdd) {
	optionDate <- putInDateFormat(yymmdd)
	if (F) cat("optionDate, yymmdd  ", optionDate, yymmdd, "\n")

	optionList <- tryCatch(getOptionChain(stock, optionDate), error = function(e) optionList = NULL)

	if (!is.null(optionList)) {
		openInt <- parseOptionList(optionList)
		return(openInt)
		}
}
