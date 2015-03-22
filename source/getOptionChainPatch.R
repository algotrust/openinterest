#install.packages("rjson")

getOptionChain.yahoo.patch <- function(Symbols, Exp, ...)
{
    library("XML")
    library("rjson")

    millisToDate <- function(x)
    {
        return (as.Date(x / 86400000, origin = "1970-01-01"))
    }

    dateToMillis <- function(x)
    {
        as.numeric(x) * 86400000  /1000
    }

    parse.expiry <- function(x) {
        if(is.null(x))
          return(NULL)

        if(is.character(x))
        {
            x <- as.Date(x)
        }

        if(inherits(x, "Date") || inherits(x, "POSIXt"))
          return(dateToMillis(x))

         return(NULL)
    }

    getOptionChainJson <- function(sym, Exp)
    {
      if(missing(Exp))
        {
            url <- paste("http://finance.yahoo.com/q/op?s",sym,sep="=")
            opt <- readLines(url)
        }
      else
        {   
            url <- paste("http://finance.yahoo.com/q/op?s=",sym,"&date=",parse.expiry(Exp),sep="")
            opt <- readLines(url)
        }

        opt <- opt[grep("percentChangeRaw", opt)]
        opt <- unlist(strsplit(opt, "<script>"))
        json <- opt[3]
        json <- gsub("<script>", "", json)
        json <- gsub("</script>", "", json)
        json <- gsub(";", "", json)
        json <- unlist(strsplit(json, "="))[4]

        j <- fromJSON(json)
        price <- j$models$applet_model$data$optionData$quote$regularMarketPrice
        calls <- j$models$applet_model$data$optionData$options$calls
        puts <- j$models$applet_model$data$optionData$options$puts
        return (list(calls=chainToDf(calls), puts=chainToDf(puts), price = price, sym = sym))
    }

    chainToDf <- function(theList)
    {
        x <- do.call(rbind.data.frame, theList)

        rownames(x) <- x$contractSymbol
        y <- x[,c("strike", "bid", "ask", "lastPrice", "volume", "openInterest")]
        theNames <- c("Strike", "Bid", "Ask", "Last", "Vol", "OI")
        names(y) <- theNames
        for(i in theNames)
        {
            y[,i] <- as.numeric(as.character(y[,i]))
        }

        #y$contractSymbol <- as.character(x$contractSymbol)
        #y$expiration <- millisToDate(as.numeric(as.character(x$expiration)) * 1000)

        return(y)
    }

    getOptionChainJson(Symbols, Exp)
}
assignInNamespace("getOptionChain.yahoo", getOptionChain.yahoo.patch, "quantmod")