#Copyright (c) 2013 david hilton shanabrook. All rights reserved. 
	
removeOld <- function(expirations){
	today <- format.Date(Sys.time(),"%y%m%d")
	notOld <- expirations >= today
	return(expirations[notOld])
}			
expirations <<- c(
                     "Oct 31" = "141031",
                     "Nov 07" = "141107",
                     "Nov 14" = "141114",
                     "Nov 22*" = "141122",
                     "Nov 28" = "141128",
                    "Dec 05" = "141205",
                     "Dec 12" = "141212",
                     "Dec 20*" = "141220",
                     "Dec 26" = "141226",
           			"Jan 2"="150102",
           			"Jan 9"="150109",
           			"Jan 17*"="150117",
           			"Jan 23"="150123",
           			"Jan 30"="150130",
           			"Feb 6"="150206",
           			"Feb 13"="150213",
           			"Feb* 20"="150220",
           			"Feb 27"="150227",
           			"Mar 6"="150306",
           			"Mar 13"="150313",
           			"Mar* 20"="150320",
           			"Mar 27"="150327",
           			"Apr* 17"="150417",
           			"Jan 15* 2016"="160115",
                 	  "Jan 20* 2017"="170120"
                     )
expirations <<- removeOld(expirations)
shinyUI(

  pageWithSidebar(
    headerPanel("Option Open Interest"),
     sidebarPanel(
      selectInput("graphType", "Type:",
                  list(
                    "Open Interest" = "prettyPlot",
                    "Volume" = "OIvol",
                   "Call-Put OI difference" = "OIDiff",
                    "Cummulative OI"="cummulative",
                    "Cummulative Diff"="cummDiff")),
                    
       checkboxInput("smoothOn",
      			label = strong("Smooth strike interval?"),
      			value = F),

      textInput("stock","Stock symbol:", value="AAPL"),
      
      selectInput("yymmdd", "Expiration:", choices = expirations),
      
      selectInput("strikes", "Number of strikes",
                  list(
                    "default (16)" = 4,6,8,12,16,20,24,28,32,40,48,64,"all"=2000) , selected=16),  
      
      helpText("Wait patiently.  Could take up to 10 sec to get data from Yahoo.****Expiration Pin based on cummulative OI."),
      
      submitButton("Plot Now")

    ),
  mainPanel(
    h3(textOutput("caption")), 
    h5(textOutput("subCaption")),
    h5(textOutput("pinCaption")),
  #  h5("", a("Blog for more info", href="http://bravo0123.tumblr.com")),
    plotOutput("openIntPlot")
  )))
