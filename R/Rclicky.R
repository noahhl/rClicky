# Provide core interface and parsing functionality to Clicky analytics API. Does not contain 
# error checking or support for objects to interact with - for that, see ClickyHelpers.R

require(RCurl)
require(XML)
require(rjson)

ParseConfigFile <- function(file) {
# Load query and acocunt parameters 
#	Params: file = path to a file that contains any query and account parameters
#	Returns: a data frame with paramater names and values
	params <- data.frame(param=character(0), value=character(0), stringsAsFactors=FALSE)
	tryCatch(params<- read.table(file,sep=":", stringsAsFactors=FALSE), error=function(e) {print("File not found or empty.")})
	colnames(params) <- c("param", "value")
	return(params)
}

CreateParams <- function(params.specified=data.frame(param=character(0), value=character(0), stringsAsFactors=FALSE), params.default=ParseConfigFile()) {
	# Creates a set of parameters for query from params.specified and default from config file. params.specified will override params.default
	#	Params: 	params.specified = a data frame with parameter names and values
	#				params.default = a set of default parameters or existing parameters to be updated/appended to
	#	Returns: a data frame with paramater names and values
	full.parameters <- rbind(params.default, params.specified)
	full.parameters <- subset(full.parameters, !duplicated(full.parameters$param, fromLast=T))
	return(full.parameters)
}

GenerateURL <- function(parameters=CreateParams()) {
	# Generates a query URL from parameters specified
	#	Params:	a data.frame specifying the parameters for the query
	#	Returns: a formatted URL
	if(!("site_id" %in% parameters$param) | !("sitekey" %in% parameters$param) | !("type" %in% parameters$param))
		stop("You must specify a site_id, sitekey, and data type.")
	
	if("base_url" %in% parameters$param)
		base_url <- parameters$value[parameters$param=="base_url"]
	else
		base_url <- "http://api.getclicky.com/api/stats/4"
	url <- paste(base_url, "?", sep="")
	url <- paste(url, parameters$param[1], "=", parameters$value[1], sep="")
	for(i in 2:dim(parameters)[1]) {
		url <- paste(url, "&", parameters$param[i], "=", parameters$value[i], sep="")
	}
	return(gsub(" ", "", url))
}

GetResults <- function(url = GenerateURL()) {
#Retrieves results from clicky API
#	Params: a formatted URL containing credentials and query
#	Returns: a character array containing whatever format was specified (XML, JSON, CSV, or PHP)
	res <- getURL(url, .encoding="UTF-8")
	return(res)	
}

ParseXML <- function(results) {
# Convert results from API to an XML tree
#	Params: results = character string XML results from GetResults()
#	Returns: an XML tree
	parsed <- xmlTreeParse(results, useInternal=TRUE)
	return(parsed)
}

ParseJSON <- function(results) {
# Convert JSON results from API to a list tree
#	Params: results = character string JSON results from GetResults()
#	Returns: a list representing the results
	parsed <- fromJSON(results)
	return(parsed)
}