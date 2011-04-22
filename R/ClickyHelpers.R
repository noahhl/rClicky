# Provide user-friendly "helpers" to interface with Clicky API. Contains error checking, handling for ClickyQuery objects



parameters.required <- c("site_id", "sitekey", "type")
parameters.valid <- c("site_id", "sitekey", "base_url", "type", "output", "limit", "date", 
							"daily", "hourly", "item", "visitor-details", "time_offset", "segments",
							"ip_address", "href", "landing", "exit", "num_actions", "search", 
							"domain", "link", "browser", "os", "resolution", "country", "city",
							"language", "hostname", "org", "source", "shorturl", "custom",
							"session_id", "title", "action_type", "filter", 
							"json_callback", "json_var", "null_values", "app",
							"goal", "campaign", "split")


ClickyQuery <- function(configfile=NULL) {
# Creates a ClickyQuery as an interface to create and retrieve queries. This 
# provides a set of helpers in a clicky object framework to define the site to 
# query, the parameters to query, and to execute the specified query. Actual 
# API interaction is conducted by functions in Rclicky.R
#
# Params:
#		configfile = a configuration file containing a default set of parameters
# Returns:
#		A list of class "clicky" that provides the allowable set of functions
#
#
#			Usage:
#			my.site <- ClickyQuery()
#			my.site$SetSiteCredentials("siteid", "sitekey")
#			my.site$SetQueryParameter(c("param1", "param2"), c("value1", "value2"))
#			my.site$DeleteParameter(c("wrongparam1", "wrongparam2"))
#			print(my.site)
#			my.site.data <- my.site$GetData()
#
#
#
#

#
#
#  Functions exposed via ClickyObject
#
#

	SetSiteCredentials <- function(id=NULL, key=NULL) {
		#Set site credentials and test that they are valiod
		#	Params:
		#		id = site_id
		#		key = sitekey
		#	Returns: no return; updates config with credentials if they are valid. 
		#
		#	Raises errors if credentials are rejected or both a site_id and sitekey are not specified

		if(is.null(id) | is.null(key))
			stop("You must provide both a site_id and a sitekey.")

		if(!CheckSiteCredentials(id,key))
			stop("Your credentials were rejected.")
			
		config <<- CreateParams(data.frame(param=c("site_id", "sitekey"), value=c(id,key), stringsAsFactors=FALSE),config)
	}
	

	SetQueryParameter <- function(params=c(), values=c()) {
		#Adds a paramter and value to the current configuration, replacing duplicates with the newest value
		#	Params:
		#		params = a vector containing the names of parameters. These must be contained within parameters.allowed.
		#		values = a vector of equal length providing the values
		#	Returns: no return, updates config
		#
		#	Raises errors if parameter set is empty, params and values are inequal lengths, or parameters not allowed

		if(is.null(params) | is.null(values))
			stop("You didn't specify any parameters.")
		if(length(params) != length(values))
			stop("Your must specify an equal number of parameters and values")
		
		config <<- CreateParams(data.frame(param=params, value=values, stringsAsFactors=FALSE), config)
		
		ValidateParameters("interim")	
	}
	
	DeleteParameter <- function(param=NULL) {
		#Delete a specific paramter and any values assigned. Can be used to delete one or more parameters. 
		#	Params:
		#		param = a list of parameters to be deleted. If called without passing any parameters, will delete all parameters
		#
		#	Returns: no return, updates config

		if(is.null(param))
			config <<- subset(config, config$param == "")
		else {
			for (i in param) {
				config <<- subset(config, config$param != i)
			}
		}
	}
	
	
	GetData <- function() {
		# Get results of the query specified by config
		# Params: none
		# Returns: the results of the query. If format is XML or unspecified, will be an xmlTree; if JSON, a list; 
		# If PHP or CSV specified, will return a text string

		ValidateParameters("final")
	
		results <- GetResults(GenerateURL(config))
		
		if (length(grep("output=json", GenerateURL(config))))
			return(ParseJSON(results))
		else if(length(grep("output=json", GenerateURL(config))) | length(grep("output", GenerateURL(config))) == 0)
			return(ParseXML(results))
		else
			return(results)
	}
	
	


#
#
# Private functions, no exposed via ClickyQuery
#
#
		
	ValidateParameters <- function(check="final") {
		# Checks that parameters in config are all valid and that required parameters exist
		#	Params:
		#		check = type of check. If final, will check that parameters.required are all present.
		#					for any other value, just checks that parameters are allv alid
		#
		#	Returns: nothing. Raises errors on any missing or invalid paramters
		#
		if(check=="final") {
			for (i in parameters.required) {
				if(!(i %in% config$param))
					stop("You must supply a valid site_id, sitekey, and type")
			}
		}
		errors <- c()
		for (i in 1:length(config$param)){
			if(!(config$param[i] %in% parameters.valid)) {
				errors[length(errors)+1] <- paste("\n\'", config$param[i], "\' 	is not a valid parameter.")
			}
		} 
		if(length(errors) > 0)
			stop(errors)
	}


	CheckSiteCredentials <- function(id, key) {
		#Check if site credentials specified are valid.
		#	Params:
		#		id = site_id		key = sitekey
		#
		#	Returns: binary representing validity of keys
		#
		return(length(grep("Invalid", GetResults(GenerateURL(CreateParams(data.frame(param=c("site_id", "sitekey"), value=c(id, key)))))) == 0))
	}

	ShowQuery <- function() {
		#Prints config 
			if(length(config) >0) {
			  print(config)
		  } else
		    cat("Nothing set yet.")
	}

	ShowURL <- function() {
		#Prints url
		print(GenerateURL(config))
	}


#
#
# Load starting config, validate input file, and return list of functions 
#
#
	if(!is.null(configfile)){
		config <- ParseConfigFile(configfile)
	  ValidateParameters("load")
  }
	
	funcs <- list(	SetSiteCredentials = SetSiteCredentials, 
						SetQueryParameter = SetQueryParameter, 
						GetData = GetData,
						ShowQuery=ShowQuery,
						ShowURL = ShowURL,
						DeleteParameter=DeleteParameter)
	class(funcs) <- "clicky"
	return(funcs)
}




print.clicky <- function(o) {
#Print config of a clicky object o
	o$ShowQuery()
}