# helper functions

## hook for namespace
.onAttach <- function(libname, pkgname) {
  options(base.address = "https://reactome.org/ContentService")
  # welcome message
  packageStartupMessage("Connecting...", appendLF=FALSE)
  version <- .checkVersion()
  packageStartupMessage("welcome to Reactome v", version, "!")
}


## Check Reactome's current version
.checkVersion <- function() {
  url <- file.path(getOption("base.address"), "data/database/version")
  version <- .retrieveData(url, fromJSON = FALSE, as = "text")
  version
}


## Check the status of http response
.checkStatus <- function(res, customMsg=NULL) {
  if (httr::status_code(res) != 200) {
    body <- jsonlite::fromJSON(httr::content(res, "text"))
    # print custom message
    if (!is.null(customMsg)) message(customMsg)
    
    # return error message
    if (is.na(body[["messages"]])) {
      warning("HTTP ",body[["code"]], " - ", body[["reason"]],", path: ", 
               gsub(".*?ContentService", "", body[["url"]]), call.=FALSE) 
    } else {
      warning("HTTP ", body[["code"]], " - ", body[["messages"]], call.=FALSE)
    }
    # no pass
    FALSE
  } else{
    # pass
    TRUE
  }
}


## retrieve data
.retrieveData <- function(url, customMsg=NULL, fromJSON=TRUE, ...) {
  tryCatch(
    expr = {
      res <- httr::GET(url)
    },
    error = function(e) {
      # catch error of GET
      if (!"ReactomeContentService4R" %in% (.packages())) {
        message("Reactome is not responding. Remember to attach the package.") 
      }
      message(e)
    }
  )
  
  # return the data if .checkStatus() passed
  status <- .checkStatus(res, customMsg=customMsg)
  if (status) {
    data <- httr::content(res, ...)
    if (fromJSON) data <- jsonlite::fromJSON(data)
    data 
  } else {
    # return NULL if not getting correct response
    NULL
  }
}


## match species names & ids
## e.g. 
## .matchSpecies("cow", "taxId")
## .matchSpecies("HPV", c("displayName", "taxId"))
.matchSpecies <- function(species, output=c("displayName", "dbId", "taxId", "abbreviation")) {
  # arg species could be name, dbID, taxonID, or abbreviation
  
  # ensure correct input
  output <- match.arg(output, several.ok = TRUE)
  species <- as.character(species)
  all.species <- getSpecies() ### store in a local file may be better ###
  
  # no need to use schemaClass & className; left with "dbId","displayName","name","taxId","abbreviation"
  all.species <- all.species[ ,which(!colnames(all.species) %in% c("schemaClass", "className"))]
  
  # to see what data type this species arg is by checking which column it belongs to
  species.data.type <- colnames(all.species)[apply(all.species, 2, function(col) species %in% unlist(col))]
  if (length(species.data.type) == 0) {
    stop(species, " not listed in Reactome", call.=FALSE)
  }
  # output
  species.data.type <- species.data.type[1] # in case type==c("displayName","name")
  if (species.data.type == "name") {
    species.row <- all.species[unlist(lapply(all.species$name, function(x) species %in% x)), ]
  } else {
    species.row <- all.species[all.species[[species.data.type]] == species, ] 
  }
  
  if (nrow(species.row) > 1) {
    warning("This species is not unique, please use IDs or full name instead")
    return(species.row)
  } else {
    return(species.row[ ,output])
  }
}


## list people in Reactome with partly or full name
.listPeople <- function(name) {
  path <- "data/people/name"
  # modify the name and write full URL
  url <- ifelse(grepl("\\s", name),
                file.path(getOption("base.address"), path, tolower(gsub("\\s", "%20", name)), "exact"),
                file.path(getOption("base.address"), path, tolower(name)))
  
  names.df <- .retrieveData(url, as="text")
  # no need to present schemaClass & className, left with dbId,displayName,firstname,initial,orcidId
  names.df[ ,which(!colnames(names.df) %in% c("schemaClass", "className"))]
}


#' Spell-check suggestions for a given query
#' @param query a search term
#' @examples
#' spellCheck("intelukin")
#' @return spell-check suggestions for a given search term
#' @export

spellCheck <- function(query) {
  path <- "search/spellcheck"
  url <- file.path(getOption("base.address"), paste0(path, "?query=", gsub("\\s", "%20", query)))
  terms <- .retrieveData(url, as="text")
  
  # if the term is incorrect
  if (length(terms) != 0) {
    check.msg <- paste0("Did you mean ", paste(sQuote(terms), collapse = ", "), "?")
    return(check.msg)
  }
}

