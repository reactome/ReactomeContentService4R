# helper functions

## The hook function called soon after package loading
.onLoad <- function(libname, pkgname) {
  options(base.address = "https://reactome.org/ContentService")
  
  #packageStartupMessage("Connecting...", appendLF=FALSE)
  
  ### test the connection or something else here? ###
  #welcome message
}



## Check Reactome's current version
.checkVersion <- function() {
  tryCatch(
    {
      res <- httr::GET(url=file.path(getOption("base.address"), "data/database/version"))
    },
    error = function(e) {
      message("Reactome is not available")
    }
  )
  #v <- httr::content(res, "text")
}


## Check the status of http response
.checkStatus <- function(res) {
  if (httr::status_code(res) != 200) {
    body <- jsonlite::fromJSON(httr::content(res, "text"))
    # return error message
    if (is.na(body[["messages"]])) {
      stop(paste0(body[["code"]], "-", body[["reason"]], ", path:", 
                  gsub(".*?ContentService", "", body[["url"]]))) 
    } else {
      stop(paste0(body[["code"]], " - ", body[["messages"]]))
    }
  }
}


## retrieve data
.retrieveData <- function(url, fromJSON=TRUE, ...) {
  res <- httr::GET(url)
  .checkStatus(res)
  data <- httr::content(res, ...)
  if (fromJSON) data <- jsonlite::fromJSON(data)
  data
}


## match species names & ids
## e.g. 
## .matchSpecies("cow", "taxId")
## .matchSpecies("HPV", c("taxId", "abbreviation"))
.matchSpecies <- function(species, output=c("displayName", "dbId", "taxId", "abbreviation")) {
  # arg species could be name, dbID, taxonID, or abbreviation
  
  # ensure correct input
  output <- match.arg(output, several.ok = TRUE)
  species <- as.character(species)
  all.species <- reactome4r::getSpecies() ### store in a local file may be better ###
  
  # to see it's a name or id or abbr.
  type <- NULL
  type <- colnames(all.species)[apply(all.species, 2, function(x) species %in% unlist(x))]
  if (is.null(type)) stop("Please input a species listed in Reactome, also can find more information using `getSpecies()`")

  # output
  type <- type[1] # in case type==c("displayName","name")
  if (type == "name") {
    species.row <- all.species[unlist(lapply(all.species$name, function(x) species %in% x)), ]
  } else {
    species.row <- all.species[all.species[[type]] == species, ] 
  }

  if (nrow(species.row) > 1) {
    warning("This species is not unique, please use IDs or full name instead")
    print(species.row[, c(1:5)])
  }
  species.row[, output]
}


## list people in Reactome with partly or full name
.listPeople <- function(name, ...) {
  path <- "data/people/name"
  # modify the name and write full URL
  url <- ifelse(grepl("\\s", name),
                file.path(getOption("base.address"), path, tolower(gsub("\\s", "%20", name)), "exact"),
                file.path(getOption("base.address"), path, tolower(name)))
  
  names.df <- .retrieveData(url, as="text")
  names.df[ ,c(1:5)]
}


#' Spell check the given search term
#' @param query A query term
#' @return a vector of spell-check suggestions
#' @examples
#' spellCheck("R-HSA-12355")
#' spellCheck("oxidition")
#' @rdname spellCheck
#' @export

spellCheck <- function(query) {
  path <- "search/spellcheck"
  url <- file.path(getOption("base.address"), paste0(path, "?query=", query))
  .retrieveData(url, as="text")
}



