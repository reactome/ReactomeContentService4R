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
      #message("hello")
    },
    error = function(e) {
      message("Reactome is not working")
      print(e)
    }
  )
  #v <- httr::content(res, "text")
}


## Check the status of http response
.checkStatus <- function(res) {
  if (httr::status_code(res) != 200) {
    body <- jsonlite::fromJSON(content(res, "text"))
    # return error message
    if (is.na(body[["messages"]])) {
      stop(paste0(body[["code"]], "-", body[["reason"]], ", path:", 
                  gsub(".*?ContentService", "", body[["url"]]))) 
    } else {
      stop(paste0(body[["code"]], " - ", body[["messages"]]))
    }
  }
}


#' Spell check the given search term
#' @param query A query term
#' @return a vector of spell-check suggestions
#' @examples
#' spellCheck("R-HSA-12355")
#' spellCheck("oxidition")
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @rdname spellCheck
#' @export

spellCheck <- function(query) {
  path <- "search/spellcheck"
  res <- GET(file.path(getOption("base.address"), paste0(path, "?query=", query)))
  .checkStatus(res)
  fromJSON(content(res, "text"))
}



