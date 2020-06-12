# helper functions

## The hook function called soon after package loading
.onLoad <- function(libname, pkgname) {
  options(base.address = "https://reactome.org/ContentService")
  
  ### test if any Internal Server Error or something else here? ###
}



## Check Reactome's current version
.checkVersion <- function() {
  r <- httr::GET(url=file.path(getOption("base.address"), "data/database/version"))
  version <- httr::content(r, "text")
  print(paste0("Version ", version))
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



