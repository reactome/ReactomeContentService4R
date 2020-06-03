#' @importFrom httr GET

.checkVersion <- function(obj=NULL) {
  r <- GET('https://reactome.org/ContentService/data/database/version')
  vs <- content(r, "text")
  vs
}

