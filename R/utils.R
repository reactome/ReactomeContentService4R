## helper functions

#' @importFrom httr GET
.checkVersion <- function() {
  r <- GET(url="https://reactome.org/ContentService/data/database/version")
  content(r, "text")
}

