#' @importFrom httr GET

.checkVersion <- function(url="https://reactome.org/ContentService/data/database/version") {
  r <- GET(url)
  vs <- content(r, "text")
  vs
}


